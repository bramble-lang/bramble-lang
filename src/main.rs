#![allow(dead_code)]
#![feature(box_syntax, box_patterns)]

mod ast;
mod diagnostics;
mod lexer;
mod llvm;
mod parser;
mod semantics;

use std::{
    path::{Path, PathBuf},
    process::exit,
};

use ast::Type;
use clap::{App, Arg};
use diagnostics::config::TracingConfig;
use inkwell::context::Context;
use lexer::tokens::Token;
use semantics::type_resolver::*;

use crate::ast::Module;

const BRAID_FILE_EXT: &str = "br";

fn main() {
    let config = configure_cli().get_matches();

    let input = config
        .value_of("input")
        .expect("Expected an input source file to compile");
    let src_path = Path::new(input);
    let src_input = read_src_files(&src_path);

    let trace_lexer = TracingConfig::parse(config.value_of("trace-lexer"));
    let token_sets = tokenize_src_files(src_input, trace_lexer);

    let trace_parser = TracingConfig::parse(config.value_of("trace-parser"));
    parser::parser::set_tracing(trace_parser);

    let asts = parse_all(token_sets);

    // Type Check
    let trace_semantic_node = TracingConfig::parse(config.value_of("trace-semantic-node"));
    let trace_type_resolver = TracingConfig::parse(config.value_of("trace-type-resolver"));
    let trace_path = TracingConfig::parse(config.value_of("trace-path"));
    let imported = configure_imported_functions();

    let semantic_ast = match resolve_types_with_imports(
        &asts,
        &imported,
        trace_semantic_node,
        trace_type_resolver,
        trace_path,
    ) {
        Ok(ast) => ast,
        Err(msg) => {
            println!("Error: {}", msg);
            std::process::exit(ERR_TYPE_CHECK);
        }
    };

    // Configure the compiler
    let output_target = config.value_of("output").unwrap_or("./target/output.asm");

    if config.is_present("llvm") {
        let context = Context::create();
        let mut llvm = llvm::IrGen::new(&context, "test", &imported);
        match llvm.ingest(&semantic_ast) {
            Ok(()) => (),
            Err(msg) => {
                println!("LLVM IR translation failed: {}", msg);
                std::process::exit(ERR_LLVM_IR_ERROR);
            }
        }

        if config.is_present("emit") {
            llvm.print(Path::new("./target/output.ll"));
        }

        llvm.emit_object_code(Path::new(output_target)).unwrap();
    } else {
    }
}

fn find_or_create_module<'a>(
    module: &'a mut Module<u32>,
    path: &[String],
) -> Option<&'a mut Module<u32>> {
    match path.split_first() {
        Some((head, rest)) => {
            if module.get_module(head).is_none() {
                let sub = Module::new(head, 0);
                module.add_module(sub);
            }

            let sub = module
                .get_module_mut(head)
                .expect("A module with this name was just created and ought to be found");

            if rest.len() == 0 {
                Some(sub)
            } else {
                find_or_create_module(sub, rest)
            }
        }
        None => None,
    }
}

struct CompilationUnit<T> {
    path: Vec<String>,
    data: T,
}

/// Given the location of source file(s) this function will read the file
/// or files and return a list of the text of all source files. For each
/// file there will also be the associated path to the file (relative to
/// `src_path`).
///
/// If `src_path` is a directory, this will recursively read every file in
/// that directory and its subdirectories.  If it is a file, it will read
/// only that file.
fn read_src_files(src_path: &Path) -> Vec<CompilationUnit<String>> {
    let files =
        get_files(&src_path, BRAID_FILE_EXT).expect(&format!("Could not open: {:?}", src_path));
    let mut texts: Vec<CompilationUnit<String>> = vec![];
    for file in files {
        let p = file_path_to_module_path(&file, &src_path);

        let text = std::fs::read_to_string(&file)
            .expect(&format!("Failed to read input file: {:?}", file));
        texts.push(CompilationUnit {
            path: p,
            data: text,
        });
    }

    texts
}

fn parse_all(token_sets: Vec<CompilationUnit<Vec<Token>>>) -> Module<u32> {
    let mut root = Module::new("root", 0);
    for src_tokens in token_sets {
        match parse_src_tokens(src_tokens) {
            Ok(ast) => append_module(&mut root, ast),
            Err(msg) => {
                println!("{}", msg);
                exit(1)
            }
        }
    }
    root
}

fn tokenize_src_files(
    src_input: Vec<CompilationUnit<String>>,
    trace_lexer: TracingConfig,
) -> Vec<CompilationUnit<Vec<Token>>> {
    let mut token_sets = vec![];
    for src in src_input {
        let src_tokens = tokenize_src_file(src, trace_lexer);
        token_sets.push(src_tokens);
    }
    token_sets
}

fn tokenize_src_file(
    src: CompilationUnit<String>,
    trace_lexer: TracingConfig,
) -> CompilationUnit<Vec<Token>> {
    let mut lexer = crate::lexer::lexer::Lexer::new(&src.data);
    lexer.set_tracing(trace_lexer);
    let tokens = lexer.tokenize();
    let tokens: Vec<Token> = tokens
        .into_iter()
        .filter(|t| match t {
            Ok(_) => true,
            Err(msg) => {
                println!("{}", msg);
                false
            }
        })
        .map(|t| t.unwrap())
        .collect();
    CompilationUnit {
        path: src.path,
        data: tokens,
    }
}

/// Takes CompilationUnit which has been tokenized and parses the tokens into
/// an AST.
///
/// The last element of the compilation unit's path (the name of the module derived
/// from the source file name).  Will be removed from the path vector, because it becomes
/// part of the data field (when a module is created with the same name that becomes the
/// parent of all items within the source file).
fn parse_src_tokens(
    src_tokens: CompilationUnit<Vec<Token>>,
) -> Result<CompilationUnit<Module<u32>>, String> {
    if let Some((name, parent_path)) = src_tokens.path.split_last() {
        match parser::parser::parse(name, &src_tokens.data) {
            Ok(Some(ast)) => Ok(CompilationUnit {
                path: parent_path.to_owned(),
                data: ast,
            }),
            Ok(None) => Err("Critical: no AST was generated by the parser".into()),
            Err(msg) => Err(format!("Error: {}", msg)),
        }
    } else {
        Err("Invalid compilation unit path: was empty".into())
    }
}

fn append_module(root: &mut Module<u32>, src_ast: CompilationUnit<Module<u32>>) {
    let parent = if src_ast.path.len() == 0 {
        root
    } else {
        find_or_create_module(root, &src_ast.path).unwrap()
    };
    parent.add_module(src_ast.data);
}

fn get_files(path: &Path, ext: &str) -> Result<Vec<PathBuf>, std::io::Error> {
    let mut files = vec![];
    match path.extension() {
        None => {
            let dir = std::fs::read_dir(path)?;
            for f in dir {
                let f = f?;
                let fty = f.file_type()?;
                if fty.is_file() {
                    match f.path().extension() {
                        Some(ex) if ex.to_ascii_lowercase() == ext => {
                            files.push(f.path());
                        }
                        _ => (),
                    }
                } else if fty.is_dir() {
                    let mut sub_files = get_files(&f.path(), ext)?;
                    files.append(&mut sub_files);
                }
            }
        }
        Some(ex) if ex.to_ascii_lowercase() == "br" => {
            files.push(path.to_path_buf());
        }
        Some(ex) => {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                format!(
                    "Is not a Braid language file, expected extension {} but got {}",
                    BRAID_FILE_EXT,
                    ex.to_str().unwrap()
                ),
            ));
        }
    }
    Ok(files)
}

fn file_path_to_module_path(file: &PathBuf, src_path: &Path) -> Vec<String> {
    let fpath = file.as_path();
    let base = if src_path.is_dir() {
        src_path
    } else {
        src_path
            .parent()
            .expect("Given a file which is also the root of the directory structure.")
    };

    let rel_path = fpath.strip_prefix(&base).unwrap();

    let mut p: Vec<String> = rel_path
        .iter()
        .map(|e| e.to_str().unwrap().into())
        .collect();

    truncate_extension(&mut p, ".br");
    p
}

fn truncate_extension(path: &mut Vec<String>, ext: &str) {
    match path.last_mut() {
        Some(l) if l.ends_with(ext) => l.truncate(l.len() - ext.len()),
        _ => (),
    }
}

// Exit Codes for different types of errors
const ERR_TYPE_CHECK: i32 = 1;
const ERR_NO_AST: i32 = 2;
const ERR_PARSER_ERROR: i32 = 3;
const ERR_LLVM_IR_ERROR: i32 = 4;

fn configure_cli() -> clap::App<'static, 'static> {
    let app = App::new("Braid Compiler")
        .version("0.1.0")
        .author("Erich Ess")
        .about("Compiles Braid language files into x86 assembly for use by the NASM assembler")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .takes_value(true)
                .required(true)
                .help("Source code file to compile"),
        )
        .arg(
            Arg::with_name("output")
                .short("o")
                .long("output")
                .takes_value(true)
                .required(true)
                .help("Name the output file that the assembly will be written to"),
        )
        .arg(
            Arg::with_name("llvm")
                .long("llvm")
                .help("When set, then compiler will emit LLVM IR rather than x86 IR")
        )
        .arg(
            Arg::with_name("emit")
                .long("emit")
                .possible_values(&["llvm-ir"])
                .takes_value(true)
                .help("When set, this will output different types of IR (LLVM, assembly, etc.)")
        )
        .arg(
            Arg::with_name("platform")
                .short("p")
                .long("platform")
                .possible_values(&["linux", "machos"])
                .takes_value(true)
                .required(true)
                .help("The target Operation System that this will be compiled for: Linux or Mac (Mac is still unreliable and being worked on)"),
        )
        .arg(
            Arg::with_name("trace-parser")
                .long("trace-parser")
                .takes_value(true)
                .help("Prints out a trace of all the steps the parser follows as it converts the token vector into an AST.  The current token is printed next to the step.
                This is for debugging the parser when adding new syntactical elements.")
        )
        .arg(
            Arg::with_name("trace-reg-assigner")
                .long("trace-reg-assigner")
                .takes_value(true)
                .help("Prints out a trace of all the nodes in the AST and their register assignment annotation data.")
        )
        .arg(
            Arg::with_name("trace-lexer")
                .long("trace-lexer")
                .takes_value(true)
                .help("Prints out a trace of all the steps the lexer follows as it converts the token vector into an AST.  The current token is printed next to the step.
                This is for debugging the lexer when adding new tokens.")
        )
        .arg(
            Arg::with_name("trace-semantic-node")
                .long("trace-symbol-table")
                .takes_value(true)
                .help("Traces the transformation from Parser AST to Semantic AST")
        )
        .arg(
            Arg::with_name("trace-type-resolver")
                .long("trace-type-resolver")
                .takes_value(true)
                .help("Traces the type resolution unit")
        )
        .arg(
            Arg::with_name("trace-path")
                .long("trace-path")
                .takes_value(true)
                .help("Prints out the current module path at the current line of code.")
        );
    app
}

fn configure_imported_functions() -> Vec<(crate::ast::Path, Vec<Type>, Type)> {
    vec![
        (
            vec!["root", "std", "io", "write"].into(),
            vec![Type::StringLiteral],
            Type::Unit,
        ),
        (
            vec!["root", "std", "io", "readi64"].into(),
            vec![],
            Type::I64,
        ),
        (
            vec!["root", "std", "io", "writeu8"].into(),
            vec![Type::U8],
            Type::Unit,
        ),
        (
            vec!["root", "std", "io", "writeu8ln"].into(),
            vec![Type::U8],
            Type::Unit,
        ),
        (
            vec!["root", "std", "io", "writeu16"].into(),
            vec![Type::U16],
            Type::Unit,
        ),
        (
            vec!["root", "std", "io", "writeu16ln"].into(),
            vec![Type::U16],
            Type::Unit,
        ),
        (
            vec!["root", "std", "io", "writeu32"].into(),
            vec![Type::U32],
            Type::Unit,
        ),
        (
            vec!["root", "std", "io", "writeu32ln"].into(),
            vec![Type::U32],
            Type::Unit,
        ),
        (
            vec!["root", "std", "io", "writeu64"].into(),
            vec![Type::U64],
            Type::Unit,
        ),
        (
            vec!["root", "std", "io", "writeu64ln"].into(),
            vec![Type::U64],
            Type::Unit,
        ),
        (
            vec!["root", "std", "io", "writei64"].into(),
            vec![Type::I64],
            Type::Unit,
        ),
        (
            vec!["root", "std", "io", "writei64ln"].into(),
            vec![Type::I64],
            Type::Unit,
        ),
        (
            vec!["root", "std", "io", "writei32"].into(),
            vec![Type::I32],
            Type::Unit,
        ),
        (
            vec!["root", "std", "io", "writei32ln"].into(),
            vec![Type::I32],
            Type::Unit,
        ),
        (
            vec!["root", "std", "io", "writei16"].into(),
            vec![Type::I16],
            Type::Unit,
        ),
        (
            vec!["root", "std", "io", "writei16ln"].into(),
            vec![Type::I16],
            Type::Unit,
        ),
        (
            vec!["root", "std", "io", "writei8"].into(),
            vec![Type::I8],
            Type::Unit,
        ),
        (
            vec!["root", "std", "io", "writei8ln"].into(),
            vec![Type::I8],
            Type::Unit,
        ),
        (
            vec!["root", "std", "io", "writebool"].into(),
            vec![Type::Bool],
            Type::Unit,
        ),
        (
            vec!["root", "std", "io", "writeboolln"].into(),
            vec![Type::Bool],
            Type::Unit,
        ),
    ]
}
