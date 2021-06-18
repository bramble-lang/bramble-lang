#![allow(dead_code)]
#![feature(box_syntax, box_patterns)]

mod ast;
mod diagnostics;
mod lexer;
mod llvm;
mod parser;
mod semantics;

use std::path::{Path, PathBuf};

use ast::Type;
use clap::{App, Arg};
use diagnostics::config::TracingConfig;
use inkwell::context::Context;
use lexer::tokens::Token;
use semantics::type_resolver::*;

use crate::ast::Module;

fn main() {
    let config = configure_cli().get_matches();

    let input = config
        .value_of("input")
        .expect("Expected an input source file to compile");
    let src_path = Path::new(input);
    let files = get_files(&src_path).unwrap();
    let mut texts: Vec<(String, String)> = vec![];
    for file in files {
        let fpath = file.as_path();
        let rel_path = fpath.strip_prefix(&src_path).unwrap();
        let p: Vec<String> = rel_path
            .iter()
            .map(|e| e.to_str().unwrap().into())
            .collect();
        println!("{:?}", p);

        let name = file.as_path().file_stem().unwrap().to_str().unwrap().into();
        let text = std::fs::read_to_string(file).expect("Failed to read input file");
        texts.push((name, text));
    }

    let trace_lexer = TracingConfig::parse(config.value_of("trace-lexer"));
    let mut token_sets = vec![];
    for (name, text) in texts {
        let mut lexer = crate::lexer::lexer::Lexer::new(&text);
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
        token_sets.push((name, tokens));
    }

    let trace_parser = TracingConfig::parse(config.value_of("trace-parser"));
    parser::parser::set_tracing(trace_parser);

    let mut asts = vec![];
    let mut root = Module::new("root", 0);
    for (name, tokens) in &token_sets {
        match parser::parser::parse(&name, tokens) {
            Ok(Some(ast)) => root.add_module(ast),
            Ok(None) => {
                println!("Critical: no AST was generated by the parser");
                std::process::exit(ERR_NO_AST);
            }
            Err(msg) => {
                println!("Error: {}", msg);
                std::process::exit(ERR_PARSER_ERROR);
            }
        };
    }
    asts.push(root);

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
        llvm.ingest(&semantic_ast);

        if config.is_present("emit") {
            llvm.print(Path::new("./target/output.ll"));
        }

        llvm.emit_object_code(Path::new(output_target)).unwrap();
    } else {
    }
}

fn get_files(path: &Path) -> Result<Vec<PathBuf>, std::io::Error> {
    let mut files = vec![];
    match path.extension() {
        None => {
            let dir = std::fs::read_dir(path)?;
            for f in dir {
                let f = f?;
                let fty = f.file_type().unwrap();
                if fty.is_file() {
                    match f.path().extension() {
                        Some(ex) if ex.to_ascii_lowercase() == "br" => {
                            files.push(f.path());
                        }
                        _ => (),
                    }
                } else if fty.is_dir() {
                    let mut sub_files = get_files(&f.path())?;
                    files.append(&mut sub_files);
                }
            }
        }
        Some(ex) if ex.to_ascii_lowercase() == "br" => {
            files.push(path.to_path_buf());
        }
        Some(_) => {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "Is not a Braid language file",
            ));
        }
    }
    Ok(files)
}

// Exit Codes for different types of errors
const ERR_TYPE_CHECK: i32 = 1;
const ERR_NO_AST: i32 = 2;
const ERR_PARSER_ERROR: i32 = 3;

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
