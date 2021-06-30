#![allow(dead_code)]
#![feature(box_syntax, box_patterns)]

mod ast;
mod cli;
mod diagnostics;
mod io;
mod lexer;
mod llvm;
mod parser;
mod project;
mod result;
mod semantics;

use std::{path::Path, process::exit};

use cli::*;
use diagnostics::config::TracingConfig;
use inkwell::context::Context;
use io::read_manifests;
use manifest::Manifest;
use project::*;
use semantics::type_resolver::*;

const BRAID_FILE_EXT: &str = "br";
const ROOT_MODULE_NAME: &str = "root";
const USER_MAIN_FN: &str = "my_main";

fn main() {
    let config = configure_cli().get_matches();

    let input = config
        .value_of("input")
        .expect("Expected an input source file to compile");
    let src_path = Path::new(input);
    let project_name = get_project_name(&src_path).unwrap();
    let src_input = read_src_files(&src_path, BRAID_FILE_EXT);

    let imports: Vec<_> = match read_manifests(&config) {
        Ok(imports) => imports,
        Err(errs) => {
            print_errs(&errs);
            exit(ERR_IMPORT_ERROR)
        }
    };

    let trace_lexer = TracingConfig::parse(config.value_of("trace-lexer"));
    let token_sets = match tokenize_project(src_input, trace_lexer) {
        Ok(ts) => ts,
        Err(errs) => {
            print_errs(&errs);
            exit(ERR_LEXER_ERROR)
        }
    };

    let trace_parser = TracingConfig::parse(config.value_of("trace-parser"));
    let root = match parse_project(project_name, token_sets, trace_parser) {
        Ok(root) => root,
        Err(errs) => {
            print_errs(&errs);
            exit(ERR_PARSER_ERROR)
        }
    };

    // Type Check
    let trace_semantic_node = TracingConfig::parse(config.value_of("trace-semantic-node"));
    let trace_type_resolver = TracingConfig::parse(config.value_of("trace-type-resolver"));
    let trace_path = TracingConfig::parse(config.value_of("trace-path"));

    let semantic_ast = match resolve_types_with_imports(
        &root,
        USER_MAIN_FN,
        &imports,
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

    let context = Context::create();
    let mut llvm = llvm::IrGen::new(&context, project_name, &imports);
    match llvm.ingest(&semantic_ast, USER_MAIN_FN) {
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

    if config.is_present("manifest") {
        let manifest = Manifest::extract(&semantic_ast);
        match std::fs::File::create(format!("./target/{}.manifest", project_name))
            .map_err(|e| format!("{}", e))
            .and_then(|mut f| manifest.write(&mut f).map_err(|e| format!("{}", e)))
        {
            Ok(()) => (),
            Err(e) => {
                println!("Failed to write manifest file: {}", e);
                exit(ERR_MANIFEST_WRITE_ERROR)
            }
        }
    }
}
