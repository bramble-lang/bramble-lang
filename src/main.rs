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
    let src_input = read_src_files(&src_path, BRAID_FILE_EXT);

    let trace_lexer = TracingConfig::parse(config.value_of("trace-lexer"));
    let token_sets = match tokenize_project(src_input, trace_lexer) {
        Ok(ts) => ts,
        Err(errs) => {
            for e in errs {
                println!("{}", e);
            }
            exit(ERR_LEXER_ERROR)
        }
    };

    let trace_parser = TracingConfig::parse(config.value_of("trace-parser"));
    let root = match parse_project(ROOT_MODULE_NAME, token_sets, trace_parser) {
        Ok(root) => root,
        Err(msg) => {
            println!("{}", msg);
            exit(ERR_PARSER_ERROR)
        }
    };

    // Type Check
    let trace_semantic_node = TracingConfig::parse(config.value_of("trace-semantic-node"));
    let trace_type_resolver = TracingConfig::parse(config.value_of("trace-type-resolver"));
    let trace_path = TracingConfig::parse(config.value_of("trace-path"));
    let imported = configure_imported_functions();

    let semantic_ast = match resolve_types_with_imports(
        &root,
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

    let context = Context::create();
    let mut llvm = llvm::IrGen::new(&context, "test", &imported);
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
}
