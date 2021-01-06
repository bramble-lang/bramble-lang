#![allow(dead_code)]
#![feature(box_syntax, box_patterns)]

mod compiler;
mod diagnostics;
mod lexer;
mod semantics;
mod syntax;

use clap::{App, Arg};
use compiler::compiler::*;
use diagnostics::config::TracingConfig;
use lexer::tokens::Token;
use semantics::type_checker::*;
use syntax::ast;
use syntax::parser;

fn main() {
    let matches = App::new("Braid Compiler")
        .version("0.1.0")
        .author("Erich Ess")
        .about("Compiles Braid language files into x86 assembly for use by the NASM assembler")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .takes_value(true)
                .help("Source code file to compile"),
        )
        .arg(
            Arg::with_name("output")
                .short("o")
                .long("output")
                .takes_value(true)
                .help("Name the output file that the assembly will be written to"),
        )
        .arg(
            Arg::with_name("trace-parser")
                .long("trace-parser")
                .takes_value(true)
                .help("Prints out a trace of all the steps the parser follows as it converts the token vector into an AST.  The current token is printed next to the step.
                This is for debugging the parser when adding new syntactical elements.")
        )
        .arg(
            Arg::with_name("trace-lexer")
                .long("trace-lexer")
                .takes_value(true)
                .help("Prints out a trace of all the steps the lexer follows as it converts the token vector into an AST.  The current token is printed next to the step.
                This is for debugging the lexer when adding new tokens.")
        )
        .arg(
            Arg::with_name("trace-symbol-table")
                .long("trace-symbol-table")
                .takes_value(true)
                .help("Prints out a trace of the value of the symbol table at each node in the AST.  You can specify specify to only trace specific lines in the source code file.")
        )
        .arg(
            Arg::with_name("trace-path")
                .long("trace-path")
                .takes_value(true)
                .help("Prints out the current module path at the current line of code.")
        )
        .get_matches();

    let input = matches
        .value_of("input")
        .expect("Expected an input source file to compile");
    let text = std::fs::read_to_string(input).expect("Failed to read input file");

    let trace_lexer = TracingConfig::parse(matches.value_of("trace-lexer"));
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

    let trace_parser = TracingConfig::parse(matches.value_of("trace-parser"));
    //let trace_parser = if matches.is_present("trace-parser") {TracingConfig::All} else {TracingConfig::Off};
    parser::set_tracing(trace_parser);
    let ast = match parser::parse(tokens) {
        Ok(Some(ast)) => ast,
        Ok(None) => {
            println!("Critical: no AST was generated by the parser");
            std::process::exit(2);
        }
        Err(msg) => {
            println!("Error: {}", msg);
            std::process::exit(3);
        }
    };

    // Type Check
    let trace_semantic_analysis = TracingConfig::parse(matches.value_of("trace-symbol-table"));
    let trace_path = TracingConfig::parse(matches.value_of("trace-path"));
    let semantic_ast = match type_check(&ast, trace_semantic_analysis, trace_path) {
        Ok(ast) => {
            //func_table = FunctionTable::from_semantic_ast(&ast);
            ast
        }
        Err(msg) => {
            println!("Error: {}", msg);
            std::process::exit(1);
        }
    };

    let program = Compiler::compile(&semantic_ast);
    let output_target = matches.value_of("output").unwrap_or("./target/output.asm");
    let mut output = std::fs::File::create(output_target).expect("Failed to create output file");
    Compiler::print(&program, &mut output).expect("Failed to write assembly");
}
