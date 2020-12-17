#![allow(dead_code)]
#![feature(box_syntax, box_patterns)]

mod compiler;
mod lexer;
mod semantics;
mod syntax;

use clap::{App, Arg};
use compiler::compiler::*;
use lexer::tokens::Token;
use semantics::type_checker::*;
use syntax::ast;
use syntax::parser;
use syntax::parser2;

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
        .get_matches();

    let input = matches
        .value_of("input")
        .expect("Expected an input source file to compile");
    let text = std::fs::read_to_string(input).expect("Failed to read input file");

    let mut lexer = crate::lexer::lexer::Lexer::new(&text);
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

    let ast = match parser2::parse(tokens) {
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
    let semantic_ast = match checker::type_check(&ast) {
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
