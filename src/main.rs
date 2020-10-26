#![allow(dead_code)]

mod compiler;
mod lexer;
mod parser;
mod type_checker;

use compiler::*;
use lexer::*;
use parser::*;
use type_checker::*;

fn main() {
    let text = "
        fn my_main ( p ) { 
            c := init my_co ;
            w := yield c ;
            x := 1 ; 
            println x ; 
            z := yield c ;
            println z ;
            return x + p ; 
        }

        co my_co ( ) {
            yret 1 ;
            return 2 ;
        }
        ";

    println!("Code: {}", text);
    let tokens = Token::tokenize(&text);
    println!("Tokens: {:?}", tokens);
    let tokens = tokens
        .into_iter()
        .filter(|t| t.is_ok())
        .map(|t| t.unwrap())
        .collect();
    let ast = Node::parse(tokens);
    println!("AST: {:?}", ast);

    let ast = ast.unwrap();
    let mut func_table = FunctionTable::generate(&ast);
    println!("FuncTable: {:?}", func_table);

    let program = Compiler::compile(&ast, &mut func_table);
    let mut output =
        std::fs::File::create("./target/output.asm").expect("Failed to create output file");
    //let mut output = std::io::stdout();
    program
        .print(&mut output)
        .expect("Failed to write assembly");
}
