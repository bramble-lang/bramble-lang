#![allow(dead_code)]

fn main() {
    println!("Hello, world!");
    let ast = new_ast();
    println!("{:?}", ast);
    let program = assembly::Program::compile(ast);
    program.print();
}

fn new_ast<'a>() -> Node {
    let ast = Node {
        value: Token::Mul,
        left: Some(Box::new(Node{
            value: Token::Integer(2),
            left: None,
            right: None,
        })),
        right: Some(Box::new(Node{
            value: Token::Integer(4),
            left: None,
            right: None,
        })),
    };
    ast
}

/*
    Grammar
    NUMBER := 0-9*
    FACTOR := NUMBER | (EXPRESSION)
    TERM := FACTOR [* FACTOR]
    EXPRESSION :=  EXPRESSION [+ TERM]
    BIND := let IDENTIFIER = EXPRESSION;

    tokenize - takes a string of text and converts it to a string of tokens
    parse - takes a string of tokens and converts it into an AST
    compile - takes an AST and converts it to assembly
*/

// Token - a type which captures the different types of tokens and which is output
// by tokenize
#[derive(Debug)]
enum Token {
    Integer(i32),
    Mul,
    Add,
}

// AST - a type(s) which is used to construct an AST representing the logic of the
// program
type NodeOption = Option<Box<Node>>;

#[derive(Debug)]
pub struct Node {
    value: Token,
    left: NodeOption,
    right: NodeOption,
}

// ASM - types capturing the different assembly instructions along with functions to
// convert to text so that a compiled program can be saves as a file of assembly
// instructions
pub mod assembly {
    #[derive(Debug)]
    enum Register {
        Eax,
        Ebx,
        Ecx,
    }

    #[derive(Debug)]
    enum Memory {}

    #[derive(Debug)]
    enum Location {
        Register(Register),
        Memory(Memory),
    }

    #[derive(Debug)]
    enum Source {
        Register(Register),
        Memory(Memory),
        Integer(i32),
    }

    type Label = String;

    #[derive(Debug)]
    enum Instr {
        Jmp(Label),
        Mov(Location, Source),
        Add(Register, Location),
        Mul(Register, Location),
        Push(Register),
        Pop(Register),
    }

    #[derive(Debug)]
    enum Assembly {
        Label(Label),
        Instr(Instr),
    }

    pub struct Program {
        code: Vec<Assembly>,
    }

    impl Program {
        pub fn print(&self) {
            for inst in self.code.iter() {
                println!("{:?}", inst);
            }
        }

        pub fn compile(ast: super::Node) -> Program {
            let mut code = vec![];
            Program::traverse(&ast, &mut code);
            Program{
                code,
            }
        }

        fn traverse(ast: &super::Node, output: &mut Vec<Assembly>) {
            if ast.left.is_none() && ast.right.is_none() {
                match ast.value {
                    super::Token::Integer(i) => {
                        output.push(
                            Assembly::Instr(
                                Instr::Mov(Location::Register(Register::Eax), Source::Integer(i))
                            )
                        );
                        output.push(
                            Assembly::Instr(
                                Instr::Push(Register::Eax)
                            )
                        );
                    },
                    _ => {
                        println!("Expected integer");
                    }
                }
            } else if ast.left.is_some() && ast.right.is_some() {
                match ast.value {
                    super::Token::Mul => {
                        let left = ast.left.as_ref().unwrap();
                        Program::traverse(left, output);
                        let right = ast.right.as_ref().unwrap();
                        Program::traverse(right, output);
                        output.push(
                            Assembly::Instr(
                                Instr::Pop(Register::Ebx)
                            )
                        );
                        output.push(
                            Assembly::Instr(
                                Instr::Pop(Register::Eax)
                            )
                        );
                        output.push(
                            Assembly::Instr(Instr::Mul(Register::Eax, Location::Register(Register::Ebx)))
                        );
                        output.push(
                            Assembly::Instr(Instr::Push(Register::Eax))
                        )
                    }
                    _ => {
                        println!("Expected an operator")
                    }
                }
            }
        }
    }
}
