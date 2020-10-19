#![allow(dead_code)]

fn main() {
    println!("Hello, world!");
    let ast = new_ast();
    println!("{:?}", ast);
    let program = assembly::Program::compile(&ast);
    program.print();

    let text = "2 * 3 + 4";
    let tokens = Token::tokenize(&text);
    println!("{:?}", tokens);
    let tokens = tokens
        .into_iter()
        .filter(|t| t.is_ok())
        .map(|t| t.unwrap())
        .collect();
    let ast = Node::parse(tokens);
    println!("{:?}", ast);

    let program = assembly::Program::compile(&ast.unwrap());
    program.print();
}

fn new_ast<'a>() -> Node {
    let ast = Node {
        value: Token::Mul,
        left: Some(Box::new(Node {
            value: Token::Add,
            left: Some(Box::new(Node {
                value: Token::Integer(2),
                left: None,
                right: None,
            })),
            right: Some(Box::new(Node {
                value: Token::Integer(4),
                left: None,
                right: None,
            })),
        })),
        right: Some(Box::new(Node {
            value: Token::Integer(4),
            left: None,
            right: None,
        })),
    };
    ast
}

// Token - a type which captures the different types of tokens and which is output
// by tokenize
#[derive(Debug)]
pub enum Token {
    Integer(i32),
    Mul,
    Add,
}

impl Token {
    /// Split a string into tokens, first by splitting by whitespace and then
    /// for each substring determining if it is an operator or an integer.
    pub fn tokenize(text: &str) -> Vec<Result<Token, &str>> {
        let ss = text.split_ascii_whitespace();
        ss.map(|s| match s.parse::<i32>() {
            Ok(i) => Ok(Token::Integer(i)),
            Err(_) => match s {
                "*" => Ok(Token::Mul),
                "+" => Ok(Token::Add),
                _ => Err("Invalid token"),
            },
        })
        .collect()
    }
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

impl Node {
    /*
        Grammar
        NUMBER := 0-9*
        FACTOR := NUMBER | (EXPRESSION)
        TERM := FACTOR [* TERM]
        EXPRESSION :=  TERM [+ EXPRESSION]
        BIND := let IDENTIFIER = EXPRESSION;

        tokenize - takes a string of text and converts it to a string of tokens
        parse - takes a string of tokens and converts it into an AST
        compile - takes an AST and converts it to assembly
    */
    pub fn parse(tokens: Vec<Token>) -> Option<Node> {
        let mut iter = tokens.iter().peekable();
        Node::expression(&mut iter)
    }

    fn expression(iter: &mut std::iter::Peekable<core::slice::Iter<Token>>) -> Option<Node> {
        match Node::term(iter) {
            Some(n) => match iter.peek() {
                Some(Token::Add) => {
                    iter.next();
                    let n2 = Node::expression(iter).expect("An expression after +");
                    Some(Node {
                        value: Token::Add,
                        left: Some(Box::new(n)),
                        right: Some(Box::new(n2)),
                    })
                }
                _ => Some(n),
            },
            None => None,
        }
    }

    fn term(iter: &mut std::iter::Peekable<core::slice::Iter<Token>>) -> Option<Node> {
        match Node::factor(iter) {
            Some(n) => match iter.peek() {
                Some(Token::Mul) => {
                    iter.next();
                    let n2 = Node::term(iter).expect("a valid term after *");
                    Some(Node {
                        value: Token::Mul,
                        left: Some(Box::new(n)),
                        right: Some(Box::new(n2)),
                    })
                }
                _ => Some(n),
            },
            None => None,
        }
    }

    fn factor(iter: &mut std::iter::Peekable<core::slice::Iter<Token>>) -> Option<Node> {
        match Node::number(iter) {
            Some(n) => {
                iter.next();
                Some(n)
            }
            None => None,
        }
    }

    fn number(iter: &mut std::iter::Peekable<core::slice::Iter<Token>>) -> Option<Node> {
        match iter.peek() {
            Some(token) => match token {
                Token::Integer(i) => Some(Node {
                    value: Token::Integer(*i),
                    left: None,
                    right: None,
                }),
                _ => None,
            },
            None => None,
        }
    }
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

    impl std::fmt::Display for Register {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Register::Eax => f.write_str("eax"),
                Register::Ebx => f.write_str("ebx"),
                Register::Ecx => f.write_str("ecx"),
            }
        }
    }

    #[derive(Debug)]
    enum Memory {}

    #[derive(Debug)]
    enum Location {
        Register(Register),
        Memory(Memory),
    }

    impl std::fmt::Display for Location {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
            match self {
                Location::Register(reg) => {
                    let s = format!("{}", reg);
                    f.write_str(&s)
                }
                Location::Memory(_) => f.write_str("mem"),
            }
        }
    }

    #[derive(Debug)]
    enum Source {
        Register(Register),
        Memory(Memory),
        Integer(i32),
    }

    impl std::fmt::Display for Source {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
            match self {
                Source::Register(reg) => {
                    let s = format!("{}", reg);
                    f.write_str(&s)
                }
                Source::Memory(_) => f.write_str("mem"),
                Source::Integer(i) => {
                    let s = format!("{}", i);
                    f.write_str(&s)
                }
            }
        }
    }

    type Label = String;

    #[derive(Debug)]
    enum Instr {
        Jmp(Label),
        Mov(Location, Source),
        Add(Register, Location),
        IMul(Register, Location),
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
                match inst {
                    Assembly::Instr(inst) => match inst {
                        Instr::Mov(l, s) => println!("mov {}, {}", l, s),
                        Instr::Push(reg) => {
                            println!("push {}", reg);
                        }
                        Instr::Pop(reg) => {
                            println!("pop {}", reg);
                        }
                        Instr::IMul(reg, s) => {
                            println!("imul {}, {}", reg, s);
                        }
                        Instr::Add(reg, s) => {
                            println!("add {}, {}", reg, s);
                        }
                        _ => {
                            println!("{:?}", inst);
                        }
                    },
                    _ => {}
                }
            }
        }

        pub fn compile(ast: &super::Node) -> Program {
            let mut code = vec![];
            Program::traverse(ast, &mut code);
            Program { code }
        }

        fn traverse(ast: &super::Node, output: &mut Vec<Assembly>) {
            if ast.left.is_none() && ast.right.is_none() {
                match ast.value {
                    super::Token::Integer(i) => {
                        output.push(Assembly::Instr(Instr::Mov(
                            Location::Register(Register::Eax),
                            Source::Integer(i),
                        )));
                        output.push(Assembly::Instr(Instr::Push(Register::Eax)));
                    }
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
                        output.push(Assembly::Instr(Instr::Pop(Register::Ebx)));
                        output.push(Assembly::Instr(Instr::Pop(Register::Eax)));
                        output.push(Assembly::Instr(Instr::IMul(
                            Register::Eax,
                            Location::Register(Register::Ebx),
                        )));
                        output.push(Assembly::Instr(Instr::Push(Register::Eax)));
                    }
                    super::Token::Add => {
                        let left = ast.left.as_ref().unwrap();
                        Program::traverse(left, output);
                        let right = ast.right.as_ref().unwrap();
                        Program::traverse(right, output);
                        output.push(Assembly::Instr(Instr::Pop(Register::Ebx)));
                        output.push(Assembly::Instr(Instr::Pop(Register::Eax)));
                        output.push(Assembly::Instr(Instr::Add(
                            Register::Eax,
                            Location::Register(Register::Ebx),
                        )));
                        output.push(Assembly::Instr(Instr::Push(Register::Eax)));
                    }
                    _ => println!("Expected an operator"),
                }
            }
        }
    }
}
