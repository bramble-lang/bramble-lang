#![allow(dead_code)]

fn main() {
    println!("Hello, world!");
    let ast = new_ast();
    println!("{:?}", ast);
    let program = assembly::Program::compile(&ast);
    program.print();

    let text = "x := 5";
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
    let vartable = VarTable::generate(&ast);
    println!("VarTable: {:?}", vartable);

    let program = assembly::Program::compile(&ast);
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
    Identifier(String),
    Mul,
    Add,
    Assign,
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
                ":=" => Ok(Token::Assign),
                s if s.is_ascii() => Ok(Token::Identifier(s.into())),
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
        BIND := IDENTIFIER := EXPRESSION;

        tokenize - takes a string of text and converts it to a string of tokens
        parse - takes a string of tokens and converts it into an AST
        compile - takes an AST and converts it to assembly
    */
    pub fn parse(tokens: Vec<Token>) -> Option<Node> {
        let mut iter = tokens.iter().peekable();
        //Node::expression(&mut iter)
        Node::bind(&mut iter)
    }

    fn bind(iter: &mut std::iter::Peekable<core::slice::Iter<Token>>) -> Option<Node> {
        match Node::identifier(iter) {
            Some(n) => {
                println!("{:?}", n);
                let pt = iter.peek();
                println!("peek: {:?}", pt);
                match pt {
                    Some(Token::Assign) => {
                        iter.next();
                        let exp = Node::expression(iter).expect("Expected an expression after :=");
                        Some(Node {
                            value: Token::Assign,
                            left: Some(Box::new(n)),
                            right: Some(Box::new(exp)),
                        })
                    }
                    _ => {
                        println!("Expected := after identifer in bind statement");
                        None
                    }
                }
            }
            None => {
                println!("Expected an identifier on the LHS");
                None
            }
        }
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
            Some(n) => Some(n),
            None => None,
        }
    }

    fn identifier(iter: &mut std::iter::Peekable<core::slice::Iter<Token>>) -> Option<Node> {
        println!("Identifier");
        match iter.peek() {
            Some(token) => match token {
                Token::Identifier(id) => {
                    iter.next();
                    Some(Node {
                        value: Token::Identifier(id.clone()),
                        left: None,
                        right: None,
                    })
                }
                _ => None,
            },
            None => None,
        }
    }

    fn number(iter: &mut std::iter::Peekable<core::slice::Iter<Token>>) -> Option<Node> {
        match iter.peek() {
            Some(token) => match token {
                Token::Integer(i) => {
                    iter.next();
                    Some(Node {
                        value: Token::Integer(*i),
                        left: None,
                        right: None,
                    })
                }
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
        Ebp,
        Esp,
    }

    impl std::fmt::Display for Register {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Register::Eax => f.write_str("eax"),
                Register::Ebx => f.write_str("ebx"),
                Register::Ecx => f.write_str("ecx"),
                Register::Ebp => f.write_str("ebp"),
                Register::Esp => f.write_str("esp"),
            }
        }
    }

    #[derive(Debug)]
    enum Memory {}

    #[derive(Debug)]
    enum Location {
        Register(Register),
        Memory(String),
    }

    impl std::fmt::Display for Location {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
            match self {
                Location::Register(reg) => {
                    let s = format!("{}", reg);
                    f.write_str(&s)
                }
                Location::Memory(m) => {
                    let s = format!("[{}]", m);
                    f.write_str(&s)
                }
            }
        }
    }

    #[derive(Debug)]
    enum Source {
        Register(Register),
        Memory(String),
        Integer(i32),
    }

    impl std::fmt::Display for Source {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
            match self {
                Source::Register(reg) => {
                    let s = format!("{}", reg);
                    f.write_str(&s)
                }
                Source::Memory(m) => {
                    let s = format!("[{}]", m);
                    f.write_str(&s)
                }
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
                    Assembly::Instr(inst) => {
                        print!("    ");
                        match inst {
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
                        }
                    }
                    _ => {}
                }
            }
        }

        pub fn compile(ast: &super::Node) -> Program {
            let mut code = vec![];
            code.push(Assembly::Instr(Instr::Push(Register::Ebp)));
            code.push(Assembly::Instr(Instr::Mov(
                Location::Register(Register::Ebp),
                Source::Register(Register::Esp),
            )));
            Program::traverse(ast, &mut code);
            code.push(Assembly::Instr(Instr::Mov(
                Location::Register(Register::Esp),
                Source::Register(Register::Ebp),
            )));
            code.push(Assembly::Instr(Instr::Pop(Register::Ebp)));
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
                    super::Token::Assign => {
                        let id = match &ast.left.as_ref().unwrap().value {
                            super::Token::Identifier(id) => id,
                            _ => panic!("CRITICAL: expected identifier on LHS of bind statement"),
                        };
                        let right = ast.right.as_ref().unwrap();
                        Program::traverse(right, output);
                        output.push(Assembly::Instr(Instr::Pop(Register::Eax)));
                        output.push(Assembly::Instr(Instr::Mov(
                            Location::Memory(id.clone()),
                            Source::Register(Register::Eax),
                        )));
                    }
                    _ => println!("Expected an operator"),
                }
            }
        }
    }
}

#[derive(Debug)]
struct VarTable {
    vars: Vec<(String, i32)>,
}

impl VarTable {
    pub fn generate(ast: &Node) -> VarTable {
        let mut vt = VarTable { vars: vec![] };
        VarTable::find_bound_identifiers(ast, &mut vt);
        if VarTable::has_duplicates(&vt) {
            panic!("An identifier was defined twice");
        }
        vt
    }

    fn find_bound_identifiers(ast: &Node, output: &mut VarTable) {
        match ast.value {
            Token::Assign => {
                let id = match &ast.left.as_ref().unwrap().value {
                    Token::Identifier(id) => id,
                    _ => panic!("CRITICAL: expected identifer on LHS of bind operator"),
                };

                output.vars.push((id.clone(), 4));
            }
            _ => {}
        }
        match &ast.left {
            Some(n) => VarTable::find_bound_identifiers(n, output),
            None => (),
        }
        match &ast.right {
            Some(n) => VarTable::find_bound_identifiers(n, output),
            None => (),
        }
    }

    fn has_duplicates(var_table: &VarTable) -> bool {
        (1..var_table.vars.len()).any(|i| var_table.vars[i..].contains(&var_table.vars[i - 1]))
    }
}
