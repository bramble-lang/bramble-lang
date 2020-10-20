#![allow(dead_code)]

fn main() {
    let text = "fn test { x := 5 ; y := 2 + x ; return y ; }";
    //let text = "x := 5 ;";
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

    let program = assembly::Program::compile(&ast, &vartable);
    program.print();
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
    Semicolon,
    Return,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
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
                ";" => Ok(Token::Semicolon),
                "(" => Ok(Token::LParen),
                ")" => Ok(Token::RParen),
                "{" => Ok(Token::LBrace),
                "}" => Ok(Token::RBrace),
                "fn" => Ok(Token::Function),
                "return" => Ok(Token::Return),
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
        IDENTIFIER := A-Za-z*
        NUMBER := 0-9*
        FACTOR := NUMBER | IDENTIFIER | (EXPRESSION)
        TERM := FACTOR [* TERM]
        EXPRESSION :=  TERM [+ EXPRESSION]
        BIND := IDENTIFIER := EXPRESSION
        RETURN := return [EXPRESSION] SEMICOLON
        STATEMENT := [BIND] SEMICOLON
        BLOCK := STATEMENT*
        FUNCTION := fn IDENTIFIER LPAREN RPAREN LBRACE BLOCK RETURN RBRACE

        tokenize - takes a string of text and converts it to a string of tokens
        parse - takes a string of tokens and converts it into an AST
        compile - takes an AST and converts it to assembly
    */
    pub fn parse(tokens: Vec<Token>) -> Option<Vec<Node>> {
        let mut iter = tokens.iter().peekable();
        Node::function(&mut iter)
    }

    fn function(iter: &mut std::iter::Peekable<core::slice::Iter<Token>>) -> Option<Vec<Node>> {
        match iter.peek() {
            Some(Token::Function) => {
                iter.next();
                match iter.peek() {
                    Some(Token::Identifier(id)) => {
                        iter.next();
                        match iter.peek() {
                            Some(Token::LBrace) => {
                                iter.next();
                                let mut stmts = Node::block(iter);

                                match Node::return_stmt(iter) {
                                    Some(ret) => stmts.push(ret),
                                    None => panic!("Function must end with a return statement"),
                                }

                                match iter.peek() {
                                    Some(Token::RBrace) => {
                                        iter.next();
                                    }
                                    _ => panic!("Expected } at end of function definition"),
                                }
                                Some(stmts)
                            }
                            _ => panic!("Expected { after function declaration"),
                        }
                    }
                    _ => panic!("Expected function name after fn"),
                }
            }
            _ => panic!("Expected a function definition"),
        }
    }

    fn block(iter: &mut std::iter::Peekable<core::slice::Iter<Token>>) -> Vec<Node> {
        let mut stmts = vec![];
        while iter.peek().is_some() {
            match Node::statement(iter) {
                Some(s) => stmts.push(s),
                None => break,
            }
        }
        stmts
    }

    fn return_stmt(iter: &mut std::iter::Peekable<core::slice::Iter<Token>>) -> Option<Node> {
        match iter.peek() {
            Some(Token::Return) => {
                iter.next();
                let exp = Node::expression(iter);
                match iter.peek() {
                    Some(Token::Semicolon) => iter.next(),
                    _ => panic!("Expected ; after return statement"),
                };
                match exp {
                    Some(exp) => Some(Node {
                        value: Token::Return,
                        left: Some(Box::new(exp)),
                        right: None,
                    }),
                    None => Some(Node {
                        value: Token::Return,
                        left: None,
                        right: None,
                    }),
                }
            }
            _ => None,
        }
    }

    fn statement(iter: &mut std::iter::Peekable<core::slice::Iter<Token>>) -> Option<Node> {
        match Node::bind(iter) {
            Some(b) => match iter.peek() {
                Some(Token::Semicolon) => {
                    iter.next();
                    Some(b)
                }
                _ => {
                    panic!("Expected ; after bind statement");
                }
            },
            None => None,
        }
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
            None => None,
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
            None => match Node::identifier(iter) {
                Some(n) => Some(n),
                None => None,
            },
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
        Add(Register, Source),
        Sub(Register, Source),
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
                            Instr::Sub(reg, s) => {
                                println!("sub {}, {}", reg, s);
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

        pub fn compile(ast: &Vec<super::Node>, vars: &super::VarTable) -> Program {
            let mut code = vec![];
            code.push(Assembly::Instr(Instr::Push(Register::Ebp)));
            code.push(Assembly::Instr(Instr::Mov(
                Location::Register(Register::Ebp),
                Source::Register(Register::Esp),
            )));

            let total_offset = vars.vars.last().unwrap().2;
            code.push(Assembly::Instr(Instr::Sub(
                Register::Esp,
                Source::Integer(total_offset),
            )));

            for stmt in ast.iter() {
                Program::traverse(stmt, vars, &mut code);
            }

            code.push(Assembly::Instr(Instr::Mov(
                Location::Register(Register::Esp),
                Source::Register(Register::Ebp),
            )));
            code.push(Assembly::Instr(Instr::Pop(Register::Ebp)));
            Program { code }
        }

        fn traverse(ast: &super::Node, vars: &super::VarTable, output: &mut Vec<Assembly>) {
            if ast.left.is_none() && ast.right.is_none() {
                match &ast.value {
                    super::Token::Integer(i) => {
                        output.push(Assembly::Instr(Instr::Mov(
                            Location::Register(Register::Eax),
                            Source::Integer(*i),
                        )));
                        output.push(Assembly::Instr(Instr::Push(Register::Eax)));
                    }
                    super::Token::Identifier(id) => {
                        let id_offset = {
                            let var = vars
                                .vars
                                .iter()
                                .find(|v| v.0 == *id)
                                .expect("CRITICAL: identifier not found in var table");
                            var.2
                        };
                        output.push(Assembly::Instr(Instr::Mov(
                            Location::Register(Register::Eax),
                            Source::Memory(format!("ebp-{}", id_offset)),
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
                        Program::traverse(left, vars, output);
                        let right = ast.right.as_ref().unwrap();
                        Program::traverse(right, vars, output);
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
                        Program::traverse(left, vars, output);
                        let right = ast.right.as_ref().unwrap();
                        Program::traverse(right, vars, output);
                        output.push(Assembly::Instr(Instr::Pop(Register::Ebx)));
                        output.push(Assembly::Instr(Instr::Pop(Register::Eax)));
                        output.push(Assembly::Instr(Instr::Add(
                            Register::Eax,
                            Source::Register(Register::Ebx),
                        )));
                        output.push(Assembly::Instr(Instr::Push(Register::Eax)));
                    }
                    super::Token::Assign => {
                        let id_offset = match &ast.left.as_ref().unwrap().value {
                            super::Token::Identifier(id) => {
                                let var = vars
                                    .vars
                                    .iter()
                                    .find(|v| v.0 == *id)
                                    .expect("CRITICAL: identifier not found in var table");
                                var.2
                            }
                            _ => panic!("CRITICAL: expected identifier on LHS of bind statement"),
                        };
                        let right = ast.right.as_ref().unwrap();
                        Program::traverse(right, vars, output);
                        output.push(Assembly::Instr(Instr::Pop(Register::Eax)));
                        output.push(Assembly::Instr(Instr::Mov(
                            Location::Memory(format!("ebp-{}", id_offset)),
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
pub struct VarTable {
    vars: Vec<(String, i32, i32)>,
}

impl VarTable {
    pub fn generate(ast: &Vec<Node>) -> VarTable {
        let mut vt = VarTable { vars: vec![] };
        let mut offset = 0;
        for n in ast.iter() {
            offset = VarTable::find_bound_identifiers(n, &mut vt, offset);
        }
        if VarTable::has_duplicates(&vt) {
            panic!("An identifier was defined twice");
        }
        vt
    }

    fn find_bound_identifiers(ast: &Node, output: &mut VarTable, total_offset: i32) -> i32 {
        let total_offset = match ast.value {
            Token::Assign => {
                let id = match &ast.left.as_ref().unwrap().value {
                    Token::Identifier(id) => id,
                    _ => panic!("CRITICAL: expected identifer on LHS of bind operator"),
                };

                output.vars.push((id.clone(), 4, total_offset + 4));
                total_offset + 4
            }
            _ => total_offset,
        };
        let total_offset = match &ast.left {
            Some(n) => VarTable::find_bound_identifiers(n, output, total_offset),
            None => total_offset,
        };
        let total_offset = match &ast.right {
            Some(n) => VarTable::find_bound_identifiers(n, output, total_offset),
            None => total_offset,
        };
        total_offset
    }

    fn has_duplicates(var_table: &VarTable) -> bool {
        (1..var_table.vars.len()).any(|i| var_table.vars[i..].contains(&var_table.vars[i - 1]))
    }
}
