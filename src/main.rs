#![allow(dead_code)]

fn main() {
    let text = "fn test { x := 1 + 2 * 3 ; return x + 2 ; }";
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
pub enum NodeType {
    Integer(i32),
    Identifier(String),
    Mul(Box<Node>, Box<Node>),
    Add(Box<Node>, Box<Node>),
    Bind(String, Box<Node>),
    Return(Option<Box<Node>>),
    Function,
}

#[derive(Debug)]
pub enum StmtType {}

#[derive(Debug)]
pub struct Node {
    value: NodeType,
    left: NodeOption,
    right: NodeOption,
}

type TokenIter<'a> = std::iter::Peekable<core::slice::Iter<'a, Token>>;
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

    fn function(iter: &mut TokenIter) -> Option<Vec<Node>> {
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

    fn block(iter: &mut TokenIter) -> Vec<Node> {
        let mut stmts = vec![];
        while iter.peek().is_some() {
            match Node::statement(iter) {
                Some(s) => stmts.push(s),
                None => break,
            }
        }
        stmts
    }

    fn return_stmt(iter: &mut TokenIter) -> Option<Node> {
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
                        value: NodeType::Return(Some(Box::new(exp))),
                        left: None,
                        right: None,
                    }),
                    None => Some(Node {
                        value: NodeType::Return(None),
                        left: None,
                        right: None,
                    }),
                }
            }
            _ => None,
        }
    }

    fn statement(iter: &mut TokenIter) -> Option<Node> {
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

    fn bind(iter: &mut TokenIter) -> Option<Node> {
        match Node::identifier(iter) {
            Some(Node {
                value: NodeType::Identifier(id),
                left: None,
                right: None,
            }) => {
                println!("Parse: Binding {:?}", id);
                let pt = iter.peek();
                println!("peek: {:?}", pt);
                match pt {
                    Some(Token::Assign) => {
                        iter.next();
                        let exp = Node::expression(iter).expect("Expected an expression after :=");
                        Some(Node {
                            value: NodeType::Bind(id, Box::new(exp)),
                            left: None,
                            right: None,
                        })
                    }
                    _ => {
                        println!("Expected := after identifer in bind statement");
                        None
                    }
                }
            },
            Some(_) => panic!("Parse: invalid LHS in bind expresion"),
            None => None,
        }
    }

    fn expression(iter: &mut TokenIter) -> Option<Node> {
        match Node::term(iter) {
            Some(n) => match iter.peek() {
                Some(Token::Add) => {
                    iter.next();
                    let n2 = Node::expression(iter).expect("An expression after +");
                    Some(Node {
                        value: NodeType::Add(Box::new(n), Box::new(n2)),
                        left: None,
                        right: None,
                    })
                }
                _ => Some(n),
            },
            None => None,
        }
    }

    fn term(iter: &mut TokenIter) -> Option<Node> {
        match Node::factor(iter) {
            Some(n) => match iter.peek() {
                Some(Token::Mul) => {
                    iter.next();
                    let n2 = Node::term(iter).expect("a valid term after *");
                    Some(Node {
                        value: NodeType::Mul(Box::new(n), Box::new(n2)),
                        left: None,
                        right: None,
                    })
                }
                _ => Some(n),
            },
            None => None,
        }
    }

    fn factor(iter: &mut TokenIter) -> Option<Node> {
        match Node::number(iter) {
            Some(n) => Some(n),
            None => match Node::identifier(iter) {
                Some(n) => Some(n),
                None => None,
            },
        }
    }

    fn identifier(iter: &mut TokenIter) -> Option<Node> {
        println!("Identifier");
        match iter.peek() {
            Some(token) => match token {
                Token::Identifier(id) => {
                    iter.next();
                    Some(Node {
                        value: NodeType::Identifier(id.clone()),
                        left: None,
                        right: None,
                    })
                }
                _ => None,
            },
            None => None,
        }
    }

    fn number(iter: &mut TokenIter) -> Option<Node> {
        match iter.peek() {
            Some(token) => match token {
                Token::Integer(i) => {
                    iter.next();
                    Some(Node {
                        value: NodeType::Integer(*i),
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
            println!("Compile @ {:?}", ast.value);
            match &ast.value {
                super::NodeType::Integer(i) => {
                    output.push(Assembly::Instr(Instr::Mov(
                        Location::Register(Register::Eax),
                        Source::Integer(*i),
                    )));
                    output.push(Assembly::Instr(Instr::Push(Register::Eax)));
                }
                super::NodeType::Identifier(id) => {
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
                super::NodeType::Mul(l, r) => {
                    let left = l.as_ref();
                    Program::traverse(left, vars, output);
                    let right = r.as_ref();
                    Program::traverse(right, vars, output);
                    output.push(Assembly::Instr(Instr::Pop(Register::Ebx)));
                    output.push(Assembly::Instr(Instr::Pop(Register::Eax)));
                    output.push(Assembly::Instr(Instr::IMul(
                        Register::Eax,
                        Location::Register(Register::Ebx),
                    )));
                    output.push(Assembly::Instr(Instr::Push(Register::Eax)));
                }
                super::NodeType::Add(l, r) => {
                    let left = l.as_ref();
                    Program::traverse(left, vars, output);
                    let right = r.as_ref();
                    Program::traverse(right, vars, output);
                    output.push(Assembly::Instr(Instr::Pop(Register::Ebx)));
                    output.push(Assembly::Instr(Instr::Pop(Register::Eax)));
                    output.push(Assembly::Instr(Instr::Add(
                        Register::Eax,
                        Source::Register(Register::Ebx),
                    )));
                    output.push(Assembly::Instr(Instr::Push(Register::Eax)));
                }
                super::NodeType::Bind(id, exp) => {
                    let id_offset = {
                        let var = vars
                            .vars
                            .iter()
                            .find(|v| v.0 == *id)
                            .expect("CRITICAL: identifier not found in var table");
                        var.2
                    };
                    Program::traverse(exp, vars, output);
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
        let total_offset = match &ast.value {
            NodeType::Bind(id, _) => {
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
