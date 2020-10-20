#![allow(dead_code)]

fn main() {
    let text = "fn main ( p ) { x := 1 + 2 * 3 ; println x ; return x + p ; }";
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
    let func_table = FunctionTable::generate(&ast);
    println!("FuncTable: {:?}", func_table);

    let program = assembly::Program::compile(&ast, &func_table);
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
    Print,
    Println,
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
                "print" => Ok(Token::Print),
                "println" => Ok(Token::Println),
                s if s.is_ascii() => Ok(Token::Identifier(s.into())),
                _ => Err("Invalid token"),
            },
        })
        .collect()
    }
}
// AST - a type(s) which is used to construct an AST representing the logic of the
// program
// Each type of node represents an expression and the only requirement is that at the
// end of computing an expression its result is in EAX
#[derive(Debug)]
pub enum Node {
    Integer(i32),
    Identifier(String),
    Mul(Box<Node>, Box<Node>),
    Add(Box<Node>, Box<Node>),
    Bind(String, Box<Node>),
    Return(Option<Box<Node>>),
    Function(String, Vec<String>, Vec<Node>),
    Print(Box<Node>),
    Println(Box<Node>),
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
        PRINTLN := println EXPRESSION ;
        RETURN := return [EXPRESSION] SEMICOLON
        STATEMENT := [BIND] SEMICOLON
        BLOCK := STATEMENT*
        FUNCTION := fn IDENTIFIER LPAREN [IDENTIFIER [, IDENTIFIER]*] RPAREN LBRACE BLOCK RETURN RBRACE

        tokenize - takes a string of text and converts it to a string of tokens
        parse - takes a string of tokens and converts it into an AST
        compile - takes an AST and converts it to assembly
    */
    pub fn parse(tokens: Vec<Token>) -> Option<Node> {
        let mut iter = tokens.iter().peekable();
        Node::function(&mut iter)
    }

    fn function(iter: &mut TokenIter) -> Option<Node> {
        match iter.peek() {
            Some(Token::Function) => {
                iter.next();
                match iter.peek() {
                    Some(Token::Identifier(id)) => {
                        iter.next();

                        let params = Node::fn_params(iter);

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
                                Some(Node::Function(id.clone(), params, stmts))
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

    fn fn_params(iter: &mut TokenIter) -> Vec<String> {
        match iter.peek() {
            Some(Token::LParen) => {
                iter.next();
            }
            _ => panic!("Parser: expected an ( after function name in function definition"),
        }

        let mut params = vec![];

        while let Some(param) = Node::identifier(iter) {
            match param {
                Node::Identifier(id) => params.push(id),
                _ => panic!("Parser: invalid parameter declaration in function definition"),
            }
        }

        match iter.peek() {
            Some(Token::RParen) => {
                iter.next();
            }
            _ => panic!("Parser: expected )"),
        }

        params
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
                    Some(exp) => Some(Node::Return(Some(Box::new(exp)))),
                    None => Some(Node::Return(None)),
                }
            }
            _ => None,
        }
    }

    fn statement(iter: &mut TokenIter) -> Option<Node> {
        let stm = match Node::bind(iter) {
            Some(b) => Some(b),
            None => match Node::println_stmt(iter) {
                Some(p) => Some(p),
                _ => None,
            },
        };

        if stm.is_some() {
            match iter.peek() {
                Some(Token::Semicolon) => {
                    iter.next();
                }
                _ => panic!("Exected ; after statement"),
            }
        }

        stm
    }

    fn println_stmt(iter: &mut TokenIter) -> Option<Node> {
        let tk = iter.peek();
        println!("Parser @ {:?}", tk);
        match tk {
            Some(Token::Println) => {
                iter.next();
                let exp = Node::expression(iter);
                match exp {
                    Some(exp) => Some(Node::Println(Box::new(exp))),
                    None => panic!("Parser: Expected expression after println"),
                }
            }
            _ => None,
        }
    }

    fn bind(iter: &mut TokenIter) -> Option<Node> {
        match Node::identifier(iter) {
            Some(Node::Identifier(id)) => {
                println!("Parse: Binding {:?}", id);
                let pt = iter.peek();
                println!("peek: {:?}", pt);
                match pt {
                    Some(Token::Assign) => {
                        iter.next();
                        let exp = Node::expression(iter).expect("Expected an expression after :=");
                        Some(Node::Bind(id, Box::new(exp)))
                    }
                    _ => {
                        println!("Expected := after identifer in bind statement");
                        None
                    }
                }
            }
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
                    Some(Node::Add(Box::new(n), Box::new(n2)))
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
                    Some(Node::Mul(Box::new(n), Box::new(n2)))
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
                    Some(Node::Identifier(id.clone()))
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
                    Some(Node::Integer(*i))
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
        Call(Label),
        Ret,
        Mov(Location, Source),
        Add(Register, Source),
        Sub(Register, Source),
        IMul(Register, Location),
        Push(Register),
        Pop(Register),
        Print(Source),
        Newline,
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
                    Assembly::Label(label) => {
                        println!("\n{}:", label);
                    }
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
                            Instr::Call(label) => println!("call {}", label),
                            Instr::Ret => println!("ret"),
                            Instr::Print(s) => {
                                println!("PRINT_DEC 4, {}", s);
                            }
                            Instr::Newline => {
                                println!("NEWLINE");
                            }
                            _ => {
                                println!("{:?}", inst);
                            }
                        }
                    }
                }
            }
        }

        pub fn compile(ast: &super::Node, funcs: &super::FunctionTable) -> Program {
            let mut code = vec![];

            // Setup stack frame for the base/runtime layer
            // this will create any runtime administrative logic
            // and also call the users `main` function.
            code.push(Assembly::Instr(Instr::Push(Register::Ebp)));
            code.push(Assembly::Instr(Instr::Mov(
                Location::Register(Register::Ebp),
                Source::Register(Register::Esp),
            )));

            // Call main function
            code.push(Assembly::Instr(Instr::Call("main".into())));

            // Clean up frame before exiting program
            code.push(Assembly::Instr(Instr::Mov(
                Location::Register(Register::Esp),
                Source::Register(Register::Ebp),
            )));
            code.push(Assembly::Instr(Instr::Pop(Register::Ebp)));
            code.push(Assembly::Instr(Instr::Ret));

            // Put user code here
            let global_func = "".into();
            Program::traverse(ast, &global_func, funcs, &mut code);
            Program { code }
        }

        fn traverse(
            ast: &super::Node,
            current_func: &String,
            funcs: &super::FunctionTable,
            output: &mut Vec<Assembly>,
        ) {
            println!("Compile @ {:?}", ast);
            match ast {
                super::Node::Integer(i) => {
                    output.push(Assembly::Instr(Instr::Mov(
                        Location::Register(Register::Eax),
                        Source::Integer(*i),
                    )));
                }
                super::Node::Identifier(id) => {
                    let id_offset = {
                        let var = funcs.funcs[current_func]
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
                }
                super::Node::Mul(l, r) => {
                    let left = l.as_ref();
                    Program::traverse(left, current_func, funcs, output);
                    output.push(Assembly::Instr(Instr::Push(Register::Eax)));
                    let right = r.as_ref();
                    Program::traverse(right, current_func, funcs, output);
                    output.push(Assembly::Instr(Instr::Push(Register::Eax)));

                    output.push(Assembly::Instr(Instr::Pop(Register::Ebx)));
                    output.push(Assembly::Instr(Instr::Pop(Register::Eax)));
                    output.push(Assembly::Instr(Instr::IMul(
                        Register::Eax,
                        Location::Register(Register::Ebx),
                    )));
                }
                super::Node::Add(l, r) => {
                    let left = l.as_ref();
                    Program::traverse(left, current_func, funcs, output);
                    output.push(Assembly::Instr(Instr::Push(Register::Eax)));
                    let right = r.as_ref();
                    Program::traverse(right, current_func, funcs, output);
                    output.push(Assembly::Instr(Instr::Push(Register::Eax)));

                    output.push(Assembly::Instr(Instr::Pop(Register::Ebx)));
                    output.push(Assembly::Instr(Instr::Pop(Register::Eax)));
                    output.push(Assembly::Instr(Instr::Add(
                        Register::Eax,
                        Source::Register(Register::Ebx),
                    )));
                }
                super::Node::Bind(id, exp) => {
                    let id_offset = {
                        let var = funcs.funcs[current_func]
                            .vars
                            .iter()
                            .find(|v| v.0 == *id)
                            .expect("CRITICAL: identifier not found in var table");
                        var.2
                    };
                    Program::traverse(exp, current_func, funcs, output);
                    output.push(Assembly::Instr(Instr::Mov(
                        Location::Memory(format!("ebp-{}", id_offset)),
                        Source::Register(Register::Eax),
                    )));
                }
                super::Node::Function(fn_name, _, stmts) => {
                    output.push(Assembly::Label(fn_name.clone()));

                    // Prepare stack frame for this function
                    output.push(Assembly::Instr(Instr::Push(Register::Ebp)));
                    output.push(Assembly::Instr(Instr::Mov(
                        Location::Register(Register::Ebp),
                        Source::Register(Register::Esp),
                    )));
                    let total_offset = funcs.funcs[fn_name].vars.last().unwrap().2;
                    output.push(Assembly::Instr(Instr::Sub(
                        Register::Esp,
                        Source::Integer(total_offset),
                    )));

                    for s in stmts.iter() {
                        Program::traverse(s, fn_name, funcs, output);
                    }

                    // Clean up frame before exiting program
                    output.push(Assembly::Instr(Instr::Mov(
                        Location::Register(Register::Esp),
                        Source::Register(Register::Ebp),
                    )));
                    output.push(Assembly::Instr(Instr::Pop(Register::Ebp)));

                    output.push(Assembly::Instr(Instr::Ret));
                }
                super::Node::Return(exp) => match exp {
                    Some(e) => Program::traverse(e, current_func, funcs, output),
                    None => (),
                },
                super::Node::Println(exp) => {
                    Program::traverse(exp, current_func, funcs, output);
                    output.push(Assembly::Instr(Instr::Print(Source::Register(Register::Eax))));
                    output.push(Assembly::Instr(Instr::Newline));
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
    pub fn generate(ast: &Node) -> VarTable {
        let mut vt = VarTable { vars: vec![] };
        let mut offset = 0;
        match ast {
            Node::Function(_, params, stmts) => {
                for p in params.iter() {
                    offset += 4;
                    vt.vars.push((p.clone(), 4, offset));
                }

                for n in stmts.iter() {
                    offset = VarTable::find_bound_identifiers(n, &mut vt, offset);
                }
            }
            _ => {}
        }
        if VarTable::has_duplicates(&vt) {
            panic!("An identifier was defined twice");
        }
        vt
    }

    fn find_bound_identifiers(ast: &Node, output: &mut VarTable, total_offset: i32) -> i32 {
        match ast {
            Node::Bind(id, exp) => {
                output.vars.push((id.clone(), 4, total_offset + 4));
                total_offset + 4 + VarTable::find_bound_identifiers(exp, output, total_offset)
            }
            _ => total_offset,
        }
    }

    fn has_duplicates(var_table: &VarTable) -> bool {
        (1..var_table.vars.len()).any(|i| var_table.vars[i..].contains(&var_table.vars[i - 1]))
    }
}

#[derive(Debug)]
pub struct FunctionTable {
    funcs: std::collections::HashMap<String, VarTable>,
}

impl FunctionTable {
    pub fn generate(ast: &Node) -> FunctionTable {
        let mut ft = FunctionTable {
            funcs: std::collections::HashMap::new(),
        };

        FunctionTable::traverse(ast, &mut ft);

        ft
    }

    fn traverse(ast: &Node, ft: &mut FunctionTable) {
        match ast {
            Node::Function(fn_name, _, _) => {
                let vt = VarTable::generate(ast);
                ft.funcs.insert(fn_name.clone(), vt);
            }
            _ => panic!("Type analysis: invalid function"),
        }
    }
}
