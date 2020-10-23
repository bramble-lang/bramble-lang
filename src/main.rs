#![allow(dead_code)]

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

    let program = assembly::Program::compile(&ast, &mut func_table);
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
    Comma,
    Return,
    LParen,
    RParen,
    LBrace,
    RBrace,
    FunctionDef,
    Print,
    Println,
    Init,
    Yield,
    YieldReturn,
    CoroutineDef,
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
                "," => Ok(Token::Comma),
                "(" => Ok(Token::LParen),
                ")" => Ok(Token::RParen),
                "{" => Ok(Token::LBrace),
                "}" => Ok(Token::RBrace),
                "fn" => Ok(Token::FunctionDef),
                "co" => Ok(Token::CoroutineDef),
                "init" => Ok(Token::Init),
                "yield" => Ok(Token::Yield),
                "yret" => Ok(Token::YieldReturn),
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
    FunctionDef(String, Vec<String>, Vec<Node>),
    FunctionCall(String, Vec<Node>),
    CoroutineDef(String, Vec<Node>),
    CoroutineInit(String),
    Yield(Box<Node>),
    YieldReturn(Option<Box<Node>>),
    Module(Vec<Node>, Vec<Node>),
    Print(Box<Node>),
    Println(Box<Node>),
}

type TokenIter<'a> = std::iter::Peekable<core::slice::Iter<'a, Token>>;
impl Node {
    /*
        Grammar
        IDENTIFIER := A-Za-z*
        NUMBER := 0-9*
        FUNCTION_CALL := IDENTIFIER LPAREN EXPRESSION [, EXPRESSION] RPAREN
        YIELD := yield IDENTIFIER
        FACTOR := FUNCTION_CALL | YIELD | NUMBER | IDENTIFIER
        TERM := FACTOR [* TERM]
        EXPRESSION :=  TERM [+ EXPRESSION]
        INIT_CO := init IDENTIFIER
        BIND := IDENTIFIER := (EXPRESSION|INIT_CO)
        PRINTLN := println EXPRESSION ;
        RETURN := return [EXPRESSION] SEMICOLON
        YIELD_RETURN := yield return [EXPRESSION] SEMICOLON
        STATEMENT := [BIND] SEMICOLON
        BLOCK := STATEMENT*
        COBLOCK := [STATEMENT | YIELD_RETURN]*
        FUNCTION := fn IDENTIFIER LPAREN [IDENTIFIER [, IDENTIFIER]*] RPAREN LBRACE BLOCK RETURN RBRACE
        COROUTINE := co IDENTIFIER LPAREN [IDENTIFIER [, IDENTIFIER]*] RPAREN LBRACE COBLOCK RETURN RBRACE
        MODULES := [FUNCTION|COROUTINE]*

        tokenize - takes a string of text and converts it to a string of tokens
        parse - takes a string of tokens and converts it into an AST
        compile - takes an AST and converts it to assembly
    */
    pub fn parse(tokens: Vec<Token>) -> Option<Node> {
        let mut iter = tokens.iter().peekable();
        //Node::function(&mut iter)
        Node::module(&mut iter)
    }

    fn module(iter: &mut TokenIter) -> Option<Node> {
        let mut functions = vec![];
        let mut coroutines = vec![];

        while iter.peek().is_some() {
            match Node::function_def(iter) {
                Some(f) => functions.push(f),
                None => match Node::coroutine_def(iter) {
                    Some(co) => coroutines.push(co),
                    None => break,
                },
            }
        }

        if functions.len() > 0 {
            Some(Node::Module(functions, coroutines))
        } else {
            None
        }
    }

    fn function_def(iter: &mut TokenIter) -> Option<Node> {
        match iter.peek() {
            Some(Token::FunctionDef) => {
                iter.next();
                match iter.peek() {
                    Some(Token::Identifier(id)) => {
                        iter.next();

                        let params = Node::fn_def_params(iter);

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
                                Some(Node::FunctionDef(id.clone(), params, stmts))
                            }
                            _ => panic!("Expected { after function declaration"),
                        }
                    }
                    _ => panic!("Expected function name after fn"),
                }
            }
            _ => None,
        }
    }

    fn coroutine_def(iter: &mut TokenIter) -> Option<Node> {
        println!("Parser @ coroutine_def");
        match iter.peek() {
            Some(Token::CoroutineDef) => {
                iter.next();
                match iter.peek() {
                    Some(Token::Identifier(id)) => {
                        iter.next();

                        let _params = Node::fn_def_params(iter);

                        match iter.peek() {
                            Some(Token::LBrace) => {
                                iter.next();
                                let mut stmts = Node::co_block(iter);

                                match Node::return_stmt(iter) {
                                    Some(ret) => stmts.push(ret),
                                    None => panic!("Coroutine must end with a return statement"),
                                }

                                match iter.peek() {
                                    Some(Token::RBrace) => {
                                        iter.next();
                                    }
                                    _ => panic!("Expected } at end of function definition"),
                                }
                                Some(Node::CoroutineDef(id.clone(), stmts))
                            }
                            _ => panic!("Expected { after function declaration"),
                        }
                    }
                    _ => panic!("Expected function name after fn"),
                }
            }
            _ => None,
        }
    }

    fn fn_def_params(iter: &mut TokenIter) -> Vec<String> {
        match iter.peek() {
            Some(Token::LParen) => {
                iter.next();
            }
            _ => panic!("Parser: expected an ( after function name in function definition"),
        }

        let mut params = vec![];

        while let Some(param) = Node::identifier(iter) {
            match param {
                Node::Identifier(id) => {
                    params.push(id);
                    match iter.peek() {
                        Some(Token::Comma) => {
                            iter.next();
                        }
                        Some(Token::RParen) => break,
                        Some(t) => panic!("Unexpected token in function definition: {:?}", t),
                        None => panic!("Parser: unexpected EOF"),
                    };
                }
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

    fn co_block(iter: &mut TokenIter) -> Vec<Node> {
        let mut stmts = vec![];
        while iter.peek().is_some() {
            match Node::statement(iter) {
                Some(s) => stmts.push(s),
                None => match Node::yield_return_stmt(iter) {
                    Some(s) => stmts.push(s),
                    None => break,
                },
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

    fn yield_return_stmt(iter: &mut TokenIter) -> Option<Node> {
        match iter.peek() {
            Some(Token::YieldReturn) => {
                iter.next();
                let exp = Node::expression(iter);
                match iter.peek() {
                    Some(Token::Semicolon) => iter.next(),
                    _ => panic!("Expected ; after return statement"),
                };
                match exp {
                    Some(exp) => Some(Node::YieldReturn(Some(Box::new(exp)))),
                    None => Some(Node::YieldReturn(None)),
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
                _ => panic!(format!(
                    "Exected ; after statement, found {:?}",
                    iter.peek()
                )),
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
                        match iter.peek() {
                            Some(Token::Init) => {
                                let co_init =
                                    Node::co_init(iter).expect("Parser: Invalid coroutine init");
                                Some(Node::Bind(id, Box::new(co_init)))
                            }
                            _ => {
                                let exp = Node::expression(iter).expect(&format!(
                                    "Expected an expression or coroutine init after :=, found {:?}",
                                    iter.peek()
                                ));
                                Some(Node::Bind(id, Box::new(exp)))
                            }
                        }
                    }
                    _ => {
                        println!("Expected := after identifer in bind statement");
                        None
                    }
                }
            }
            Some(_) => panic!("Parser: invalid LHS in bind expresion"),
            None => None,
        }
    }

    fn co_init(iter: &mut TokenIter) -> Option<Node> {
        match iter.peek() {
            Some(Token::Init) => {
                iter.next();
                match iter.peek() {
                    Some(Token::Identifier(id)) => {
                        iter.next();
                        Some(Node::CoroutineInit(id.clone()))
                    }
                    _ => {
                        panic!("Parser: expected identifier after init");
                    }
                }
            }
            _ => None,
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
            None => match Node::function_call_or_variable(iter) {
                Some(n) => Some(n),
                None => match Node::co_yield(iter) {
                    Some(n) => Some(n),
                    None => None,
                },
            },
        }
    }

    fn function_call_or_variable(iter: &mut TokenIter) -> Option<Node> {
        println!("Function call");
        match Node::identifier(iter) {
            Some(Node::Identifier(id)) => match Node::fn_call_params(iter) {
                Some(params) => {
                    // this is a function call
                    Some(Node::FunctionCall(id, params))
                }
                _ => Some(Node::Identifier(id)),
            },
            Some(_) => panic!("Parser: expected identifier"),
            None => None,
        }
    }

    /// LPAREN [EXPRESSION [, EXPRESSION]*] RPAREN
    fn fn_call_params(iter: &mut TokenIter) -> Option<Vec<Node>> {
        println!("Parser @ func call params");
        match iter.peek() {
            Some(Token::LParen) => {
                // this is a function call
                iter.next();

                let mut params = vec![];
                while let Some(param) = Node::expression(iter) {
                    match param {
                        exp => {
                            params.push(exp);
                            match iter.peek() {
                                Some(Token::Comma) => {
                                    iter.next();
                                }
                                Some(Token::RParen) => break,
                                Some(t) => panic!("Unexpected token in function call: {:?}", t),
                                None => panic!("Parser: unexpected EOF"),
                            };
                        }
                        _ => panic!("Parser: invalid parameter in function call"),
                    }
                }

                match iter.peek() {
                    Some(Token::RParen) => {
                        iter.next();
                    }
                    _ => panic!("Parser: expected ) after function call"),
                }
                Some(params)
            }
            _ => None,
        }
    }

    fn co_yield(iter: &mut TokenIter) -> Option<Node> {
        println!("Identifier");
        match iter.peek() {
            Some(Token::Yield) => {
                iter.next();
                match Node::identifier(iter) {
                    Some(id) => Some(Node::Yield(Box::new(id))),
                    _ => panic!("Parser: expected an identifier after yield"),
                }
            }
            _ => None,
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
    #[derive(Debug, Copy, Clone)]
    enum Register {
        Eax,
        Ebx,
        Ecx,
        Edx,
        Ebp,
        Esp,
    }

    impl std::fmt::Display for Register {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Register::Eax => f.write_str("eax"),
                Register::Ebx => f.write_str("ebx"),
                Register::Ecx => f.write_str("ecx"),
                Register::Edx => f.write_str("edx"),
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
        Address(String),
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
                Source::Address(m) => {
                    let s = format!("{}", m);
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
        Lea(Location, Source),
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
        Include(String),
        Section(String),
        Global(String),
        Data(String, i32),
    }

    pub struct Program {
        code: Vec<Assembly>,
    }

    impl Program {
        pub fn print(&self) {
            for inst in self.code.iter() {
                match inst {
                    Assembly::Include(include) => {
                        println!("%include \"{}\"", include);
                    }
                    Assembly::Section(section) => {
                        println!("\nsection {}", section);
                    }
                    Assembly::Global(global) => {
                        println!("global {}", global);
                    }
                    Assembly::Data(label, value) => {
                        println!("{}: dd {}", label, value);
                    }
                    Assembly::Label(label) => {
                        if !label.starts_with(".") {
                            println!();
                        }
                        println!("{}:", label);
                    }
                    Assembly::Instr(inst) => {
                        print!("    ");
                        match inst {
                            Instr::Mov(l, Source::Memory(s)) => {
                                println!("mov {}, DWORD [{}]", l, s)
                            }
                            Instr::Mov(l, Source::Address(s)) => println!("mov {}, {}", l, s),
                            Instr::Mov(l, Source::Integer(s)) => println!("mov {}, DWORD {}", l, s),
                            Instr::Mov(l, Source::Register(s)) => println!("mov {}, {}", l, s),
                            Instr::Lea(l, s) => println!("lea {}, {}", l, s),
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
                            Instr::Jmp(loc) => {
                                println!("jmp {}", loc);
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

        pub fn compile(ast: &super::Node, funcs: &mut super::FunctionTable) -> Program {
            let mut code = vec![];

            Program::create_base(&mut code);

            Program::coroutine_init("next_stack_addr".into(), 2 * 1024, &mut code);
            Program::runtime_yield_into_coroutine(&mut code);
            Program::runtime_yield_return(&mut code);

            // Put user code here
            let global_func = "".into();
            Program::traverse(ast, &global_func, funcs, &mut code);
            Program { code }
        }

        /// Creates the runtime code that will manage the entire execution of this program.
        fn create_base(output: &mut Vec<Assembly>) {
            // %include "io.inc"
            // section .data
            // next_stack_addr dd 0
            // stack_size 2048
            output.push(Assembly::Include("io.inc".into()));
            output.push(Assembly::Section(".data".into()));
            output.push(Assembly::Data("next_stack_addr".into(), 0));
            output.push(Assembly::Data("stack_size".into(), 2 * 1024));

            // section .text
            // global CMAIN
            // CMAIN
            output.push(Assembly::Section(".text".into()));
            output.push(Assembly::Global("CMAIN".into()));
            output.push(Assembly::Label("CMAIN".into()));

            // Setup stack frame for the base/runtime layer
            // this will create any runtime administrative logic
            // and also call the users `main` function.
            output.push(Assembly::Instr(Instr::Push(Register::Ebp)));
            output.push(Assembly::Instr(Instr::Mov(
                Location::Register(Register::Ebp),
                Source::Register(Register::Esp),
            )));

            output.push(Assembly::Instr(Instr::Mov(
                Location::Register(Register::Eax),
                Source::Register(Register::Esp),
            )));
            output.push(Assembly::Instr(Instr::Sub(
                Register::Eax,
                Source::Integer(4096),
            )));
            output.push(Assembly::Instr(Instr::Mov(
                Location::Memory("next_stack_addr".into()),
                Source::Register(Register::Eax),
            )));

            // Call main function
            output.push(Assembly::Instr(Instr::Call("my_main".into())));

            // Clean up frame before exiting program
            output.push(Assembly::Instr(Instr::Mov(
                Location::Register(Register::Esp),
                Source::Register(Register::Ebp),
            )));
            output.push(Assembly::Instr(Instr::Pop(Register::Ebp)));
            output.push(Assembly::Instr(Instr::Ret));
        }

        /// Writes the function which will handle initializing a new coroutine
        fn coroutine_init(ns: String, sinc: i32, output: &mut Vec<Assembly>) {
            /*
             * Input:
             * EAX - address of the coroutine's instructions
             * EBX - EnX - initialization parameters for the coroutine
             *
             * Output:
             * EAX - address of the new coroutine instance
             *
             * Parameter: the IP to the coroutine's actual code.  Along with any init parameters
             * to be passed to the coroutine.
             *
             * Returns a pointer to the new coroutine stack (which contains the coroutine's
             * metadata)
             *
             * use the next stack address variable as the location for the new coroutine, then
             * increment the next stack address.
             *
             * Store the entry address
             * Compute the initial stack frame: allocating space for meta data, to store the
             * esp/ebp, and to store initial parameters.
             *
             * Store esp/ebp
             * Store the initial parameters.
             *
             * Move the address of the stack into EAX and return
             */

            // Create new stack frame
            output.push(Assembly::Label("runtime_init_coroutine".into()));
            output.push(Assembly::Instr(Instr::Push(Register::Ebp)));
            output.push(Assembly::Instr(Instr::Mov(
                Location::Register(Register::Ebp),
                Source::Register(Register::Esp),
            )));

            // Create coroutine's stack
            output.push(Assembly::Instr(Instr::Mov(
                Location::Register(Register::Esp),
                Source::Memory(ns.clone()),
            )));

            output.push(Assembly::Instr(Instr::Mov(
                Location::Memory(format!("{}-4", Register::Esp)),
                Source::Register(Register::Eax),
            ))); // Store the coroutine's current next instruction to execute
            output.push(Assembly::Instr(Instr::Mov(
                Location::Memory(format!("{}-8", Register::Esp)),
                Source::Integer(0),
            ))); // store the return ESP
            output.push(Assembly::Instr(Instr::Mov(
                Location::Memory(format!("{}-12", Register::Esp)),
                Source::Integer(0),
            ))); // store the return EBP
            output.push(Assembly::Instr(Instr::Mov(
                Location::Memory(format!("{}-16", Register::Esp)),
                Source::Integer(0),
            ))); // store the return Instruction address

            output.push(Assembly::Instr(Instr::Lea(
                Location::Register(Register::Eax),
                Source::Memory(format!("{}-20", Register::Esp)),
            )));
            output.push(Assembly::Instr(Instr::Mov(
                Location::Memory(format!("{}-20", Register::Esp)),
                Source::Register(Register::Eax),
            )));

            // Move satck address into EAX for return
            output.push(Assembly::Instr(Instr::Mov(
                Location::Register(Register::Eax),
                Source::Register(Register::Esp),
            )));

            output.push(Assembly::Instr(Instr::Sub(
                Register::Esp,
                Source::Integer(sinc),
            )));
            output.push(Assembly::Instr(Instr::Mov(
                Location::Memory(ns.clone()),
                Source::Register(Register::Esp),
            )));

            // clean up stack frame
            output.push(Assembly::Instr(Instr::Mov(
                Location::Register(Register::Esp),
                Source::Register(Register::Ebp),
            )));
            output.push(Assembly::Instr(Instr::Pop(Register::Ebp)));
            output.push(Assembly::Instr(Instr::Ret));
        }

        fn runtime_yield_into_coroutine(output: &mut Vec<Assembly>) {
            /*
             * Input:
             * EAX - address of the coroutine instance
             * EBX - address of the return point
             */
            output.push(Assembly::Label("runtime_yield_into_coroutine".into()));

            // mov ESP into metadata (return ESP)
            // mov EBP into metadata (return EBP)
            // mov return address into metadata (return address)
            output.push(Assembly::Instr(Instr::Mov(
                Location::Memory(format!("{}-8", Register::Eax)),
                Source::Register(Register::Esp),
            ))); // store the return ESP
            output.push(Assembly::Instr(Instr::Mov(
                Location::Memory(format!("{}-12", Register::Eax)),
                Source::Register(Register::Ebp),
            ))); // store the return EBP
            output.push(Assembly::Instr(Instr::Mov(
                Location::Memory(format!("{}-16", Register::Eax)),
                Source::Register(Register::Ebx),
            ))); // store the return Instruction address

            // Load the address of the coroutine into EBP (the base of the stack frame)
            output.push(Assembly::Instr(Instr::Mov(
                Location::Register(Register::Ebp),
                Source::Register(Register::Eax),
            )));

            // Load the coroutines current stack location into ESP
            output.push(Assembly::Instr(Instr::Mov(
                Location::Register(Register::Esp),
                Source::Memory(format!("{}-20", Register::Ebp)),
            )));

            // Re/enter the coroutine
            output.push(Assembly::Instr(Instr::Jmp(format!(
                "{}",
                Source::Memory(format!("{}-4", Register::Ebp))
            ))));
        }

        fn runtime_yield_return(output: &mut Vec<Assembly>) {
            /*
             * Input:
             * EAX - value being returned (if any)
             * EBX - re-entry address
             */
            // When in a coroutine, return to the calling coroutine
            output.push(Assembly::Label("runtime_yield_return".into()));

            // Store the current ESP into metadata
            output.push(Assembly::Instr(Instr::Mov(
                Location::Memory(format!("{}-20", Register::Ebp)),
                Source::Register(Register::Esp),
            )));
            // Store the re-entry address into metadata
            output.push(Assembly::Instr(Instr::Mov(
                Location::Memory(format!("{}-4", Register::Ebp)),
                Source::Register(Register::Ebx),
            )));

            // Get the return ESP from metadata
            output.push(Assembly::Instr(Instr::Mov(
                Location::Register(Register::Esp),
                Source::Memory(format!("{}-8", Register::Ebp)),
            )));
            // Get the return address from metadata
            output.push(Assembly::Instr(Instr::Mov(
                Location::Register(Register::Ebx),
                Source::Memory(format!("{}-16", Register::Ebp)),
            )));
            // Get the return EBP from metadata
            output.push(Assembly::Instr(Instr::Mov(
                Location::Register(Register::Ebp),
                Source::Memory(format!("{}-12", Register::Ebp)),
            )));

            // jmp to the return address
            output.push(Assembly::Instr(Instr::Jmp(format!(
                "{}",
                Source::Register(Register::Ebx)
            ))));
        }

        fn traverse(
            ast: &super::Node,
            current_func: &String,
            function_table: &mut super::FunctionTable,
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
                        let var = function_table.funcs[current_func]
                            .vars
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
                    Program::traverse(left, current_func, function_table, output);
                    output.push(Assembly::Instr(Instr::Push(Register::Eax)));
                    let right = r.as_ref();
                    Program::traverse(right, current_func, function_table, output);
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
                    Program::traverse(left, current_func, function_table, output);
                    output.push(Assembly::Instr(Instr::Push(Register::Eax)));
                    let right = r.as_ref();
                    Program::traverse(right, current_func, function_table, output);
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
                        let var = function_table.funcs[current_func]
                            .vars
                            .vars
                            .iter()
                            .find(|v| v.0 == *id)
                            .expect("CRITICAL: identifier not found in var table");
                        var.2
                    };
                    Program::traverse(exp, current_func, function_table, output);
                    output.push(Assembly::Instr(Instr::Mov(
                        Location::Memory(format!("ebp-{}", id_offset)),
                        Source::Register(Register::Eax),
                    )));
                }
                super::Node::FunctionDef(fn_name, params, stmts) => {
                    output.push(Assembly::Label(fn_name.clone()));

                    // Prepare stack frame for this function
                    output.push(Assembly::Instr(Instr::Push(Register::Ebp)));
                    output.push(Assembly::Instr(Instr::Mov(
                        Location::Register(Register::Ebp),
                        Source::Register(Register::Esp),
                    )));
                    let total_offset = function_table.funcs[fn_name].vars.vars.last().unwrap().2;
                    output.push(Assembly::Instr(Instr::Sub(
                        Register::Esp,
                        Source::Integer(total_offset),
                    )));

                    // Move function parameters from registers into the stack frame
                    let param_registers =
                        [Register::Eax, Register::Ebx, Register::Ecx, Register::Edx];
                    if params.len() > param_registers.len() {
                        panic!("Compiler: too many parameters in function definition");
                    }
                    for (param, reg) in params.iter().zip(param_registers.iter()) {
                        let param_offset = function_table.funcs[fn_name]
                            .vars
                            .vars
                            .iter()
                            .find(|(id, _, _)| id == param)
                            .unwrap()
                            .2;
                        output.push(Assembly::Instr(Instr::Mov(
                            Location::Memory(format!("ebp-{}", param_offset)),
                            Source::Register(*reg),
                        )));
                    }

                    for s in stmts.iter() {
                        Program::traverse(s, fn_name, function_table, output);
                    }

                    // Clean up frame before exiting program
                    output.push(Assembly::Instr(Instr::Mov(
                        Location::Register(Register::Esp),
                        Source::Register(Register::Ebp),
                    )));
                    output.push(Assembly::Instr(Instr::Pop(Register::Ebp)));

                    output.push(Assembly::Instr(Instr::Ret));
                }
                super::Node::CoroutineDef(fn_name, stmts) => {
                    output.push(Assembly::Label(fn_name.clone()));

                    // Prepare stack frame for this function
                    for s in stmts.iter() {
                        Program::traverse(s, fn_name, function_table, output);
                    }
                    output.push(Assembly::Instr(Instr::Jmp("runtime_yield_return".into())))
                }
                super::Node::Module(functions, coroutines) => {
                    for f in functions.iter() {
                        Program::traverse(f, current_func, function_table, output);
                    }
                    for co in coroutines.iter() {
                        Program::traverse(co, current_func, function_table, output);
                    }
                }
                super::Node::Return(exp) => match exp {
                    Some(e) => Program::traverse(e, current_func, function_table, output),
                    None => (),
                },
                super::Node::CoroutineInit(id) => {
                    output.push(Assembly::Instr(Instr::Lea(
                        Location::Register(Register::Eax),
                        Source::Memory(format!("{}", id)),
                    )));
                    output.push(Assembly::Instr(Instr::Call(
                        "runtime_init_coroutine".into(),
                    )))
                }
                super::Node::Yield(id) => {
                    Program::traverse(id, current_func, function_table, output);
                    function_table
                        .funcs
                        .entry(current_func.clone())
                        .and_modify(|fi| fi.label_count += 1);
                    let ret_lbl =
                        format!(".lbl_{}", function_table.funcs[current_func].label_count);
                    output.push(Assembly::Instr(Instr::Mov(
                        Location::Register(Register::Ebx),
                        Source::Address(ret_lbl.clone()),
                    )));
                    output.push(Assembly::Instr(Instr::Jmp(
                        "runtime_yield_into_coroutine".into(),
                    )));
                    output.push(Assembly::Label(ret_lbl.clone()));
                }
                super::Node::YieldReturn(exp) => {
                    if let Some(exp) = exp {
                        Program::traverse(exp, current_func, function_table, output);
                    }
                    output.push(Assembly::Instr(Instr::Jmp("runtime_yield_return".into())))
                }
                super::Node::Println(exp) => {
                    Program::traverse(exp, current_func, function_table, output);
                    output.push(Assembly::Instr(Instr::Print(Source::Register(
                        Register::Eax,
                    ))));
                    output.push(Assembly::Instr(Instr::Newline));
                }
                super::Node::FunctionCall(fn_name, params) => {
                    // Check if function exists and if the right number of parameters are being
                    // passed
                    if !function_table.funcs.contains_key(fn_name) {
                        panic!("Compiler: no definition found for function `{}`", fn_name);
                    }

                    let expected_num_params = function_table.funcs[fn_name].params.len();
                    let got_num_params = params.len();
                    if expected_num_params != got_num_params {
                        panic!(
                            "Compiler: expected {} but got {} parameters for function `{}`",
                            expected_num_params, got_num_params, fn_name
                        );
                    }

                    // evaluate each paramater then store in registers Eax, Ebx, Ecx, Edx before
                    // calling the function
                    let param_registers =
                        [Register::Eax, Register::Ebx, Register::Ecx, Register::Edx];
                    if params.len() > param_registers.len() {
                        panic!("Compiler: too many parameters being passed to function");
                    }
                    for param in params.iter() {
                        Program::traverse(param, current_func, function_table, output);
                        output.push(Assembly::Instr(Instr::Push(Register::Eax)));
                    }
                    for reg in param_registers.iter().take(params.len()).rev() {
                        output.push(Assembly::Instr(Instr::Pop(*reg)));
                    }
                    output.push(Assembly::Instr(Instr::Call(fn_name.clone())));
                }
                node => println!("Expected an operator, found {:?}", node),
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
            Node::FunctionDef(_, params, stmts) => {
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
            Node::Bind(id, _) => {
                output.vars.push((id.clone(), 4, total_offset + 4));
                total_offset + 4
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
    funcs: std::collections::HashMap<String, FunctionInfo>,
}

#[derive(Debug)]
pub struct FunctionInfo {
    params: Vec<String>,
    vars: VarTable,
    label_count: u32,
}

impl FunctionTable {
    pub fn generate(ast: &Node) -> FunctionTable {
        let mut ft = FunctionTable {
            funcs: std::collections::HashMap::new(),
        };

        match ast {
            Node::Module(functions, coroutines) => {
                for f in functions.iter() {
                    FunctionTable::traverse(f, &mut ft);
                }
                for co in coroutines.iter() {
                    FunctionTable::traverse(co, &mut ft);
                }
            }
            _ => panic!("Type analysis: expected Module at root level of the AST"),
        }

        ft
    }

    fn traverse(ast: &Node, ft: &mut FunctionTable) {
        match ast {
            Node::FunctionDef(fn_name, params, _) => {
                let vars = VarTable::generate(ast);
                ft.funcs.insert(
                    fn_name.clone(),
                    FunctionInfo {
                        params: params.clone(),
                        vars,
                        label_count: 0,
                    },
                );
            }
            Node::CoroutineDef(fn_name, _) => {
                let vars = VarTable::generate(ast);
                ft.funcs.insert(
                    fn_name.clone(),
                    FunctionInfo {
                        params: vec![],
                        vars,
                        label_count: 0,
                    },
                );
            }
            _ => panic!("Type analysis: invalid function"),
        }
    }
}
