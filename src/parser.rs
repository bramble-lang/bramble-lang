// AST - a type(s) which is used to construct an AST representing the logic of the
// program
// Each type of node represents an expression and the only requirement is that at the
// end of computing an expression its result is in EAX
use crate::lexer::{self, Lex};
use crate::Token;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Primitive {
    I32,
    Bool,
    Unit,
    Unknown,
}

#[derive(Debug)]
pub enum Ast {
    Integer(i32),
    Boolean(bool),
    Identifier(String, Primitive),
    Primitive(Primitive),
    Mul(Box<Ast>, Box<Ast>),
    Add(Box<Ast>, Box<Ast>),
    BAnd(Box<Ast>, Box<Ast>),
    BOr(Box<Ast>, Box<Ast>),
    Gr(Box<Ast>, Box<Ast>),
    GrEq(Box<Ast>, Box<Ast>),
    Ls(Box<Ast>, Box<Ast>),
    LsEq(Box<Ast>, Box<Ast>),
    Eq(Box<Ast>, Box<Ast>),
    NEq(Box<Ast>, Box<Ast>),
    Bind(String, Primitive, Box<Ast>),
    Return(Option<Box<Ast>>),
    FunctionDef(String, Vec<(String, Primitive)>, Primitive, Vec<Ast>),
    FunctionCall(String, Vec<Ast>),
    CoroutineDef(String, Vec<(String, Primitive)>, Primitive, Vec<Ast>),
    CoroutineInit(String, Vec<Ast>),
    Yield(Box<Ast>),
    YieldReturn(Option<Box<Ast>>),
    If(Box<Ast>, Box<Ast>, Box<Ast>),
    Module(Vec<Ast>, Vec<Ast>),
    Printi(Box<Ast>),
    Printiln(Box<Ast>),
    Printbln(Box<Ast>),
}

#[derive(Debug)]
pub struct AstNode {
    l: u32,
    n: Ast,
}

impl AstNode {
    pub fn new(l: u32, n: Ast) -> AstNode {
        AstNode {
            l, n,
        }
    }
}

type TokenIter<'a> = std::iter::Peekable<core::slice::Iter<'a, Token>>;
pub struct Parser {
}
impl Parser {
    /*
        Grammar
        PRIMITIVE := i32 | bool
        IDENTIFIER := A-Za-z*
        ID_DEC := IDENTIFIER COLON PRIMITIVE
        NUMBER := 0-9*
        FUNCTION_CALL := IDENTIFIER LPAREN EXPRESSION [, EXPRESSION] RPAREN
        YIELD := yield IDENTIFIER
        IF := if EXPRESSION LBRACE EXPRESSION RBRACE else LBRACE EXPRESSION RBRACE
        FACTOR := FUNCTION_CALL | YIELD | NUMBER | IDENTIFIER | IF
        TERM := FACTOR [* TERM]
        EXPRESSION :=  TERM [+ EXPRESSION]
        INIT_CO := init IDENTIFIER
        BIND := ID_DEC := (EXPRESSION|INIT_CO)
        PRINTLN := println EXPRESSION ;
        RETURN := return [EXPRESSION] SEMICOLON
        YIELD_RETURN := yield return [EXPRESSION] SEMICOLON
        STATEMENT := [BIND] SEMICOLON
        BLOCK := STATEMENT*
        COBLOCK := [STATEMENT | YIELD_RETURN]*
        FUNCTION := fn IDENTIFIER LPAREN [ID_DEC [, ID_DEC]*] RPAREN  [LARROW PRIMITIVE] LBRACE BLOCK RETURN RBRACE
        COROUTINE := co IDENTIFIER LPAREN [ID_DEC [, ID_DEC]*] RPAREN [LARROW PRIMITIVE] LBRACE COBLOCK RETURN RBRACE
        MODULES := [FUNCTION|COROUTINE]*

        tokenize - takes a string of text and converts it to a string of tokens
        parse - takes a string of tokens and converts it into an AST
        compile - takes an AST and converts it to assembly
    */
    pub fn parse(tokens: Vec<Token>) -> Option<Ast> {
        let mut iter = tokens.iter().peekable();
        //Node::function(&mut iter)
        Parser::module(&mut iter)
    }

    fn module(iter: &mut TokenIter) -> Option<Ast> {
        let mut functions = vec![];
        let mut coroutines = vec![];

        while iter.peek().is_some() {
            match Parser::function_def(iter) {
                Some(f) => functions.push(f),
                None => match Parser::coroutine_def(iter) {
                    Some(co) => coroutines.push(co),
                    None => break,
                },
            }
        }

        if functions.len() > 0 {
            Some(Ast::Module(functions, coroutines))
        } else {
            None
        }
    }

    fn function_def(iter: &mut TokenIter) -> Option<Ast> {
        match iter.peek() {
            Some(Token{l:_, s: Lex::FunctionDef}) => {
                iter.next();
                match iter.peek() {
                    Some(Token{l, s: Lex::Identifier(id)}) => {
                        iter.next();

                        let params = Parser::fn_def_params(iter);

                        let fn_type = match iter.peek() {
                            Some(Token{l, s: Lex::LArrow}) => {
                                iter.next();
                                Parser::primitive(iter).expect(
                                    &format!("L{}: Expected primitive type after -> in function definition", l),
                                )
                            }
                            _ => Primitive::Unit,
                        };

                        match iter.peek() {
                            Some(Token{l, s: Lex::LBrace}) => {
                                iter.next();
                                let mut stmts = Parser::block(iter);

                                match Parser::return_stmt(iter) {
                                    Some(ret) => stmts.push(ret),
                                    None => panic!(
                                        "L{}: Function must end with a return statement, got {:?}",
                                        l,
                                        iter.peek()
                                    ),
                                }

                                match iter.peek() {
                                    Some(Token{l:_, s: Lex::RBrace}) => {
                                        iter.next();
                                    }
                                    _ => panic!("Expected } at end of function definition"),
                                }
                                Some(Ast::FunctionDef(id.clone(), params, fn_type, stmts))
                            }
                            _ => panic!("L{}: Expected {{ after function declaration", l),
                        }
                    }
                    _ => panic!("Expected function name after fn"),
                }
            }
            _ => None,
        }
    }

    fn coroutine_def(iter: &mut TokenIter) -> Option<Ast> {
        match iter.peek() {
            Some(Token{l:_, s: Lex::CoroutineDef}) => {
                iter.next();
                match iter.peek() {
                    Some(Token{l, s: Lex::Identifier(id)}) => {
                        iter.next();

                        let params = Parser::fn_def_params(iter);

                        let co_type = match iter.peek() {
                            Some(Token{l, s: Lex::LArrow}) => {
                                iter.next();
                                Parser::primitive(iter).expect(
                                    &format!("L{}: Expected primitive type after -> in function definition", l),
                                )
                            }
                            _ => Primitive::Unit,
                        };

                        match iter.peek() {
                            Some(Token{l, s: Lex::LBrace}) => {
                                iter.next();
                                let mut stmts = Parser::co_block(iter);

                                match Parser::return_stmt(iter) {
                                    Some(ret) => stmts.push(ret),
                                    None => panic!("L{}: Coroutine must end with a return statement", l),
                                }

                                match iter.peek() {
                                    Some(Token{l:_, s: Lex::RBrace}) => {
                                        iter.next();
                                    }
                                    _ => panic!("L{}: Expected }} at end of function definition", l),
                                }
                                Some(Ast::CoroutineDef(id.clone(), params, co_type, stmts))
                            }
                            _ => panic!("L{}: Expected {{ after function declaration", l),
                        }
                    }
                    _ => panic!("Expected function name after fn"),
                }
            }
            _ => None,
        }
    }

    fn fn_def_params(iter: &mut TokenIter) -> Vec<(String, Primitive)> {
        match iter.peek() {
            Some(Token{l:_, s: Lex::LParen}) => {
                iter.next();
            }
            _ => panic!("expected an ( after function name in function definition"),
        }

        let mut params = vec![];

        while let Some(param) = Parser::identifier_declare(iter) {
            match param {
                Ast::Identifier(id, id_type) => {
                    params.push((id, id_type));
                    match iter.peek() {
                        Some(Token{l:_, s: Lex::Comma}) => {
                            iter.next();
                        }
                        Some(Token{l:_, s: Lex::RParen}) => break,
                        Some(Token{l, s}) => panic!("L{}: Unexpected token in function definition: {:?}", l, s),
                        None => panic!("unexpected EOF"),
                    };
                }
                _ => panic!("invalid parameter declaration in function definition"),
            }
        }

        match iter.peek() {
            Some(Token{l:_, s: Lex::RParen}) => {
                iter.next();
            }
            _ => panic!("expected )"),
        }

        params
    }

    fn block(iter: &mut TokenIter) -> Vec<Ast> {
        let mut stmts = vec![];
        while iter.peek().is_some() {
            match Parser::statement(iter) {
                Some(s) => stmts.push(s),
                None => break,
            }
        }
        stmts
    }

    fn co_block(iter: &mut TokenIter) -> Vec<Ast> {
        let mut stmts = vec![];
        while iter.peek().is_some() {
            match Parser::statement(iter) {
                Some(s) => stmts.push(s),
                None => match Parser::yield_return_stmt(iter) {
                    Some(s) => stmts.push(s),
                    None => break,
                },
            }
        }
        stmts
    }

    fn return_stmt(iter: &mut TokenIter) -> Option<Ast> {
        match iter.peek() {
            Some(Token{l, s: Lex::Return}) => {
                iter.next();
                let exp = Parser::expression(iter);
                match iter.peek() {
                    Some(Token{l:_, s: Lex::Semicolon}) => iter.next(),
                    _ => panic!("L{}: Expected ; after return statement", l),
                };
                match exp {
                    Some(exp) => Some(Ast::Return(Some(Box::new(exp)))),
                    None => Some(Ast::Return(None)),
                }
            }
            _ => None,
        }
    }

    fn yield_return_stmt(iter: &mut TokenIter) -> Option<Ast> {
        match iter.peek() {
            Some(Token{l, s: Lex::YieldReturn}) => {
                iter.next();
                let exp = Parser::expression(iter);
                match iter.peek() {
                    Some(Token{l:_, s: Lex::Semicolon}) => iter.next(),
                    _ => panic!("L{}: Expected ; after yield return statement", l),
                };
                match exp {
                    Some(exp) => Some(Ast::YieldReturn(Some(Box::new(exp)))),
                    None => Some(Ast::YieldReturn(None)),
                }
            }
            _ => None,
        }
    }

    fn statement(iter: &mut TokenIter) -> Option<Ast> {
        let line = iter.peek().map_or(0, |t| t.l);
        let stm = match Parser::bind(iter) {
            Some(b) => Some(b),
            None => match Parser::println_stmt(iter) {
                Some(p) => Some(p),
                _ => None,
            },
        };

        if stm.is_some() {
            match iter.peek() {
                Some(Token{l:_, s: Lex::Semicolon}) => {
                    iter.next();
                }
                _ => panic!(format!(
                    "L{}: Exected ; after statement, found {:?}",
                    line,
                    iter.peek()
                )),
            }
        }

        stm
    }

    fn println_stmt(iter: &mut TokenIter) -> Option<Ast> {
        let tk = iter.peek();
        match tk {
            Some(Token{l, s: Lex::Printiln}) => {
                iter.next();
                let exp = Parser::expression(iter);
                match exp {
                    Some(exp) => Some(Ast::Printiln(Box::new(exp))),
                    None => panic!("L{}: Expected expression after println", l),
                }
            }
            Some(Token{l, s: Lex::Printbln}) => {
                iter.next();
                let exp = Parser::expression(iter);
                match exp {
                    Some(exp) => Some(Ast::Printbln(Box::new(exp))),
                    None => panic!("L{}: Expected expression after println", l),
                }
            }
            _ => None,
        }
    }

    fn bind(iter: &mut TokenIter) -> Option<Ast> {
        match Parser::identifier_declare(iter) {
            Some(Ast::Identifier(id, id_type)) => {
                let pt = iter.peek();
                match pt {
                    Some(Token{l, s: Lex::Assign}) => {
                        iter.next();
                        match iter.peek() {
                            Some(Token{l, s: Lex::Init}) => {
                                let co_init =
                                    Parser::co_init(iter).expect(&format!("L{}: Invalid coroutine init", l));
                                Some(Ast::Bind(id, id_type, Box::new(co_init)))
                            }
                            _ => {
                                let exp = Parser::expression(iter).expect(&format!(
                                    "L{}: Expected an expression or coroutine init after :=, found {:?}",
                                    l,
                                    iter.peek()
                                ));
                                Some(Ast::Bind(id, id_type, Box::new(exp)))
                            }
                        }
                    }
                    _ => {
                        panic!("Expected := after identifer in bind statement");
                    }
                }
            }
            Some(_) => panic!("invalid LHS in bind expresion"),
            None => None,
        }
    }

    fn co_init(iter: &mut TokenIter) -> Option<Ast> {
        match iter.peek() {
            Some(Token{l, s: Lex::Init}) => {
                iter.next();
                match iter.peek() {
                    Some(Token{l, s: Lex::Identifier(id)}) => {
                        iter.next();
                        let params = Parser::fn_call_params(iter)
                            .expect(&format!("L{}: Expected parameters after coroutine name", l));
                        Some(Ast::CoroutineInit(id.clone(), params))
                    }
                    _ => {
                        panic!("L{}: expected identifier after init", l);
                    }
                }
            }
            _ => None,
        }
    }

    fn expression(iter: &mut TokenIter) -> Option<Ast> {
        Parser::logical_or(iter)
    }

    fn logical_or(iter: &mut TokenIter) -> Option<Ast> {
        match Parser::logical_and(iter) {
            Some(n) => match iter.peek() {
                Some(Token{l, s: Lex::BOr}) => {
                    iter.next();
                    let n2 = Parser::logical_or(iter).expect(&format!("L{}: An expression after ||", l));
                    Some(Ast::BOr(Box::new(n), Box::new(n2)))
                }
                _ => Some(n),
            },
            None => None,
        }
    }

    fn logical_and(iter: &mut TokenIter) -> Option<Ast> {
        match Parser::comparison(iter) {
            Some(n) => match iter.peek() {
                Some(Token{l, s: Lex::BAnd}) => {
                    iter.next();
                    let n2 = Parser::logical_and(iter).expect(&format!("L{}: An expression after ||", l));
                    Some(Ast::BAnd(Box::new(n), Box::new(n2)))
                }
                _ => Some(n),
            },
            None => None,
        }
    }

    fn comparison(iter: &mut TokenIter) -> Option<Ast> {
        match Parser::sum(iter) {
            Some(n) => match iter.peek() {
                Some(Token{l, s: Lex::Eq}) => {
                    iter.next();
                    let n2 = Parser::comparison(iter).expect(&format!("L{}: An expression after ==", l));
                    Some(Ast::Eq(Box::new(n), Box::new(n2)))
                }
                Some(Token{l, s: Lex::NEq}) => {
                    iter.next();
                    let n2 = Parser::comparison(iter).expect(&format!("L{}: An expression after !=", l));
                    Some(Ast::NEq(Box::new(n), Box::new(n2)))
                }
                Some(Token{l, s: Lex::Gr}) => {
                    iter.next();
                    let n2 = Parser::comparison(iter).expect(&format!("L{}: An expression after >", l));
                    Some(Ast::Gr(Box::new(n), Box::new(n2)))
                }
                Some(Token{l, s: Lex::GrEq}) => {
                    iter.next();
                    let n2 = Parser::comparison(iter).expect(&format!("L{}: An expression after >=", l));
                    Some(Ast::GrEq(Box::new(n), Box::new(n2)))
                }
                Some(Token{l, s: Lex::Ls}) => {
                    iter.next();
                    let n2 = Parser::comparison(iter).expect(&format!("L{}: An expression after <", l));
                    Some(Ast::Ls(Box::new(n), Box::new(n2)))
                }
                Some(Token{l, s: Lex::LsEq}) => {
                    iter.next();
                    let n2 = Parser::comparison(iter).expect(&format!("L{}: An expression after <=", l));
                    Some(Ast::LsEq(Box::new(n), Box::new(n2)))
                }
                _ => Some(n),
            },
            None => None,
        }
    }

    fn sum(iter: &mut TokenIter) -> Option<Ast> {
        match Parser::term(iter) {
            Some(n) => match iter.peek() {
                Some(Token{l, s: Lex::Add}) => {
                    iter.next();
                    let n2 = Parser::sum(iter).expect(&format!("L{}: An expression after +", l));
                    Some(Ast::Add(Box::new(n), Box::new(n2)))
                }
                _ => Some(n),
            },
            None => None,
        }
    }

    fn term(iter: &mut TokenIter) -> Option<Ast> {
        match Parser::factor(iter) {
            Some(n) => match iter.peek() {
                Some(Token{l, s: Lex::Mul}) => {
                    iter.next();
                    let n2 = Parser::term(iter).expect(&format!("L{}: a valid term after *", l));
                    Some(Ast::Mul(Box::new(n), Box::new(n2)))
                }
                _ => Some(n),
            },
            None => None,
        }
    }

    fn factor(iter: &mut TokenIter) -> Option<Ast> {
        match iter.peek() {
            Some(Token{l:_, s: Lex::If}) => Parser::if_expression(iter),
            Some(Token{l, s: Lex::LParen}) => {
                iter.next();
                let exp = Parser::expression(iter);
                match iter.peek() {
                    Some(Token{l:_, s: Lex::RParen}) => iter.next(),
                    x => panic!("L{}: exected ) but found {:?}", l, x),
                };
                exp
            }
            _ => match Parser::constant(iter) {
                Some(n) => Some(n),
                None => match Parser::function_call_or_variable(iter) {
                    Some(n) => Some(n),
                    None => match Parser::co_yield(iter) {
                        Some(n) => Some(n),
                        None => None,
                    },
                },
            },
        }
    }

    fn if_expression(iter: &mut TokenIter) -> Option<Ast> {
        match iter.peek() {
            Some(Token{l, s: Lex::If}) => {
                iter.next();
                // expression
                let cond =
                    Parser::expression(iter).expect("Expected conditional expressoin after if");
                // lbrace
                iter.next()
                    .map(|t| t.s == Lex::LBrace)
                    .expect("Expected {");
                // expression
                let true_arm =
                    Parser::expression(iter).expect("Expression in true arm of if expression");
                // rbrace
                iter.next()
                    .map(|t| t.s == Lex::RBrace)
                    .expect("Expected }");
                // else
                iter.next()
                    .map(|t| t.s == Lex::Else)
                    .expect("Expected else arm of if expression");

                // check for `else if`
                let false_arm = match iter.peek() {
                    Some(Token{l, s: Lex::If}) => {
                        Parser::if_expression(iter).expect(&format!("L{}: Expected if expression after else if", l))
                    }
                    _ => {
                        iter.next()
                            .map(|t| t.s == Lex::LBrace)
                            .expect(&format!("L{}: Expected {{", l));
                        // expression
                        let false_arm = Parser::expression(iter)
                            .expect(&format!("L{}: Expression in false arm of if expression", l));
                        // rbrace
                        iter.next()
                            .map(|t| t.s == Lex::RBrace)
                            .expect(&format!("L{}: Expected }}", l));
                        false_arm
                    }
                };
                Some(Ast::If(
                    Box::new(cond),
                    Box::new(true_arm),
                    Box::new(false_arm),
                ))
            }
            _ => None,
        }
    }

    fn function_call_or_variable(iter: &mut TokenIter) -> Option<Ast> {
        match Parser::identifier(iter) {
            Some(Ast::Identifier(id, _id_type)) => match Parser::fn_call_params(iter) {
                Some(params) => {
                    // this is a function call
                    Some(Ast::FunctionCall(id, params))
                }
                _ => Some(Ast::Identifier(id, _id_type)),
            },
            Some(_) => panic!("expected identifier"),
            None => None,
        }
    }

    /// LPAREN [EXPRESSION [, EXPRESSION]*] RPAREN
    fn fn_call_params(iter: &mut TokenIter) -> Option<Vec<Ast>> {
        match iter.peek() {
            Some(Token{l, s: Lex::LParen}) => {
                // this is a function call
                iter.next();

                let mut params = vec![];
                while let Some(param) = Parser::expression(iter) {
                    match param {
                        exp => {
                            params.push(exp);
                            match iter.peek() {
                                Some(Token{l:_, s: Lex::Comma}) => {
                                    iter.next();
                                }
                                Some(Token{l:_, s: Lex::RParen}) => break,
                                Some(t) => panic!("L{}: Unexpected token in function call: {:?}", t.l, t.s),
                                None => panic!("L{}: unexpected EOF", l),
                            };
                        }
                    }
                }

                match iter.peek() {
                    Some(Token{l:_, s: Lex::RParen}) => {
                        iter.next();
                    }
                    _ => panic!("L{}: expected ) after function call", l),
                }
                Some(params)
            }
            _ => None,
        }
    }

    fn co_yield(iter: &mut TokenIter) -> Option<Ast> {
        match iter.peek() {
            Some(Token{l, s: Lex::Yield}) => {
                iter.next();
                match Parser::identifier(iter) {
                    Some(id) => Some(Ast::Yield(Box::new(id))),
                    _ => panic!("L{}: expected an identifier after yield", l),
                }
            }
            _ => None,
        }
    }

    fn primitive(iter: &mut TokenIter) -> Option<Primitive> {
        match iter.peek() {
            Some(Token{l:_, s: Lex::Primitive(primitive)}) => {
                iter.next();
                match primitive {
                    lexer::Primitive::I32 => Some(Primitive::I32),
                    lexer::Primitive::Bool => Some(Primitive::Bool),
                }
            }
            _ => None,
        }
    }

    fn identifier_declare(iter: &mut TokenIter) -> Option<Ast> {
        match iter.peek() {
            Some(Token{l, s: Lex::Identifier(id)}) => {
                iter.next();
                match iter.peek() {
                    Some(Token{l, s: Lex::Colon}) => {
                        iter.next();
                        match Parser::primitive(iter) {
                            Some(p) => Some(Ast::Identifier(id.clone(), p)),
                            None => panic!("L{}: Invalid primitive type: {:?}", l, iter.peek()),
                        }
                    }
                    _ => panic!(
                        "L{}: Expected type after variable declaration, found: {:?}",
                        l,
                        iter.peek().map(|t| &t.s)
                    ),
                }
            }
            _ => None,
        }
    }

    fn identifier(iter: &mut TokenIter) -> Option<Ast> {
        match iter.peek() {
            Some(token) => match token {
                Token{l:_, s: Lex::Identifier(id)} => {
                    iter.next();
                    Some(Ast::Identifier(id.clone(), Primitive::Unknown))
                }
                _ => None,
            },
            None => None,
        }
    }

    fn constant(iter: &mut TokenIter) -> Option<Ast> {
        match Parser::number(iter) {
            None => match Parser::boolean(iter) {
                None => None,
                Some(t) => Some(t),
            },
            Some(i) => Some(i),
        }
    }

    fn number(iter: &mut TokenIter) -> Option<Ast> {
        match iter.peek() {
            Some(token) => match token {
                Token{l:_, s: Lex::Integer(i)} => {
                    iter.next();
                    Some(Ast::Integer(*i))
                }
                _ => None,
            },
            None => None,
        }
    }

    fn boolean(iter: &mut TokenIter) -> Option<Ast> {
        match iter.peek() {
            Some(Token{l:_, s: Lex::Bool(b)}) => {
                iter.next();
                Some(Ast::Boolean(*b))
            }
            _ => None,
        }
    }
}
