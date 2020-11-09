// AST - a type(s) which is used to construct an AST representing the logic of the
// program
// Each type of node represents an expression and the only requirement is that at the
// end of computing an expression its result is in EAX
use crate::lexer::{self, Lex};
use crate::Token;

type TokenIter<'a> = std::iter::Peekable<core::slice::Iter<'a, Token>>;

type PResult = Result<Option<AstNode>, String>;

#[derive(Debug)]
pub struct AstNode {
    pub l: u32,
    pub n: Ast,
}

impl AstNode {
    pub fn new(l: u32, n: Ast) -> AstNode {
        AstNode { l, n }
    }
}

#[derive(Debug)]
pub enum Ast {
    Integer(i32),
    Boolean(bool),
    Identifier(String, Primitive),
    Primitive(Primitive),
    Mul(Box<AstNode>, Box<AstNode>),
    Add(Box<AstNode>, Box<AstNode>),
    BAnd(Box<AstNode>, Box<AstNode>),
    BOr(Box<AstNode>, Box<AstNode>),
    Gr(Box<AstNode>, Box<AstNode>),
    GrEq(Box<AstNode>, Box<AstNode>),
    Ls(Box<AstNode>, Box<AstNode>),
    LsEq(Box<AstNode>, Box<AstNode>),
    Eq(Box<AstNode>, Box<AstNode>),
    NEq(Box<AstNode>, Box<AstNode>),
    Bind(String, Primitive, Box<AstNode>),
    Return(Option<Box<AstNode>>),
    FunctionDef(String, Vec<(String, Primitive)>, Primitive, Vec<AstNode>),
    FunctionCall(String, Vec<AstNode>),
    CoroutineDef(String, Vec<(String, Primitive)>, Primitive, Vec<AstNode>),
    CoroutineInit(String, Vec<AstNode>),
    Yield(Box<AstNode>),
    YieldReturn(Option<Box<AstNode>>),
    If(Box<AstNode>, Box<AstNode>, Box<AstNode>),
    Module(Vec<AstNode>, Vec<AstNode>),
    Printi(Box<AstNode>),
    Printiln(Box<AstNode>),
    Printbln(Box<AstNode>),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Primitive {
    I32,
    Bool,
    Unit,
    Unknown,
}
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
pub fn parse(tokens: Vec<Token>) -> PResult {
    let mut iter = tokens.iter().peekable();
    //Node::function(&mut iter)
    module(&mut iter)
}

fn module(iter: &mut TokenIter) -> PResult {
    let mut functions = vec![];
    let mut coroutines = vec![];

    let line = iter.peek().map_or(0, |t| t.l);
    while iter.peek().is_some() {
        match function_def(iter)? {
            Some(f) => functions.push(f),
            None => match coroutine_def(iter)? {
                Some(co) => coroutines.push(co),
                None => break,
            },
        }
    }

    Ok(if functions.len() > 0 {
        Some(AstNode::new(line, Ast::Module(functions, coroutines)))
    } else {
        None
    })
}

fn function_def(iter: &mut TokenIter) -> PResult {
    let syntax = match iter.peek() {
        Some(Token {
            l: _,
            s: Lex::FunctionDef,
        }) => {
            iter.next();
            match iter.peek() {
                Some(Token {
                    l,
                    s: Lex::Identifier(id),
                }) => {
                    iter.next();

                    let params = fn_def_params(iter)?;

                    let fn_type = match iter.peek() {
                        Some(Token { l, s: Lex::LArrow }) => {
                            iter.next();
                            primitive(iter).ok_or(&format!(
                                "L{}: Expected primitive type after -> in function definition",
                                l
                            ))?
                        }
                        _ => Primitive::Unit,
                    };

                    consume_must_be(iter, Lex::LBrace)?;
                    let mut stmts = block(iter)
                        .into_iter()
                        .collect::<Result<Vec<AstNode>, String>>()?;

                    match return_stmt(iter)? {
                        Some(ret) => stmts.push(ret),
                        None => {
                            return Err(format!(
                                "L{}: Function must end with a return statement, got {:?}",
                                l,
                                iter.peek(),
                            ))
                        }
                    }

                    consume_must_be(iter, Lex::RBrace)?;

                    Some(AstNode::new(
                        *l,
                        Ast::FunctionDef(id.clone(), params, fn_type, stmts),
                    ))
                }
                _ => panic!("Expected function name after fn"),
            }
        }
        _ => None,
    };
    Ok(syntax)
}

fn coroutine_def(iter: &mut TokenIter) -> PResult {
    let syntax = match iter.peek() {
        Some(Token {
            l: _,
            s: Lex::CoroutineDef,
        }) => {
            iter.next();
            match iter.peek() {
                Some(Token {
                    l,
                    s: Lex::Identifier(id),
                }) => {
                    iter.next();

                    let params = fn_def_params(iter)?;

                    let co_type = match iter.peek() {
                        Some(Token { l, s: Lex::LArrow }) => {
                            iter.next();
                            primitive(iter).expect(&format!(
                                "L{}: Expected primitive type after -> in function definition",
                                l
                            ))
                        }
                        _ => Primitive::Unit,
                    };

                    consume_must_be(iter, Lex::LBrace)?;
                    let mut stmts = co_block(iter)
                        .into_iter()
                        .collect::<Result<Vec<AstNode>, String>>()?;

                    match return_stmt(iter)? {
                        Some(ret) => stmts.push(ret),
                        None => {
                            return Err(format!(
                                "L{}: Coroutine must end with a return statement",
                                l
                            ))
                        }
                    }

                    consume_must_be(iter, Lex::RBrace)?;

                    Some(AstNode::new(
                        *l,
                        Ast::CoroutineDef(id.clone(), params, co_type, stmts),
                    ))
                }
                _ => return Err(format!("Expected function name after fn")),
            }
        }
        _ => None,
    };
    Ok(syntax)
}

fn fn_def_params(iter: &mut TokenIter) -> Result<Vec<(String, Primitive)>, String> {
    consume_must_be(iter, Lex::LParen)?;

    let mut params = vec![];

    while let Some(param) = identifier_declare(iter)? {
        match param {
            AstNode {
                l: _,
                n: Ast::Identifier(id, id_type),
            } => {
                params.push((id, id_type));
                match iter.peek() {
                    Some(Token {
                        l: _,
                        s: Lex::Comma,
                    }) => {
                        iter.next();
                    }
                    Some(Token {
                        l: _,
                        s: Lex::RParen,
                    }) => break,
                    Some(Token { l, s }) =>
                        return Err(format!("L{}: Unexpected token in function definition: {:?}", l, s)),
                    None => return Err(format!("unexpected EOF")),
                };
            }
            _ => return Err(format!("invalid parameter declaration in function definition")),
        }
    }

    consume_must_be(iter, Lex::RParen)?;

    Ok(params)
}

fn block(iter: &mut TokenIter) -> Vec<Result<AstNode, String>> {
    let mut stmts = vec![];
    while iter.peek().is_some() {
        match statement(iter) {
            Ok(Some(s)) => stmts.push(Ok(s)),
            Ok(None) => break,
            Err(msg) => stmts.push(Err(msg)),
        }
    }
    stmts
}

fn co_block(iter: &mut TokenIter) -> Vec<Result<AstNode, String>> {
    let mut stmts = vec![];
    while iter.peek().is_some() {
        match statement(iter) {
            Ok(Some(s)) => stmts.push(Ok(s)),
            Ok(None) => match yield_return_stmt(iter) {
                Ok(Some(s)) => stmts.push(Ok(s)),
                Ok(None) => break,
                Err(msg) => stmts.push(Err(msg)),
            },
            Err(msg) => stmts.push(Err(msg)),
        }
    }
    stmts
}

fn return_stmt(iter: &mut TokenIter) -> PResult {
    Ok(match iter.peek() {
        Some(Token { l, s: Lex::Return }) => {
            iter.next();
            let exp = expression(iter)?;
            consume_must_be(iter, Lex::Semicolon)?;
            match exp {
                Some(exp) => Some(AstNode::new(*l, Ast::Return(Some(Box::new(exp))))),
                None => Some(AstNode::new(*l, Ast::Return(None))),
            }
        }
        _ => None,
    })
}

fn yield_return_stmt(iter: &mut TokenIter) -> PResult {
    Ok(match iter.peek() {
        Some(Token {
            l,
            s: Lex::YieldReturn,
        }) => {
            iter.next();
            let exp = expression(iter)?;
            consume_must_be(iter, Lex::Semicolon)?;
            match exp {
                Some(exp) => Some(AstNode::new(*l, Ast::YieldReturn(Some(Box::new(exp))))),
                None => Some(AstNode::new(*l, Ast::YieldReturn(None))),
            }
        }
        _ => None,
    })
}

fn statement(iter: &mut TokenIter) -> PResult {
    let stm = match bind(iter)? {
        Some(b) => Some(b),
        None => match println_stmt(iter)? {
            Some(p) => Some(p),
            _ => None,
        },
    };

    if stm.is_some() {
        consume_must_be(iter, Lex::Semicolon)?;
    }

    Ok(stm)
}

fn println_stmt(iter: &mut TokenIter) -> PResult {
    let tk = iter.peek();
    Ok(match tk {
        Some(Token { l, s }) if *s == Lex::Printiln || *s == Lex::Printbln => {
            iter.next();
            let exp =
                expression(iter)?.ok_or(format!("L{}: Expected expression after println", l))?;

            match s {
                Lex::Printiln => Some(AstNode::new(*l, Ast::Printiln(Box::new(exp)))),
                Lex::Printbln => Some(AstNode::new(*l, Ast::Printbln(Box::new(exp)))),
                _ => panic!(
                    "CRITICAL: already tested for a print token but found {:?}",
                    s
                ),
            }
        }
        _ => None,
    })
}

fn bind(iter: &mut TokenIter) -> PResult {
    Ok(match identifier_declare(iter)? {
        Some(AstNode {
            l: _,
            n: Ast::Identifier(id, id_type),
        }) => {
            let pt = iter.peek();
            match pt {
                Some(Token { l, s: Lex::Assign }) => {
                    iter.next();
                    match iter.peek() {
                        Some(Token { l, s: Lex::Init }) => {
                            let co_init =
                                co_init(iter)?.ok_or(&format!("L{}: Invalid coroutine init", l))?;
                            Some(AstNode::new(*l, Ast::Bind(id, id_type, Box::new(co_init))))
                        }
                        _ => {
                            let exp = expression(iter)?.ok_or(&format!(
                                    "L{}: Expected an expression or coroutine init after :=, found {:?}",
                                    l,
                                    iter.peek()
                                ))?;
                            Some(AstNode::new(*l, Ast::Bind(id, id_type, Box::new(exp))))
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
    })
}

fn co_init(iter: &mut TokenIter) -> PResult {
    match iter.peek() {
        Some(Token { l, s: Lex::Init }) => {
            iter.next();
            match iter.peek() {
                Some(Token {
                    l,
                    s: Lex::Identifier(id),
                }) => {
                    iter.next();
                    let params = fn_call_params(iter)?
                        .ok_or(&format!("L{}: Expected parameters after coroutine name", l))?;
                    Ok(Some(AstNode::new(
                        *l,
                        Ast::CoroutineInit(id.clone(), params),
                    )))
                }
                _ => Err(format!("L{}: expected identifier after init", l)),
            }
        }
        _ => Ok(None),
    }
}

fn expression(iter: &mut TokenIter) -> PResult {
    logical_or(iter)
}

fn logical_or(iter: &mut TokenIter) -> PResult {
    Ok(match logical_and(iter)? {
        Some(n) => match iter.peek() {
            Some(Token { l, s: Lex::BOr }) => {
                iter.next();
                let n2 = logical_or(iter)?.ok_or(&format!("L{}: An expression after ||", l))?;
                Some(AstNode::new(*l, Ast::BOr(Box::new(n), Box::new(n2))))
            }
            _ => Some(n),
        },
        None => None,
    })
}

fn logical_and(iter: &mut TokenIter) -> PResult {
    Ok(match comparison(iter)? {
        Some(n) => match iter.peek() {
            Some(Token { l, s: Lex::BAnd }) => {
                iter.next();
                let n2 = logical_and(iter)?.ok_or(&format!("L{}: An expression after ||", l))?;
                Some(AstNode::new(*l, Ast::BAnd(Box::new(n), Box::new(n2))))
            }
            _ => Some(n),
        },
        None => None,
    })
}

fn comparison(iter: &mut TokenIter) -> PResult {
    Ok(match sum(iter)? {
        Some(n) => match iter.peek() {
            Some(Token { l, s: Lex::Eq }) => {
                iter.next();
                let n2 = comparison(iter)?.ok_or(&format!("L{}: An expression after ==", l))?;
                Some(AstNode::new(*l, Ast::Eq(Box::new(n), Box::new(n2))))
            }
            Some(Token { l, s: Lex::NEq }) => {
                iter.next();
                let n2 = comparison(iter)?.ok_or(&format!("L{}: An expression after !=", l))?;
                Some(AstNode::new(*l, Ast::NEq(Box::new(n), Box::new(n2))))
            }
            Some(Token { l, s: Lex::Gr }) => {
                iter.next();
                let n2 = comparison(iter)?.ok_or(&format!("L{}: An expression after >", l))?;
                Some(AstNode::new(*l, Ast::Gr(Box::new(n), Box::new(n2))))
            }
            Some(Token { l, s: Lex::GrEq }) => {
                iter.next();
                let n2 = comparison(iter)?.ok_or(&format!("L{}: An expression after >=", l))?;
                Some(AstNode::new(*l, Ast::GrEq(Box::new(n), Box::new(n2))))
            }
            Some(Token { l, s: Lex::Ls }) => {
                iter.next();
                let n2 = comparison(iter)?.ok_or(&format!("L{}: An expression after <", l))?;
                Some(AstNode::new(*l, Ast::Ls(Box::new(n), Box::new(n2))))
            }
            Some(Token { l, s: Lex::LsEq }) => {
                iter.next();
                let n2 = comparison(iter)?.ok_or(&format!("L{}: An expression after <=", l))?;
                Some(AstNode::new(*l, Ast::LsEq(Box::new(n), Box::new(n2))))
            }
            _ => Some(n),
        },
        None => None,
    })
}

fn sum(iter: &mut TokenIter) -> PResult {
    Ok(match term(iter)? {
        Some(n) => match iter.peek() {
            Some(Token { l, s: Lex::Add }) => {
                iter.next();
                let n2 = sum(iter)?.ok_or(&format!("L{}: An expression after +", l))?;
                Some(AstNode::new(*l, Ast::Add(Box::new(n), Box::new(n2))))
            }
            _ => Some(n),
        },
        None => None,
    })
}

fn term(iter: &mut TokenIter) -> PResult {
    Ok(match factor(iter)? {
        Some(n) => match iter.peek() {
            Some(Token { l, s: Lex::Mul }) => {
                iter.next();
                let n2 = term(iter)?.ok_or(&format!("L{}: a valid term after *", l))?;
                Some(AstNode::new(*l, Ast::Mul(Box::new(n), Box::new(n2))))
            }
            _ => Some(n),
        },
        None => None,
    })
}

fn factor(iter: &mut TokenIter) -> PResult {
    Ok(match iter.peek() {
        Some(Token { l: _, s: Lex::If }) => if_expression(iter)?,
        Some(Token {
            l: _,
            s: Lex::LParen,
        }) => {
            iter.next();
            let exp = expression(iter)?;
            consume_must_be(iter, Lex::RParen)?;
            exp
        }
        _ => match constant(iter)? {
            Some(n) => Some(n),
            None => match function_call_or_variable(iter)? {
                Some(n) => Some(n),
                None => match co_yield(iter)? {
                    Some(n) => Some(n),
                    None => None,
                },
            },
        },
    })
}

fn if_expression(iter: &mut TokenIter) -> PResult {
    Ok(match iter.peek() {
        Some(Token { l, s: Lex::If }) => {
            iter.next();
            let cond = expression(iter)?.ok_or("Expected conditional expression after if")?;
            consume_must_be(iter, Lex::LBrace)?;

            let true_arm = expression(iter)?.ok_or("Expression in true arm of if expression")?;
            consume_must_be(iter, Lex::RBrace)?;
            consume_must_be(iter, Lex::Else)?;

            // check for `else if`
            let false_arm = match iter.peek() {
                Some(Token { l, s: Lex::If }) => if_expression(iter)?
                    .ok_or(format!("L{}: Expected if expression after else if", l))?,
                _ => {
                    consume_must_be(iter, Lex::LBrace)?;
                    // expression
                    let false_arm = expression(iter)?
                        .ok_or(&format!("L{}: Expression in false arm of if expression", l))?;
                    consume_must_be(iter, Lex::RBrace)?;
                    false_arm
                }
            };
            Some(AstNode::new(
                *l,
                Ast::If(Box::new(cond), Box::new(true_arm), Box::new(false_arm)),
            ))
        }
        _ => None,
    })
}

fn function_call_or_variable(iter: &mut TokenIter) -> PResult {
    Ok(match identifier(iter)? {
        Some(AstNode {
            l,
            n: Ast::Identifier(id, _id_type),
        }) => match fn_call_params(iter)? {
            Some(params) => {
                // this is a function call
                Some(AstNode::new(l, Ast::FunctionCall(id, params)))
            }
            _ => Some(AstNode::new(l, Ast::Identifier(id, _id_type))),
        },
        Some(_) => return Err(format!("expected identifier")),
        None => None,
    })
}

/// LPAREN [EXPRESSION [, EXPRESSION]*] RPAREN
fn fn_call_params(iter: &mut TokenIter) -> Result<Option<Vec<AstNode>>, String> {
    match iter.peek() {
        Some(Token { l, s: Lex::LParen }) => {
            // this is a function call
            iter.next();

            let mut params = vec![];
            while let Some(param) = expression(iter)? {
                match param {
                    exp => {
                        params.push(exp);
                        match iter.peek() {
                            Some(Token {
                                l: _,
                                s: Lex::Comma,
                            }) => {
                                iter.next();
                            }
                            Some(Token {
                                l: _,
                                s: Lex::RParen,
                            }) => break,
                            Some(t) => {
                                panic!("L{}: Unexpected token in function call: {:?}", t.l, t.s)
                            }
                            None => panic!("L{}: unexpected EOF", l),
                        };
                    }
                }
            }

            match iter.peek() {
                Some(Token {
                    l: _,
                    s: Lex::RParen,
                }) => {
                    iter.next();
                }
                _ => panic!("L{}: expected ) after function call", l),
            }
            Ok(Some(params))
        }
        _ => Ok(None),
    }
}

fn co_yield(iter: &mut TokenIter) -> PResult {
    Ok(match iter.peek() {
        Some(Token { l, s: Lex::Yield }) => {
            iter.next();
            match identifier(iter)? {
                Some(id) => Some(AstNode::new(*l, Ast::Yield(Box::new(id)))),
                _ => panic!("L{}: expected an identifier after yield", l),
            }
        }
        _ => None,
    })
}

fn primitive(iter: &mut TokenIter) -> Option<Primitive> {
    match iter.peek() {
        Some(Token {
            l: _,
            s: Lex::Primitive(primitive),
        }) => {
            iter.next();
            match primitive {
                lexer::Primitive::I32 => Some(Primitive::I32),
                lexer::Primitive::Bool => Some(Primitive::Bool),
            }
        }
        _ => None,
    }
}

fn identifier_declare(iter: &mut TokenIter) -> PResult {
    Ok(match iter.peek() {
        Some(Token {
            l,
            s: Lex::Identifier(id),
        }) => {
            iter.next();
            match iter.peek() {
                Some(Token { l, s: Lex::Colon }) => {
                    iter.next();
                    match primitive(iter) {
                        Some(p) => Some(AstNode::new(*l, Ast::Identifier(id.clone(), p))),
                        None => {
                            return Err(format!(
                                "L{}: Invalid primitive type: {:?}",
                                l,
                                iter.peek()
                            ))
                        }
                    }
                }
                _ => {
                    return Err(format!(
                        "L{}: Expected type after variable declaration, found: {:?}",
                        l,
                        iter.peek().map(|t| &t.s)
                    ))
                }
            }
        }
        _ => None,
    })
}

fn identifier(iter: &mut TokenIter) -> PResult {
    Ok(match iter.peek() {
        Some(token) => match token {
            Token {
                l,
                s: Lex::Identifier(id),
            } => {
                iter.next();
                Some(AstNode::new(
                    *l,
                    Ast::Identifier(id.clone(), Primitive::Unknown),
                ))
            }
            _ => None,
        },
        None => None,
    })
}

fn constant(iter: &mut TokenIter) -> PResult {
    Ok(match number(iter)? {
        None => match boolean(iter)? {
            None => None,
            Some(t) => Some(t),
        },
        Some(i) => Some(i),
    })
}

fn number(iter: &mut TokenIter) -> PResult {
    Ok(match iter.peek() {
        Some(token) => match token {
            Token {
                l,
                s: Lex::Integer(i),
            } => {
                iter.next();
                Some(AstNode::new(*l, Ast::Integer(*i)))
            }
            _ => None,
        },
        None => None,
    })
}

fn boolean(iter: &mut TokenIter) -> PResult {
    match iter.peek() {
        Some(Token { l, s: Lex::Bool(b) }) => {
            iter.next();
            Ok(Some(AstNode::new(*l, Ast::Boolean(*b))))
        }
        _ => Ok(None),
    }
}

fn consume_if<'a>(iter: &'a mut TokenIter, test: Lex) -> Option<&'a Token> {
    match iter.peek() {
        Some(tok) if tok.s == test => {
            let tok = iter.next();
            tok
        }
        _ => None,
    }
}

fn consume_must_be<'a>(iter: &'a mut TokenIter, test: Lex) -> Result<&'a Token, String> {
    match iter.peek() {
        Some(tok) if tok.s == test => {
            let tok = iter
                .next()
                .expect("CRITICAL: failed to go to next token after successful match");
            Ok(tok)
        }
        Some(Token { l, s }) => Err(format!("L{}: Expected {:?}, but found {:?}", l, test, s)),
        None => Err(format!("Expected {:?}, but found EOF", test)),
    }
}
