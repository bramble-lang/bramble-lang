// AST - a type(s) which is used to construct an AST representing the logic of the
// program
// Each type of node represents an expression and the only requirement is that at the
// end of computing an expression its result is in EAX
use crate::lexer::{self, Lex};
use crate::Token;

type TokenIter<'a> = std::iter::Peekable<core::slice::Iter<'a, Token>>;

type PResult = Result<Option<AstNode>, String>;

#[derive(Debug, PartialEq)]
pub struct AstNode {
    pub l: u32,
    pub n: Ast,
}

impl AstNode {
    pub fn new(l: u32, n: Ast) -> AstNode {
        AstNode { l, n }
    }

    pub fn new_yield(line: u32, id_line: u32, id: String) -> AstNode {
        AstNode::new(
            line,
            Ast::Yield(Box::new(AstNode::new(id_line, Ast::Identifier(id)))),
        )
    }

    pub fn new_bind(line: u32, id: Box<AstNode>, exp: Box<AstNode>) -> Result<AstNode, String> {
        match id.as_ref() {
            AstNode{l:_, n: Ast::IdentifierDeclare(id, prim)} => 
                Ok(AstNode::new(line, Ast::Bind(id.clone(), *prim, exp))),
            _ => Err(format!("L{}: Expected identifier declaration, found {:?}", line, id))
        }
    }

    pub fn binary_op(
        line: u32,
        op: &Lex,
        left: Box<AstNode>,
        right: Box<AstNode>,
    ) -> Result<AstNode, String> {
        match op {
            Lex::Eq => Ok(AstNode::new(line, Ast::Eq(left, right))),
            Lex::NEq => Ok(AstNode::new(line, Ast::NEq(left, right))),
            Lex::Ls => Ok(AstNode::new(line, Ast::Ls(left, right))),
            Lex::LsEq => Ok(AstNode::new(line, Ast::LsEq(left, right))),
            Lex::Gr => Ok(AstNode::new(line, Ast::Gr(left, right))),
            Lex::GrEq => Ok(AstNode::new(line, Ast::GrEq(left, right))),
            Lex::BAnd => Ok(AstNode::new(line, Ast::BAnd(left, right))),
            Lex::BOr => Ok(AstNode::new(line, Ast::BOr(left, right))),
            Lex::Add => Ok(AstNode::new(line, Ast::Add(left, right))),
            Lex::Mul => Ok(AstNode::new(line, Ast::Mul(left, right))),
            _ => Err(format!("L{}: {} is not a binary operator", line, op)),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Ast {
    Integer(i32),
    Boolean(bool),
    Identifier(String),
    IdentifierDeclare(String, Primitive),
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
    Printi(Box<AstNode>),
    Printiln(Box<AstNode>),
    Printbln(Box<AstNode>),

    If(Box<AstNode>, Box<AstNode>, Box<AstNode>),
    ExpressionBlock(Vec<AstNode>),

    Statement(Box<AstNode>),
    Bind(String, Primitive, Box<AstNode>),
    Return(Option<Box<AstNode>>),
    Yield(Box<AstNode>),
    YieldReturn(Option<Box<AstNode>>),

    FunctionDef(String, Vec<(String, Primitive)>, Primitive, Vec<AstNode>),
    FunctionCall(String, Vec<AstNode>),
    CoroutineDef(String, Vec<(String, Primitive)>, Primitive, Vec<AstNode>),
    CoroutineInit(String, Vec<AstNode>),
    Module(Vec<AstNode>, Vec<AstNode>),
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
    FUNCTION_CALL := IDENTIFIER LPAREN EXPRESSION [, EXPRESSION]* RPAREN
    YIELD := yield IDENTIFIER
    IF := if EXPRESSION LBRACE EXPRESSION RBRACE else LBRACE EXPRESSION RBRACE
    FACTOR := FUNCTION_CALL | YIELD | NUMBER | IDENTIFIER | IF
    TERM := FACTOR [* TERM]
    EXPRESSION_BLOCK := {STATEMENT* [EXPRESSION]}
    EXPRESSION :=  TERM [+ EXPRESSION] | EXPESSION_BLOCK
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
    module(&mut iter)
}

fn module(iter: &mut TokenIter) -> PResult {
    let mut functions = vec![];
    let mut coroutines = vec![];

    match iter.peek() {
        Some(Token { l, s: _ }) => {
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
                Some(AstNode::new(*l, Ast::Module(functions, coroutines)))
            } else {
                None
            })
        }
        None => Ok(None),
    }
}

fn function_def(iter: &mut TokenIter) -> PResult {
    let syntax = match consume_if(iter, Lex::FunctionDef) {
        Some(_) => match consume_if_id(iter) {
            Some((l, id)) => {
                let params = fn_def_params(iter)?;

                let fn_type = match consume_if(iter, Lex::LArrow) {
                    Some(l) => primitive(iter).ok_or(&format!(
                        "L{}: Expected primitive type after -> in function definition",
                        l
                    ))?,
                    _ => Primitive::Unit,
                };

                consume_must_be(iter, Lex::LBrace)?;
                let mut stmts = block(iter)?;

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
                    l,
                    Ast::FunctionDef(id.clone(), params, fn_type, stmts),
                ))
            }
            _ => return Err(format!("Expected function name after fn")),
        },
        _ => None,
    };
    Ok(syntax)
}

fn coroutine_def(iter: &mut TokenIter) -> PResult {
    let syntax = match consume_if(iter, Lex::CoroutineDef) {
        Some(_) => match consume_if_id(iter) {
            Some((l, id)) => {
                let params = fn_def_params(iter)?;

                let co_type = match consume_if(iter, Lex::LArrow) {
                    Some(l) => primitive(iter).expect(&format!(
                        "L{}: Expected primitive type after -> in function definition",
                        l
                    )),
                    _ => Primitive::Unit,
                };

                consume_must_be(iter, Lex::LBrace)?;
                let mut stmts = co_block(iter)?;

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
                    l,
                    Ast::CoroutineDef(id.clone(), params, co_type, stmts),
                ))
            }
            _ => return Err(format!("Expected function name after fn")),
        },
        _ => None,
    };
    Ok(syntax)
}

fn fn_def_params(iter: &mut TokenIter) -> Result<Vec<(String, Primitive)>, String> {
    consume_must_be(iter, Lex::LParen)?;

    let mut params = vec![];

    while let Some(param) = identifier_or_declare(iter)? {
        match param {
                AstNode {
                    l: _,
                    n: Ast::IdentifierDeclare(id, id_type),
                }
            => {
                params.push((id, id_type));
                consume_if(iter, Lex::Comma);
            }
            _ => {
                return Err(format!(
                    "invalid parameter declaration in function definition"
                ))
            }
        }
    }

    consume_must_be(iter, Lex::RParen)?;

    Ok(params)
}

fn block(iter: &mut TokenIter) -> Result<Vec<AstNode>, String> {
    let mut stmts = vec![];
    while iter.peek().is_some() {
        match statement(iter)? {
            Some(s) => stmts.push(s),
            None => break,
        }
    }
    Ok(stmts)
}

fn co_block(iter: &mut TokenIter) -> Result<Vec<AstNode>, String> {
    let mut stmts = vec![];
    while iter.peek().is_some() {
        match statement(iter)? {
            Some(s) => stmts.push(s),
            None => match yield_return_stmt(iter)? {
                Some(s) => stmts.push(s),
                None => break,
            },
        }
    }
    Ok(stmts)
}

fn return_stmt(iter: &mut TokenIter) -> PResult {
    Ok(match consume_if(iter, Lex::Return) {
        Some(l) => {
            let exp = expression(iter)?;
            consume_must_be(iter, Lex::Semicolon)?;
            match exp {
                Some(exp) => Some(AstNode::new(l, Ast::Return(Some(Box::new(exp))))),
                None => Some(AstNode::new(l, Ast::Return(None))),
            }
        }
        _ => None,
    })
}

fn yield_return_stmt(iter: &mut TokenIter) -> PResult {
    Ok(match consume_if(iter, Lex::YieldReturn) {
        Some(l) => {
            let exp = expression(iter)?;
            consume_must_be(iter, Lex::Semicolon)?;
            match exp {
                Some(exp) => Some(AstNode::new(l, Ast::YieldReturn(Some(Box::new(exp))))),
                None => Some(AstNode::new(l, Ast::YieldReturn(None))),
            }
        }
        _ => None,
    })
}

fn statement(iter: &mut TokenIter) -> PResult {
    let stm = match let_bind(iter)? {
        Some(b) => Some(b),
        None => match println_stmt(iter)? {
            Some(p) => Some(p),
            _ => None,
        },
    };

    match stm {
        Some(stm) => match consume_must_be(iter, Lex::Semicolon)? {
            Token{l, s:_} => {
                Ok(Some(AstNode::new(*l, Ast::Statement(Box::new(stm)))))
            }
        }
        None => Ok(None),
    }
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

fn let_bind(iter: &mut TokenIter) -> PResult {
    match consume_if(iter, Lex::Let) {
        Some(l) => {
            let id_decl = identifier_or_declare(iter)?.ok_or(format!("L{}: expected identifier after let", l))?;
            consume_must_be(iter, Lex::Assign)?;
            //let exp = expression(iter)?.ok_or(format!("L{}: expected expression on LHS of bind", l))?;
            let exp = match co_init(iter)? {
                Some(co_init) => co_init,
                None => expression(iter)?.ok_or(format!("L{}: expected expression on LHS of bind", l))?
            };
            Ok(Some(AstNode::new_bind(l, Box::new(id_decl), Box::new(exp))?))
        },
        None => Ok(None)
    }
}

fn co_init(iter: &mut TokenIter) -> PResult {
    match consume_if(iter, Lex::Init) {
        Some(l) => match consume_if_id(iter) {
            Some((l, id)) => {
                let params = fn_call_params(iter)?
                    .ok_or(&format!("L{}: Expected parameters after coroutine name", l))?;
                Ok(Some(AstNode::new(
                    l,
                    Ast::CoroutineInit(id.clone(), params),
                )))
            }
            None => Err(format!("L{}: expected identifier after init", l)),
        },
        _ => Ok(None),
    }
}

fn expression(iter: &mut TokenIter) -> PResult {
    logical_or(iter)
}

fn expression_block(iter: &mut TokenIter) -> PResult {
    match consume_if(iter, Lex::LBrace) {
        Some(l) =>{
            let mut stmts = block(iter)?;
            match iter.peek() {
                Some(Token{l:_, s: Lex::RBrace}) => {
                    ()
                }
                Some(_) => {
                    let exp = expression(iter)?.ok_or(format!("L{}: Expected expression at end of block", l))?;
                    stmts.push(exp);
                }
                None => {
                    return Err(format!("L{}: expected {}, but found EOF", l, Lex::RBrace))
                }
            };
            consume_must_be(iter, Lex::RBrace)?;
            Ok(Some(AstNode::new(l, Ast::ExpressionBlock(stmts))))
        },
        None => Ok(None)
    }
}

fn logical_or(iter: &mut TokenIter) -> PResult {
    Ok(match logical_and(iter)? {
        Some(n) => match consume_if(iter, Lex::BOr) {
            Some(l) => {
                let n2 = logical_or(iter)?.ok_or(&format!("L{}: An expression after ||", l))?;
                Some(AstNode::binary_op(l, &Lex::BOr, Box::new(n), Box::new(n2))?)
            }
            _ => Some(n),
        },
        None => None,
    })
}

fn logical_and(iter: &mut TokenIter) -> PResult {
    Ok(match comparison(iter)? {
        Some(n) => match consume_if(iter, Lex::BAnd) {
            Some(l) => {
                let n2 = logical_and(iter)?.ok_or(&format!("L{}: An expression after &&", l))?;
                Some(AstNode::binary_op(
                    l,
                    &Lex::BAnd,
                    Box::new(n),
                    Box::new(n2),
                )?)
            }
            _ => Some(n),
        },
        None => None,
    })
}

fn comparison(iter: &mut TokenIter) -> PResult {
    Ok(match sum(iter)? {
        Some(n) => match consume_if_one_of(
            iter,
            vec![Lex::Eq, Lex::NEq, Lex::Ls, Lex::LsEq, Lex::Gr, Lex::GrEq],
        ) {
            Some((l, op)) => {
                let n2 = comparison(iter)?.ok_or(&format!("L{}: An expression after {}", l, op))?;
                Some(AstNode::binary_op(l, &op, Box::new(n), Box::new(n2))?)
            }
            _ => Some(n),
        },
        None => None,
    })
}

fn sum(iter: &mut TokenIter) -> PResult {
    Ok(match term(iter)? {
        Some(n) => match consume_if(iter, Lex::Add) {
            Some(l) => {
                let n2 = sum(iter)?.ok_or(&format!("L{}: An expression after +", l))?;
                Some(AstNode::binary_op(l, &Lex::Add, Box::new(n), Box::new(n2))?)
            }
            _ => Some(n),
        },
        None => None,
    })
}

fn term(iter: &mut TokenIter) -> PResult {
    Ok(match factor(iter)? {
        Some(n) => match consume_if(iter, Lex::Mul) {
            Some(l) => {
                let n2 = term(iter)?.ok_or(&format!("L{}: a valid term after *", l))?;
                Some(AstNode::binary_op(l, &Lex::Mul, Box::new(n), Box::new(n2))?)
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
    Ok(match consume_if(iter, Lex::If) {
        Some(l) => {
            let cond = expression(iter)?.ok_or("Expected conditional expression after if")?;

            let true_arm = expression_block(iter)?.ok_or("Expression in true arm of if expression")?;
            consume_must_be(iter, Lex::Else)?;

            // check for `else if`
            let false_arm = match iter.peek() {
                Some(Token { l, s: Lex::If }) => if_expression(iter)?
                    .ok_or(format!("L{}: Expected if expression after else if", l))?,
                _ => {
                    let false_arm = expression_block(iter)?
                        .ok_or(&format!("L{}: Expression in false arm of if expression", l))?;
                    false_arm
                }
            };
            Some(AstNode::new(
                l,
                Ast::If(Box::new(cond), Box::new(true_arm), Box::new(false_arm)),
            ))
        }
        _ => None,
    })
}

/// LPAREN [EXPRESSION [, EXPRESSION]*] RPAREN
fn fn_call_params(iter: &mut TokenIter) -> Result<Option<Vec<AstNode>>, String> {
    match consume_if(iter, Lex::LParen) {
        Some(_) => {
            let mut params = vec![];
            while let Some(param) = expression(iter)? {
                match param {
                    exp => {
                        params.push(exp);
                        match consume_if(iter, Lex::Comma) {
                            Some(_) => {}
                            None => break,
                        };
                    }
                }
            }

            consume_must_be(iter, Lex::RParen)?;
            Ok(Some(params))
        }
        _ => Ok(None),
    }
}

fn co_yield(iter: &mut TokenIter) -> PResult {
    match consume_if(iter, Lex::Yield) {
        Some(l) => match consume_if_id(iter) {
            Some((ll, id)) => Ok(Some(AstNode::new_yield(l, ll, id))),
            _ => Err(format!("L{}: expected an identifier after yield", l)),
        },
        _ => Ok(None),
    }
}

fn function_call_or_variable(iter: &mut TokenIter) -> PResult {
    Ok(match consume_if_id(iter) {
        Some((l, id)) => match fn_call_params(iter)? {
            Some(params) => {
                // this is a function call
                Some(AstNode::new(l, Ast::FunctionCall(id, params)))
            }
            _ => Some(AstNode::new(l, Ast::Identifier(id))),
        },
        None => None,
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

fn identifier_or_declare(iter: &mut TokenIter) -> Result<Option<AstNode>, String> {
    Ok(match consume_if_id(iter) {
        Some((l, id)) => match consume_if(iter, Lex::Colon) {
            Some(l) => match primitive(iter) {
                Some(p) => Some(AstNode::new(l, Ast::IdentifierDeclare(id.clone(), p))),
                None => return Err(format!("L{}: Invalid primitive type: {:?}", l, iter.peek())),
            },
            _ => 
                Some(AstNode::new(l, Ast::Identifier(id.clone()))),
        },
        _ => None,
    })
}

fn constant(iter: &mut TokenIter) -> PResult {
    Ok(match number(iter)? {
        Some(i) => Some(i),
        None => match boolean(iter)? {
            Some(t) => Some(t),
            None => None,
        },
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

fn consume_if(iter: &mut TokenIter, test: Lex) -> Option<u32> {
    match iter.peek() {
        Some(tok) if tok.s == test => {
            let line = tok.l;
            iter.next();
            Some(line)
        }
        _ => None,
    }
}

fn consume_if_one_of(iter: &mut TokenIter, tests: Vec<Lex>) -> Option<(u32, Lex)> {
    match iter.peek() {
        Some(Token { l, s }) => {
            if tests.iter().find(|sym| *sym == s).is_some() {
                iter.next();
                Some((*l, s.clone()))
            } else {
                None
            }
        }
        None => None,
    }
}

fn must_be_one_of(iter: &mut TokenIter, tests: Vec<Lex>) -> Result<(u32, Lex), String> {
    match iter.peek() {
        Some(Token { l, s }) => {
            if tests.iter().find(|sym| *sym == s).is_some() {
                iter.next();
                Ok((*l, s.clone()))
            } else {
                Err(format!(
                    "L{}: expected one of {:?} but found {}",
                    l, tests, s
                ))
            }
        }
        None => Err(format!("Expected {:?} but found EOF", tests)),
    }
}

fn consume_if_id(iter: &mut TokenIter) -> Option<(u32, String)> {
    match iter.peek() {
        Some(Token {
            l,
            s: Lex::Identifier(id),
        }) => {
            iter.next();
            Some((*l, id.clone()))
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

#[cfg(test)]
pub mod tests {
    use super::lexer::Lexer;
    use super::*;

    #[test]
    fn parse_arithmetic_expression() {
        let mut lexer = Lexer::new();
        let text = "2 + 2";
        let tokens: Vec<Token> = lexer
            .tokenize(&text)
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut iter = tokens.iter().peekable();
        if let Some(AstNode {
            l,
            n: Ast::Add(left, right),
        }) = expression(&mut iter).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(left.n, Ast::Integer(2));
            assert_eq!(right.n, Ast::Integer(2));
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_nested_arithmetic_expression() {
        let mut lexer = Lexer::new();
        let text = "(2 + 4) * 3";
        let tokens: Vec<Token> = lexer
            .tokenize(&text)
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut iter = tokens.iter().peekable();
        if let Some(AstNode {
            l,
            n: Ast::Mul(left, right),
        }) = expression(&mut iter).unwrap()
        {
            assert_eq!(l, 1);
            match left.n {
                Ast::Add(ll, lr) => {
                    assert_eq!(ll.n, Ast::Integer(2));
                    assert_eq!(lr.n, Ast::Integer(4));
                }
                _ => panic!("Expected Add syntax"),
            }
            assert_eq!(right.n, Ast::Integer(3));
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_boolean_expression() {
        let mut lexer = Lexer::new();
        let text = "true || false";
        let tokens: Vec<Token> = lexer
            .tokenize(&text)
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut iter = tokens.iter().peekable();
        if let Some(AstNode {
            l,
            n: Ast::BOr(left, right),
        }) = expression(&mut iter).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(left.n, Ast::Boolean(true));
            assert_eq!(right.n, Ast::Boolean(false));
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_function_def() {
        let mut lexer = Lexer::new();
        let text = "fn test(x:i32) -> bool {return true;}";
        let tokens: Vec<Token> = lexer
            .tokenize(&text)
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut iter = tokens.iter().peekable();
        if let Some(AstNode {
            l,
            n: Ast::FunctionDef(name, params, ty, body),
        }) = function_def(&mut iter).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(name, "test");
            assert_eq!(params, vec![("x".into(), Primitive::I32)]);
            assert_eq!(ty, Primitive::Bool);
            assert_eq!(body.len(), 1);
            match &body[0].n {
                Ast::Return(Some(exp)) => {
                    assert_eq!(exp.n, Ast::Boolean(true));
                }
                _ => panic!("No body"),
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_expression_block_oneline() {
        let mut lexer = Lexer::new();
        let text = "{5}";
        let tokens: Vec<Token> = lexer
            .tokenize(&text)
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut iter = tokens.iter().peekable();
        if let Some(AstNode {
            l,
            n: Ast::ExpressionBlock(body),
        }) = expression_block(&mut iter).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(body.len(), 1);
            assert_eq!(body[0].n, Ast::Integer(5));
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_expression_block_bad() {
        let mut lexer = Lexer::new();
        let text = "{let x: i32 := 10 5}";
        let tokens: Vec<Token> = lexer
            .tokenize(&text)
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut iter = tokens.iter().peekable();
        assert_eq!(expression_block(&mut iter), Err("L1: Expected Semicolon, but found Integer(5)".into()));
    }

    #[test]
    fn parse_expression_block_missing_semicolons() {
        let mut lexer = Lexer::new();
        for (text,msg) in [
                    ("{5 10 51}", "L1: Expected RBrace, but found Integer(10)"),
                    ("{5; 10 51}", "L1: Expected RBrace, but found Semicolon"),
                ].iter() {
            let tokens: Vec<Token> = lexer
                .tokenize(&text)
                .into_iter()
                .collect::<Result<_, _>>()
                .unwrap();
            let mut iter = tokens.iter().peekable();
            assert_eq!(expression_block(&mut iter), Err((*msg).into()));
        }
    }

    #[test]
    fn parse_expression_block_multiline() {
        let mut lexer = Lexer::new();
        let text = "{let x:i32 := 5; x * x}";
        let tokens: Vec<Token> = lexer
            .tokenize(&text)
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut iter = tokens.iter().peekable();
        if let Some(AstNode {
            l,
            n: Ast::ExpressionBlock(body),
        }) = expression_block(&mut iter).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(body.len(), 2);
            match &body[0].n {
                Ast::Statement(stm) => {
                    match stm.as_ref() {
                        AstNode{l:_, n: Ast::Bind(id, p, exp)} => {
                            assert_eq!(id, "x");
                            assert_eq!(*p, Primitive::I32);
                            assert_eq!(exp.n, Ast::Integer(5));
                        },
                        _ => panic!("Not a binding statement"),
                    }
                }
                _ => panic!("No body: {:?}", &body[0].n),
            }
            match &body[1].n {
                Ast::Mul(l, r) => {
                    assert_eq!(l.n, Ast::Identifier("x".into()));
                    assert_eq!(r.n, Ast::Identifier("x".into()));
                }
                _ => panic!("No body: {:?}", &body[0].n),
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }
}
