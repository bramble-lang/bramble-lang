// AST - a type(s) which is used to construct an AST representing the logic of the
// program
// Each type of node represents an expression and the only requirement is that at the
// end of computing an expression its result is in EAX
use super::ast::*;
use crate::lexer::{self, Lex};
use crate::Token;

type PResult = Result<Option<PNode>, String>;
type TokenIter<'a> = std::iter::Peekable<core::slice::Iter<'a, Token>>;

type ParserInfo = u32;
pub type PNode = Ast<ParserInfo>;

impl PNode {
    pub fn new_yield(line: u32, id_line: u32, id: String) -> Self {
        let i = line; //ParserInfo{l: line};
        let i_id = id_line; //ParserInfo{l: id_line};
        Ast::Yield(i, Box::new(Ast::Identifier(i_id, id)))
    }

    pub fn new_bind(line: u32, id: Box<Self>, exp: Box<Self>) -> Result<Self, String> {
        let i = line; //ParserInfo{l: line};
        match id.as_ref() {
            Ast::IdentifierDeclare(_, id, prim) => Ok(Ast::Bind(i, id.clone(), *prim, exp)),
            _ => Err(format!(
                "L{}: Expected type specification after {}",
                line,
                id.root_str()
            )),
        }
    }

    pub fn binary_op(
        line: u32,
        op: &Lex,
        left: Box<Self>,
        right: Box<Self>,
    ) -> Result<Self, String> {
        let i = line; //ParserInfo{l: line};
        match op {
            Lex::Eq => Ok(Ast::Eq(i, left, right)),
            Lex::NEq => Ok(Ast::NEq(i, left, right)),
            Lex::Ls => Ok(Ast::Ls(i, left, right)),
            Lex::LsEq => Ok(Ast::LsEq(i, left, right)),
            Lex::Gr => Ok(Ast::Gr(i, left, right)),
            Lex::GrEq => Ok(Ast::GrEq(i, left, right)),
            //Lex::BAnd => Ok(Ast::BAnd(i, left, right)),
            Lex::BAnd => Ok(Ast::BinaryOp(i, BinaryOperator::BAnd, left, right)),
            //Lex::BOr => Ok(Ast::BOr(i, left, right)),
            Lex::BOr => Ok(Ast::BinaryOp(i, BinaryOperator::BOr, left, right)),
            //Lex::Add => Ok(Ast::Add(i, left, right)),
            Lex::Add => Ok(Ast::BinaryOp(i, BinaryOperator::Add, left, right)),
            //Lex::Mul => Ok(Ast::Mul(i, left, right)),
            Lex::Mul => Ok(Ast::BinaryOp(i, BinaryOperator::Mul, left, right)),
            _ => Err(format!("L{}: {} is not a binary operator", line, op)),
        }
    }
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
    module(&mut iter).map_err(|e| format!("Parser: {}", e))
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
                Some(Ast::Module(*l, functions, coroutines))
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

                Some(Ast::FunctionDef(l, id.clone(), params, fn_type, stmts))
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

                Some(Ast::CoroutineDef(l, id.clone(), params, co_type, stmts))
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
            Ast::IdentifierDeclare(_, id, id_type) => {
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

fn block(iter: &mut TokenIter) -> Result<Vec<PNode>, String> {
    let mut stmts = vec![];
    while iter.peek().is_some() {
        match statement(iter)? {
            Some(s) => stmts.push(s),
            None => break,
        }
    }
    Ok(stmts)
}

fn co_block(iter: &mut TokenIter) -> Result<Vec<PNode>, String> {
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
                Some(exp) => Some(Ast::Return(l, Some(Box::new(exp)))),
                None => Some(Ast::Return(l, None)),
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
                Some(exp) => Some(Ast::YieldReturn(l, Some(Box::new(exp)))),
                None => Some(Ast::YieldReturn(l, None)),
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
            Token { l, s: _ } => Ok(Some(Ast::Statement(*l, Box::new(stm)))),
        },
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
                Lex::Printiln => Some(Ast::Printiln(*l, Box::new(exp))),
                Lex::Printbln => Some(Ast::Printbln(*l, Box::new(exp))),
                _ => panic!("CRITICAL: already tested for a print token but found {}", s),
            }
        }
        _ => None,
    })
}

fn let_bind(iter: &mut TokenIter) -> PResult {
    match consume_if(iter, Lex::Let) {
        Some(l) => {
            let id_decl = identifier_or_declare(iter)?
                .ok_or(format!("L{}: expected identifier after let", l))?;
            consume_must_be(iter, Lex::Assign)?;
            //let exp = expression(iter)?.ok_or(format!("L{}: expected expression on LHS of bind", l))?;
            let exp = match co_init(iter)? {
                Some(co_init) => co_init,
                None => expression(iter)?
                    .ok_or(format!("L{}: expected expression on LHS of bind", l))?,
            };
            Ok(Some(PNode::new_bind(l, Box::new(id_decl), Box::new(exp))?))
        }
        None => Ok(None),
    }
}

fn co_init(iter: &mut TokenIter) -> PResult {
    match consume_if(iter, Lex::Init) {
        Some(l) => match consume_if_id(iter) {
            Some((l, id)) => {
                let params = fn_call_params(iter)?
                    .ok_or(&format!("L{}: Expected parameters after coroutine name", l))?;
                Ok(Some(Ast::CoroutineInit(l, id.clone(), params)))
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
        Some(l) => {
            let mut stmts = block(iter)?;
            match iter.peek() {
                Some(Token {
                    l: _,
                    s: Lex::RBrace,
                }) => (),
                Some(_) => {
                    let exp = expression(iter)?
                        .ok_or(format!("L{}: Expected expression at end of block", l))?;
                    stmts.push(exp);
                }
                None => return Err(format!("L{}: expected {}, but found EOF", l, Lex::RBrace)),
            };
            consume_must_be(iter, Lex::RBrace)?;
            Ok(Some(Ast::ExpressionBlock(l, stmts)))
        }
        None => Ok(None),
    }
}

fn logical_or(iter: &mut TokenIter) -> PResult {
    Ok(match logical_and(iter)? {
        Some(n) => match consume_if(iter, Lex::BOr) {
            Some(l) => {
                let n2 = logical_or(iter)?.ok_or(&format!("L{}: An expression after ||", l))?;
                Some(PNode::binary_op(l, &Lex::BOr, Box::new(n), Box::new(n2))?)
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
                Some(PNode::binary_op(l, &Lex::BAnd, Box::new(n), Box::new(n2))?)
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
                Some(PNode::binary_op(l, &op, Box::new(n), Box::new(n2))?)
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
                Some(PNode::binary_op(l, &Lex::Add, Box::new(n), Box::new(n2))?)
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
                Some(PNode::binary_op(l, &Lex::Mul, Box::new(n), Box::new(n2))?)
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

            let true_arm =
                expression_block(iter)?.ok_or("Expression in true arm of if expression")?;
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
            Some(Ast::If(
                l,
                Box::new(cond),
                Box::new(true_arm),
                Box::new(false_arm),
            ))
        }
        _ => None,
    })
}

/// LPAREN [EXPRESSION [, EXPRESSION]*] RPAREN
fn fn_call_params(iter: &mut TokenIter) -> Result<Option<Vec<PNode>>, String> {
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
            Some((ll, id)) => Ok(Some(PNode::new_yield(l, ll, id))),
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
                Some(Ast::FunctionCall(l, id, params))
            }
            _ => Some(Ast::Identifier(l, id)),
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

fn identifier_or_declare(iter: &mut TokenIter) -> Result<Option<PNode>, String> {
    Ok(match consume_if_id(iter) {
        Some((l, id)) => match consume_if(iter, Lex::Colon) {
            Some(l) => match primitive(iter) {
                Some(p) => Some(Ast::IdentifierDeclare(l, id.clone(), p)),
                None => return Err(format!("L{}: Invalid primitive type: {:?}", l, iter.peek())),
            },
            _ => Some(Ast::Identifier(l, id.clone())),
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
                Some(Ast::Integer(*l, *i))
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
            Ok(Some(Ast::Boolean(*l, *b)))
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
                    "L{}: expected one of {} but found {}",
                    l,
                    tests
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join(","),
                    s
                ))
            }
        }
        None => Err(format!(
            "Expected {} but found EOF",
            tests
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<String>>()
                .join(",")
        )),
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
        Some(Token { l, s }) => Err(format!("L{}: Expected {}, but found {}", l, test, s)),
        None => Err(format!("Expected {}, but found EOF", test)),
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
        if let Some(Ast::BinaryOp(l, BinaryOperator::Add, left, right)) = expression(&mut iter).unwrap() {
            assert_eq!(l, 1);
            assert_eq!(*left, Ast::Integer(1, 2));
            assert_eq!(*right, Ast::Integer(1, 2));
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
        if let Some(Ast::BinaryOp(l, BinaryOperator::Mul, left, right)) = expression(&mut iter).unwrap() {
            assert_eq!(l, 1);
            match left.as_ref() {
                Ast::BinaryOp(_, BinaryOperator::Add, ll, lr) => {
                    assert_eq!(**ll, Ast::Integer(1, 2));
                    assert_eq!(**lr, Ast::Integer(1, 4));
                }
                _ => panic!("Expected Add syntax"),
            }
            assert_eq!(*right, Ast::Integer(1, 3));
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
        if let Some(Ast::BinaryOp(l, BinaryOperator::BOr, left, right)) = expression(&mut iter).unwrap() {
            assert_eq!(l, 1);
            assert_eq!(*left, Ast::Boolean(1, true));
            assert_eq!(*right, Ast::Boolean(1, false));
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
        if let Some(Ast::FunctionDef(l, name, params, ty, body)) = function_def(&mut iter).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(name, "test");
            assert_eq!(params, vec![("x".into(), Primitive::I32)]);
            assert_eq!(ty, Primitive::Bool);
            assert_eq!(body.len(), 1);
            match &body[0] {
                Ast::Return(_, Some(exp)) => {
                    assert_eq!(*exp.as_ref(), Ast::Boolean(1, true));
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
        if let Some(Ast::ExpressionBlock(l, body)) = expression_block(&mut iter).unwrap() {
            assert_eq!(l, 1);
            assert_eq!(body.len(), 1);
            assert_eq!(body[0], Ast::Integer(1, 5));
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_expression_block_bad() {
        let mut lexer = Lexer::new();
        for (text, msg) in [
            ("{5 10 51}", "L1: Expected }, but found literal 10"),
            ("{5; 10 51}", "L1: Expected }, but found ;"),
            ("{5; 10 let x:i32 := 5}", "L1: Expected }, but found ;"),
            (
                "{let x: i32 := 10 5}",
                "L1: Expected ;, but found literal 5",
            ),
        ]
        .iter()
        {
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
        if let Some(Ast::ExpressionBlock(l, body)) = expression_block(&mut iter).unwrap() {
            assert_eq!(l, 1);
            assert_eq!(body.len(), 2);
            match &body[0] {
                Ast::Statement(_, stm) => match stm.as_ref() {
                    Ast::Bind(_, id, p, exp) => {
                        assert_eq!(id, "x");
                        assert_eq!(*p, Primitive::I32);
                        assert_eq!(*exp, Box::new(PNode::Integer(1, 5)));
                    }
                    _ => panic!("Not a binding statement"),
                },
                _ => panic!("No body: {:?}", &body[0]),
            }
            match &body[1] {
                Ast::BinaryOp(_, BinaryOperator::Mul, l, r) => {
                    assert_eq!(*l.as_ref(), Ast::Identifier(1, "x".into()));
                    assert_eq!(*r.as_ref(), Ast::Identifier(1, "x".into()));
                }
                _ => panic!("No body: {:?}", &body[0]),
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }
}
