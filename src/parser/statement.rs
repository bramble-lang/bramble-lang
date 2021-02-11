use super::{
    expression::expression,
    parser::{id_declaration, path, routine_call_params, ENABLE_TRACING, TRACE_END, TRACE_START},
};
use std::sync::atomic::Ordering;
use stdext::function_name;

use crate::{lexer::tokens::{Lex, Token}, syntax::{expression::{Expression, RoutineCall}, node::Node, statement::*}, trace};

use super::{
    parser::{ParserInfo, ParserResult},
    tokenstream::TokenStream,
};

pub(super) fn statement_or_yield_return(
    stream: &mut TokenStream,
) -> ParserResult<Statement<ParserInfo>> {
    let stm = match statement(stream)? {
        Some(n) => Some(n),
        None => match yield_return_stmt(stream)? {
            Some(yr) => Some(yr),
            None => None,
        },
    };

    Ok(stm)
}

pub(super) fn statement(stream: &mut TokenStream) -> ParserResult<Statement<ParserInfo>> {
    let start_index = stream.index();
    let must_have_semicolon = stream.test_if_one_of(vec![Lex::Let, Lex::Mut]);
    let stm = match let_bind(stream)? {
        Some(bind) => Some(Statement::Bind(Box::new(bind))),
        None => match mutate(stream)? {
            Some(mutate) => Some(Statement::Mutate(Box::new(mutate))),
            None => expression(stream)?
                .map(|s| Statement::from_ast(s))
                .flatten(),
        },
    };

    match stm {
        Some(stm) => match stream.next_if(&Lex::Semicolon) {
            Some(Token { s: _, .. }) => Ok(Some(stm)),
            _ => {
                if must_have_semicolon {
                    let line = *stm.annotation();
                    Err(format!(
                        "L{}: Expected ;, but found {}",
                        line,
                        match stream.peek() {
                            Some(x) => format!("{}", x.s),
                            None => "EOF".into(),
                        }
                    ))
                } else {
                    stream.set_index(start_index);
                    Ok(None)
                }
            }
        },
        None => {
            stream.set_index(start_index);
            Ok(None)
        }
    }
}

fn let_bind(stream: &mut TokenStream) -> ParserResult<Bind<ParserInfo>> {
    trace!(stream);
    match stream.next_if(&Lex::Let) {
        Some(token) => {
            let is_mutable = stream.next_if(&Lex::Mut).is_some();
            let id_decl = id_declaration(stream)?.ok_or(format!(
                "L{}: Expected identifier declaration (`<id> : <type>`) after let",
                token.l
            ))?;
            stream.next_must_be(&Lex::Assign)?;
            let exp = match co_init(stream)? {
                Some(co_init) => co_init,
                None => expression(stream)?
                    .ok_or(format!("L{}: expected expression on LHS of bind", token.l))?,
            };

            match id_decl {
                Expression::IdentifierDeclare(_, id, ty) => {
                    Ok(Some(Bind::new(token.l, &id, ty.clone(), is_mutable, exp)))
                }
                _ => Err(format!(
                    "L{}: Expected type specification after {}",
                    token.l,
                    id_decl.root_str()
                )),
            }
        }
        None => Ok(None),
    }
}

fn mutate(stream: &mut TokenStream) -> ParserResult<Mutate<ParserInfo>> {
    trace!(stream);
    match stream.next_ifn(vec![Lex::Mut, Lex::Identifier("".into()), Lex::Assign]) {
        None => Ok(None),
        Some(tokens) => {
            let id = tokens[1]
                .s
                .get_str()
                .expect("CRITICAL: identifier token cannot be converted to string");
            let exp = expression(stream)?.ok_or(format!(
                "L{}: expected expression on LHS of assignment",
                tokens[2].l
            ))?;
            //Expression::new_mutate(tokens[0].l, &id, Box::new(exp))
            Ok(Some(Mutate::new(tokens[0].l, &id, exp)))
        }
    }
}

fn co_init(stream: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
    trace!(stream);
    match stream.next_if(&Lex::Init) {
        Some(token) => match path(stream)? {
            Some((l, path)) => {
                let params = routine_call_params(stream)?
                    .ok_or(&format!("L{}: Expected parameters after coroutine name", l))?;
                Ok(Some(Expression::RoutineCall(
                    l,
                    RoutineCall::CoroutineInit,
                    path,
                    params,
                )))
            }
            None => Err(format!("L{}: expected identifier after init", token.l)),
        },
        _ => Ok(None),
    }
}

pub(super) fn return_stmt(stream: &mut TokenStream) -> ParserResult<Return<ParserInfo>> {
    trace!(stream);
    Ok(match stream.next_if(&Lex::Return) {
        Some(token) => {
            let exp = expression(stream)?;
            stream.next_must_be(&Lex::Semicolon)?;
            match exp {
                Some(exp) => Some(Return::new(token.l, Some(exp))),
                None => Some(Return::new(token.l, None)),
            }
        }
        _ => None,
    })
}

fn yield_return_stmt(stream: &mut TokenStream) -> ParserResult<Statement<ParserInfo>> {
    trace!(stream);
    Ok(match stream.next_if(&Lex::YieldReturn) {
        Some(token) => {
            let exp = expression(stream)?;
            stream.next_must_be(&Lex::Semicolon)?;
            let yret = match exp {
                Some(exp) => YieldReturn::new(token.l, Some(exp)),
                None => YieldReturn::new(token.l, None),
            };
            Some(Statement::YieldReturn(Box::new(yret)))
        }
        _ => None,
    })
}
