use super::{
    expression::expression,
    parser::{id_declaration, path, routine_call_params, ENABLE_TRACING, TRACE_END, TRACE_START},
    ParserResult,
};
use std::sync::atomic::Ordering;
use stdext::function_name;

use crate::{
    compiler::{
        ast::*,
        lexer::tokens::{Lex, Token},
        CompilerError,
    },
    trace, StringId,
};

use super::{tokenstream::TokenStream, ParserContext, ParserError};

pub(super) fn statement_or_yield_return(
    stream: &mut TokenStream,
) -> ParserResult<Statement<ParserContext>> {
    let stm = match statement(stream)? {
        Some(n) => Some(n),
        None => match yield_return_stmt(stream)? {
            Some(yr) => Some(yr),
            None => None,
        },
    };

    Ok(stm)
}

pub(super) fn statement(stream: &mut TokenStream) -> ParserResult<Statement<ParserContext>> {
    trace!(stream);
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
                    let line = *stm.get_context();
                    err!(
                        line,
                        ParserError::ExpectedButFound(
                            vec![Lex::Semicolon],
                            stream.peek().map(|x| x.s.clone())
                        )
                    )
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

fn let_bind(stream: &mut TokenStream) -> ParserResult<Bind<ParserContext>> {
    trace!(stream);
    match stream.next_if(&Lex::Let) {
        Some(token) => {
            let is_mutable = stream.next_if(&Lex::Mut).is_some();
            let id_decl = id_declaration(stream)?.ok_or(CompilerError::new(
                token.l,
                ParserError::ExpectedIdDeclAfterLet,
            ))?;
            stream.next_must_be(&Lex::Assign)?;
            let exp = match co_init(stream)? {
                Some(co_init) => co_init,
                None => expression(stream)?.ok_or(CompilerError::new(
                    token.l,
                    ParserError::ExpectedExpressionOnRhs,
                ))?,
            };

            match id_decl {
                Expression::IdentifierDeclare(_, id, ty) => {
                    Ok(Some(Bind::new(token.l, id, ty.clone(), is_mutable, exp)))
                }
                _ => Err(CompilerError::new(
                    token.l,
                    ParserError::ExpectedTypeInIdDecl,
                )),
            }
        }
        None => Ok(None),
    }
}

fn mutate(stream: &mut TokenStream) -> ParserResult<Mutate<ParserContext>> {
    trace!(stream);
    match stream.next_ifn(vec![
        Lex::Mut,
        Lex::Identifier(StringId::new()),
        Lex::Assign,
    ]) {
        None => Ok(None),
        Some(tokens) => {
            let id = tokens[1]
                .s
                .get_str()
                .expect("CRITICAL: identifier token cannot be converted to string");
            let exp = expression(stream)?.ok_or(CompilerError::new(
                tokens[2].l,
                ParserError::ExpectedExpressionOnRhs,
            ))?;
            Ok(Some(Mutate::new(tokens[0].l, id, exp)))
        }
    }
}

fn co_init(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    match stream.next_if(&Lex::Init) {
        Some(token) => match path(stream)? {
            Some((l, path)) => {
                let params = routine_call_params(stream)?
                    .ok_or(CompilerError::new(l, ParserError::ExpectedParams))?;
                Ok(Some(Expression::RoutineCall(
                    l,
                    RoutineCall::CoroutineInit,
                    path,
                    params,
                )))
            }
            None => Err(CompilerError::new(
                token.l,
                ParserError::ExpectedIdAfterInit,
            )),
        },
        _ => Ok(None),
    }
}

pub(super) fn return_stmt(stream: &mut TokenStream) -> ParserResult<Return<ParserContext>> {
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

fn yield_return_stmt(stream: &mut TokenStream) -> ParserResult<Statement<ParserContext>> {
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
