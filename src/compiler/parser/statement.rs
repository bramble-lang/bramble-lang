use super::{
    expression::expression,
    parser::{id_declaration, path, routine_call_params, ENABLE_TRACING, TRACE_END, TRACE_START},
    ParserResult,
};
use std::sync::atomic::Ordering;
use stdext::function_name;

use crate::{
    compiler::{ast::*, lexer::tokens::Lex, CompilerError},
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
        Some(mut stm) => match stream.next_if(&Lex::Semicolon) {
            Some(semicolon) => {
                let ctx = stm.get_context().join(semicolon.to_ctx());
                *stm.get_context_mut() = ctx;
                Ok(Some(stm))
            }
            _ => {
                if must_have_semicolon {
                    err!(
                        stm.get_context().line(),
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

            let ctx = exp.get_context().join(token.to_ctx());

            match id_decl {
                Expression::IdentifierDeclare(_, id, ty) => {
                    Ok(Some(Bind::new(ctx, id, ty.clone(), is_mutable, exp)))
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
            Ok(Some(Mutate::new(tokens[0].to_ctx(), id, exp)))
        }
    }
}

fn co_init(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    match stream.next_if(&Lex::Init) {
        Some(init_tok) => match path(stream)? {
            Some((path, path_ctx)) => {
                let (params, params_ctx) = routine_call_params(stream)?.ok_or(
                    CompilerError::new(path_ctx.line(), ParserError::ExpectedParams),
                )?;
                Ok(Some(Expression::RoutineCall(
                    init_tok.to_ctx().join(params_ctx),
                    RoutineCall::CoroutineInit,
                    path,
                    params,
                )))
            }
            None => Err(CompilerError::new(
                init_tok.l,
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
                Some(exp) => Some(Return::new(token.to_ctx(), Some(exp))),
                None => Some(Return::new(token.to_ctx(), None)),
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
                Some(exp) => YieldReturn::new(token.to_ctx(), Some(exp)),
                None => YieldReturn::new(token.to_ctx(), None),
            };
            Some(Statement::YieldReturn(Box::new(yret)))
        }
        _ => None,
    })
}
