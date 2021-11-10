use super::{
    parser::{ENABLE_TRACING, TRACE_END, TRACE_START},
    Parser, ParserResult,
};
use std::sync::atomic::Ordering;
use stdext::function_name;

use crate::{
    compiler::{
        ast::*,
        diagnostics::{Event, View, ViewErr},
        lexer::tokens::Lex,
        source::SourceIr,
        CompilerError,
    },
    trace, StringId,
};

use super::{tokenstream::TokenStream, ParserContext, ParserError};

impl<'a> Parser<'a> {
    pub(super) fn statement_or_yield_return(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Statement<ParserContext>> {
        let stm = match self.statement(stream)? {
            Some(n) => Some(n),
            None => match self.yield_return_stmt(stream)? {
                Some(yr) => Some(yr),
                None => None,
            },
        };

        Ok(stm)
    }

    pub(super) fn statement(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Statement<ParserContext>> {
        trace!(stream);
        let start_index = stream.index();
        let must_have_semicolon = stream.test_if_one_of(vec![Lex::Let, Lex::Mut]);
        let stm = match self.let_bind(stream)? {
            Some(bind) => Some(Statement::Bind(Box::new(bind))),
            None => match self.mutate(stream)? {
                Some(mutate) => Some(Statement::Mutate(Box::new(mutate))),
                None => self
                    .expression(stream)?
                    .map(|s| Statement::from_ast(s))
                    .flatten(),
            },
        };

        match stm {
            Some(mut stm) => match stream.next_if(&Lex::Semicolon) {
                Some(semicolon) => {
                    let ctx = stm.context().join(semicolon.to_ctx());
                    *stm.get_context_mut() = ctx;
                    Ok(Some(stm))
                }
                _ => {
                    if must_have_semicolon {
                        err!(
                            stm.span(),
                            ParserError::ExpectedButFound(
                                vec![Lex::Semicolon],
                                stream.peek().map(|x| x.sym.clone())
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
        .view(|v| {
            let msg = match &v {
                Statement::Bind(..) => "Statement Bind",
                Statement::Expression(..) => "Statement Expression",
                Statement::Mutate(..) => "Statement Mutate",
                Statement::Return(..) => "Statement Return",
                Statement::YieldReturn(..) => "Statement Yield Return",
            };
            self.logger.write(Event::<ParserError> {
                stage: "parser",
                input: v.span(),
                msg: Ok(msg),
            });
        })
        .view_err(|err| {
            self.logger.write(Event::<ParserError> {
                stage: "parser",
                input: err.span(),
                msg: Err(&err),
            });
        })
    }

    fn let_bind(&self, stream: &mut TokenStream) -> ParserResult<Bind<ParserContext>> {
        trace!(stream);
        match stream.next_if(&Lex::Let) {
            Some(let_tok) => {
                let is_mutable = stream.next_if(&Lex::Mut).is_some();
                let id_decl = self
                    .id_declaration(stream)?
                    .ok_or(CompilerError::new(
                        let_tok.span(),
                        ParserError::ExpectedIdDeclAfterLet,
                    ))
                    .view_err(|err| {
                        self.logger.write(Event::<ParserError> {
                            stage: "parser",
                            input: err.span(),
                            msg: Err(&err),
                        });
                    })?;
                stream.next_must_be(&Lex::Assign)?;

                let exp = match self.co_init(stream)? {
                    Some(co_init) => co_init,
                    None => self
                        .expression(stream)?
                        .ok_or(CompilerError::new(
                            let_tok.span(),
                            ParserError::ExpectedExpressionOnRhs,
                        ))
                        .view_err(|err| {
                            self.logger.write(Event::<ParserError> {
                                stage: "parser",
                                input: err.span(),
                                msg: Err(&err),
                            });
                        })?,
                };
                let ctx = exp.context().join(let_tok.to_ctx());

                match id_decl {
                    Expression::IdentifierDeclare(_, id, ty) => {
                        Ok(Some(Bind::new(ctx, id, ty.clone(), is_mutable, exp)))
                    }
                    _ => Err(CompilerError::new(
                        let_tok.span(),
                        ParserError::ExpectedTypeInIdDecl,
                    )),
                }
                .view(|v| {
                    self.logger.write(Event::<ParserError> {
                        stage: "parser",
                        input: v.span(),
                        msg: Ok("Let Binding"),
                    });
                })
                .view_err(|err| {
                    self.logger.write(Event::<ParserError> {
                        stage: "parser",
                        input: err.span(),
                        msg: Err(&err),
                    });
                })
            }
            None => Ok(None),
        }
    }

    fn mutate(&self, stream: &mut TokenStream) -> ParserResult<Mutate<ParserContext>> {
        trace!(stream);
        match stream.next_ifn(vec![
            Lex::Mut,
            Lex::Identifier(StringId::new()),
            Lex::Assign,
        ]) {
            None => Ok(None),
            Some(tokens) => {
                let id = tokens[1]
                    .sym
                    .get_str()
                    .expect("CRITICAL: identifier token cannot be converted to string");
                let exp = self
                    .expression(stream)?
                    .ok_or(CompilerError::new(
                        tokens[2].span(),
                        ParserError::ExpectedExpressionOnRhs,
                    ))
                    .view_err(|err| {
                        self.logger.write(Event::<ParserError> {
                            stage: "parser",
                            input: err.span(),
                            msg: Err(&err),
                        });
                    })?;
                Ok(Some(Mutate::new(tokens[0].to_ctx(), id, exp))).view(|v| {
                    self.logger.write(Event::<ParserError> {
                        stage: "parser",
                        input: v.span(),
                        msg: Ok("Mutate"),
                    });
                })
            }
        }
    }

    fn co_init(&self, stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
        trace!(stream);
        match stream.next_if(&Lex::Init) {
            Some(init_tok) => match self.path(stream)? {
                Some((path, path_ctx)) => self
                    .routine_call_params(stream)?
                    .ok_or(CompilerError::new(
                        path_ctx.span(),
                        ParserError::ExpectedParams,
                    ))
                    .and_then(|(params, params_ctx)| {
                        Ok(Some(Expression::RoutineCall(
                            init_tok.to_ctx().join(params_ctx),
                            RoutineCall::CoroutineInit,
                            path,
                            params,
                        )))
                    }),
                None => Err(CompilerError::new(
                    init_tok.span(),
                    ParserError::ExpectedIdAfterInit,
                )),
            },
            _ => Ok(None),
        }
        .view(|v| {
            self.logger.write(Event::<ParserError> {
                stage: "parser",
                input: v.span(),
                msg: Ok("Coroutine Init"),
            });
        })
        .view_err(|err| {
            self.logger.write(Event::<ParserError> {
                stage: "parser",
                input: err.span(),
                msg: Err(&err),
            });
        })
    }

    pub(super) fn return_stmt(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Return<ParserContext>> {
        trace!(stream);
        Ok(match stream.next_if(&Lex::Return) {
            Some(token) => {
                let exp = self.expression(stream)?;
                stream.next_must_be(&Lex::Semicolon)?;
                match exp {
                    Some(exp) => Some(Return::new(token.to_ctx(), Some(exp))),
                    None => Some(Return::new(token.to_ctx(), None)),
                }
            }
            _ => None,
        })
        .view(|v| {
            self.logger.write(Event::<ParserError> {
                stage: "parser",
                input: v.span(),
                msg: Ok("Return"),
            });
        })
    }

    fn yield_return_stmt(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Statement<ParserContext>> {
        trace!(stream);
        Ok(match stream.next_if(&Lex::YieldReturn) {
            Some(token) => {
                let exp = self.expression(stream)?;
                stream.next_must_be(&Lex::Semicolon)?;
                let yret = match exp {
                    Some(exp) => YieldReturn::new(token.to_ctx(), Some(exp)),
                    None => YieldReturn::new(token.to_ctx(), None),
                };
                Some(Statement::YieldReturn(Box::new(yret)))
            }
            _ => None,
        })
        .view(|v| {
            self.logger.write(Event::<ParserError> {
                stage: "parser",
                input: v.span(),
                msg: Ok("Yield Return"),
            });
        })
    }
}
