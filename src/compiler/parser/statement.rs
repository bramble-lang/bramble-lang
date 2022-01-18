use super::{Parser, ParserResult};
use crate::{
    compiler::{
        ast::*, diagnostics::View2, lexer::tokens::Lex, source::SourceIr, CompilerError, Span,
    },
    StringId,
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
        let (event, result) = self.new_event(Span::zero()).and_then(|| {
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
        });
        result.view(|v| {
            let msg = v.map(|v| match &v {
                Statement::Bind(..) => "Statement Bind",
                Statement::Expression(..) => "Statement Expression",
                Statement::Mutate(..) => "Statement Mutate",
                Statement::Return(..) => "Statement Return",
                Statement::YieldReturn(..) => "Statement Yield Return",
            });
            self.record(event.with_span(v.span()), msg)
        })
    }

    fn let_bind(&self, stream: &mut TokenStream) -> ParserResult<Bind<ParserContext>> {
        let (event, result) =
            self.new_event(Span::zero())
                .and_then(|| match stream.next_if(&Lex::Let) {
                    Some(let_tok) => {
                        let is_mutable = stream.next_if(&Lex::Mut).is_some();
                        self.id_declaration(stream)?
                            .ok_or(CompilerError::new(
                                let_tok.span(),
                                ParserError::ExpectedIdDeclAfterLet,
                            ))
                            .and_then(|id_decl| {
                                stream.next_must_be(&Lex::Assign)?;

                                let exp = match self.co_init(stream)? {
                                    Some(co_init) => co_init,
                                    None => self.expression(stream)?.ok_or(CompilerError::new(
                                        let_tok.span(),
                                        ParserError::ExpectedExpressionOnRhs,
                                    ))?,
                                };

                                match id_decl {
                                    Expression::IdentifierDeclare(_, id, ty) => {
                                        let ctx = exp.context().join(let_tok.to_ctx());
                                        Ok(Some(Bind::new(ctx, id, ty.clone(), is_mutable, exp)))
                                    }
                                    _ => Err(CompilerError::new(
                                        let_tok.span(),
                                        ParserError::ExpectedTypeInIdDecl,
                                    )),
                                }
                            })
                    }
                    None => Ok(None),
                });
        result.view(|v| {
            let msg = v.map(|_| "Let Binding");
            self.record(event.with_span(v.span()), msg)
        })
    }

    fn mutate(&self, stream: &mut TokenStream) -> ParserResult<Mutate<ParserContext>> {
        let (event, result) = self.new_event(Span::zero()).and_then(|| {
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
                    self.expression(stream)?
                        .ok_or(CompilerError::new(
                            tokens[2].span(),
                            ParserError::ExpectedExpressionOnRhs,
                        ))
                        .and_then(|exp| {
                            let ctx = tokens[0].to_ctx().join(*exp.context());
                            Ok(Some(Mutate::new(ctx, id, exp)))
                        })
                }
            }
        });
        result.view(|v| {
            let msg = v.map(|_| "Mutate");
            self.record(event.with_span(v.span()), msg)
        })
    }

    fn co_init(&self, stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
        let (event, result) =
            self.new_event(Span::zero())
                .and_then(|| match stream.next_if(&Lex::Init) {
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
                });
        result.view(|v| {
            let msg = v.map(|_| "Coroutine Init");
            self.record(event.with_span(v.span()), msg)
        })
    }

    pub(super) fn return_stmt(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Return<ParserContext>> {
        let (event, result) = self.new_event(Span::zero()).and_then(|| {
            Ok(match stream.next_if(&Lex::Return) {
                Some(token) => {
                    let exp = self.expression(stream)?;
                    stream.next_must_be(&Lex::Semicolon)?;
                    match exp {
                        Some(exp) => {
                            let ctx = token.to_ctx().join(*exp.context());
                            Some(Return::new(ctx, Some(exp)))
                        }
                        None => Some(Return::new(token.to_ctx(), None)),
                    }
                }
                _ => None,
            })
        });
        result.view(|v| {
            let msg = v.map(|_| "Return");
            self.record(event.with_span(v.span()), msg)
        })
    }

    fn yield_return_stmt(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Statement<ParserContext>> {
        let (event, result) = self.new_event(Span::zero()).and_then(|| {
            Ok(match stream.next_if(&Lex::YieldReturn) {
                Some(token) => {
                    let exp = self.expression(stream)?;
                    stream.next_must_be(&Lex::Semicolon)?;
                    let yret = match exp {
                        Some(exp) => {
                            let ctx = token.to_ctx().join(*exp.context());
                            YieldReturn::new(ctx, Some(exp))
                        }
                        None => YieldReturn::new(token.to_ctx(), None),
                    };
                    Some(Statement::YieldReturn(Box::new(yret)))
                }
                _ => None,
            })
        });
        result.view(|v| {
            let msg = v.map(|_| "Yield Return");
            self.record(event.with_span(v.span()), msg)
        })
    }
}
