use crate::compiler::diagnostics::{Event, View2, Writable};
use crate::compiler::source::SourceIr;
use crate::compiler::Span;
use crate::compiler::{
    ast::*,
    lexer::tokens::{Lex, Primitive, Token},
    CompilerError,
};
use crate::StringId;

use super::{ctx_over_tokens, Parser, ParserContext};
// AST - a type(s) which is used to construct an AST representing the logic of the
// program
// Each type of node represents an expression and the only requirement is that at the
// end of computing an expression its result is in EAX
use super::ParserResult;
use super::{tokenstream::TokenStream, ParserError};

type HasVarArgs = bool;

impl<'a> Parser<'a> {
    pub(super) fn new_event<'e>(&self, span: Span) -> Event<'e, &'e str, ParserError> {
        Event::new("parser", span, self.event_stack.clone())
    }

    /// Support function which records parser events to the tracing system
    pub(super) fn record<V: Writable>(
        &self,
        evt: Event<V, ParserError>,
        r: Result<V, &CompilerError<ParserError>>,
    ) {
        let evt = evt.with_msg(r);
        self.logger.write(evt)
    }

    /// Support function which records parser events to the tracing system
    pub(super) fn record_noop<V: Writable>(&self, evt: Event<V, ParserError>) {
        self.logger.write(evt)
    }

    /// Support function which records parser events to the tracing system
    pub(super) fn record_terminal(&self, span: Span, r: Result<&str, &CompilerError<ParserError>>) {
        self.logger.write(Event::new_with_result(
            "parser",
            span,
            r,
            self.event_stack.clone(),
        ))
    }

    pub fn parse(
        &self,
        name: StringId,
        tokens: &Vec<Token>,
    ) -> ParserResult<Module<ParserContext>> {
        // Create the module that represents the source code unit as a whole (usually the file)
        // give it span that covers the entire set of tokens
        let (file_module_event, result) = self.new_event(Span::zero()).and_then(|| {
            ctx_over_tokens(&tokens)
                .ok_or_else(|| CompilerError::new(Span::zero(), ParserError::EmptyProject))
                .and_then(|module_ctx| {
                    let mut module = Module::new(name, module_ctx);

                    // Create the token stream.
                    let mut stream = TokenStream::new(&tokens, self.logger).ok_or_else(|| {
                        CompilerError::new(Span::zero(), ParserError::EmptyProject)
                    })?;

                    while stream.peek().is_some() {
                        let start_index = stream.index();
                        self.parse_items_into(&mut stream, &mut module)?;

                        if stream.index() == start_index {
                            return err!(
                                stream.peek().unwrap().span(),
                                ParserError::Locked(stream.peek().map(|t| t.clone()))
                            );
                        }
                    }

                    Ok(Some(module))
                })
        });
        result.view(|v| {
            let msg = v.map(|_| "File Module");
            self.record(file_module_event.with_span(v.span()), msg)
        })
    }

    fn module(&self, stream: &mut TokenStream) -> ParserResult<Module<ParserContext>> {
        let (event, result) =
            self.new_event(Span::zero())
                .and_then(|| match stream.next_if(&Lex::ModuleDef) {
                    Some(module) => match stream.next_if_id() {
                        Some((module_name, _)) => {
                            let mut module = Module::new(module_name, module.to_ctx());
                            stream.next_must_be(&Lex::LBrace)?;

                            self.parse_items_into(stream, &mut module)?;

                            let ctx = stream
                                .next_must_be(&Lex::RBrace)?
                                .to_ctx()
                                .join(*module.context());
                            *module.get_context_mut() = ctx;
                            Ok(Some(module))
                        }
                        _ => err!(module.span(), ParserError::ModExpectedName),
                    },
                    None => Ok(None),
                });
        result.view(|v| {
            let msg = v.map(|_| "Module");
            self.record(event.with_span(v.span()), msg)
        })
    }

    fn parse_items_into(
        &self,
        stream: &mut TokenStream,
        module: &mut Module<ParserContext>,
    ) -> ParserResult<()> {
        if let Some((submods, items)) = self.parse_items(stream)? {
            for sm in submods {
                module.add_module(sm);
            }

            for item in items {
                module.add_item(item)?;
            }
        }

        Ok(Some(()))
    }

    fn parse_items(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<(Vec<Module<ParserContext>>, Vec<Item<ParserContext>>)> {
        let mut modules = vec![];
        let mut items = vec![];
        while stream.peek().is_some() {
            let start_index = stream.index();
            if let Some(m) = self.module(stream)? {
                modules.push(m);
            }

            if let Some(f) = self.function_def(stream)? {
                items.push(Item::Routine(f));
            }

            if let Some(s) = self.struct_def(stream)? {
                items.push(Item::Struct(s));
            }

            if let Some(e) = self.extern_def(stream)? {
                items.push(Item::Extern(e));
            }

            if stream.index() == start_index {
                break;
            }
        }

        if modules.is_empty() && items.is_empty() {
            Ok(None)
        } else {
            Ok(Some((modules, items)))
        }
    }

    fn extern_def(&self, stream: &mut TokenStream) -> ParserResult<Extern<ParserContext>> {
        let (event, result) =
            self.new_event(Span::zero())
                .and_then(|| match stream.next_if(&Lex::Extern) {
                    Some(extern_tok) => match self.function_decl(stream, true)? {
                        Some((fn_ctx, fn_name, params, has_varargs, fn_type)) => {
                            if has_varargs && params.is_empty() {
                                err!(fn_ctx.span(), ParserError::ExternInvalidVarArgs)
                            } else {
                                let ctx = stream
                                    .next_must_be(&Lex::Semicolon)?
                                    .to_ctx()
                                    .join(extern_tok.to_ctx());
                                Ok(Some(Extern::new(
                                    fn_name,
                                    ctx,
                                    params,
                                    has_varargs,
                                    fn_type,
                                )))
                            }
                        }
                        None => err!(extern_tok.span(), ParserError::ExternExpectedFnDecl),
                    },
                    None => Ok(None),
                });
        result.view(|v| {
            let msg = v.map(|_| "Extern Definition");
            self.record(event.with_span(v.span()), msg)
        })
    }

    fn struct_def(&self, stream: &mut TokenStream) -> ParserResult<StructDef<ParserContext>> {
        let (event, result) =
            self.new_event(Span::zero())
                .and_then(|| match stream.next_if(&Lex::Struct) {
                    Some(st_def) => match stream.next_if_id() {
                        Some((id, _)) => {
                            stream.next_must_be(&Lex::LBrace)?;
                            let fields = self.parameter_list(stream)?;
                            let ctx = stream
                                .next_must_be(&Lex::RBrace)?
                                .to_ctx()
                                .join(st_def.to_ctx());
                            Ok(Some(StructDef::new(id, ctx, fields)))
                        }
                        None => {
                            err!(st_def.span(), ParserError::StructExpectedIdentifier)
                        }
                    },
                    None => Ok(None),
                });
        result.view(|v| {
            let msg = v.map(|_| "Struct Definition");
            self.record(event.with_span(v.span()), msg)
        })
    }

    fn function_def(&self, stream: &mut TokenStream) -> ParserResult<RoutineDef<ParserContext>> {
        let (event, result) = self.new_event(Span::zero()).and_then(|| {
            match self.function_decl(stream, false)? {
                Some((ctx, name, params, is_variadic, ret_ty)) => {
                    if is_variadic {
                        err!(ctx.span(), ParserError::FnVarArgsNotAllowed)
                    } else {
                        Ok((ctx, name, params, ret_ty))
                    }
                }
                None => return Ok(None),
            }
            .and_then(|(fn_ctx, fn_name, params, fn_type)| {
                stream.next_must_be(&Lex::LBrace)?;
                let mut stmts = self.fn_body(stream)?;

                match self.return_stmt(stream)? {
                    Some(ret) => stmts.push(Statement::Return(Box::new(ret))),
                    None => {
                        return err!(
                            fn_ctx.span(),
                            ParserError::FnExpectedReturn(stream.peek().map(|t| t.clone()))
                        );
                    }
                }
                let ctx = stream.next_must_be(&Lex::RBrace)?.to_ctx().join(fn_ctx);

                Ok(Some(RoutineDef {
                    context: ctx,
                    def: RoutineDefType::Function,
                    name: fn_name,
                    params,
                    ret_ty: fn_type,
                    body: stmts,
                }))
            })
        });
        result.view(|v| {
            let msg = v.map(|_| "Funtion Definition");
            self.record(event.with_span(v.span()), msg)
        })
    }

    fn function_decl(
        &self,
        stream: &mut TokenStream,
        allow_var_args: bool,
    ) -> ParserResult<(
        ParserContext,
        StringId,
        Vec<Parameter<ParserContext>>,
        HasVarArgs,
        Type,
    )> {
        let (event, result) = self.new_event(Span::zero()).and_then(|| {
            let mut fn_ctx = match stream.next_if(&Lex::FunctionDef) {
                Some(co) => co.to_ctx(),
                None => return Ok(None),
            };

            stream
                .next_if_id()
                .ok_or_else(|| {
                    CompilerError::new(fn_ctx.span(), ParserError::FnExpectedIdentifierAfterFn)
                })
                .and_then(|(fn_name, fn_def_span)| {
                    fn_ctx = fn_ctx.extend(fn_def_span);

                    let (params, has_varargs, params_ctx) =
                        self.fn_def_params(stream, allow_var_args)?;
                    let fn_ctx = params_ctx.join(fn_ctx);

                    let (fn_type, fn_type_ctx) = if stream.next_if(&Lex::LArrow).is_some() {
                        self.consume_type(stream)?.ok_or_else(|| {
                            CompilerError::new(fn_ctx.span(), ParserError::FnExpectedTypeAfterArrow)
                        })?
                    } else {
                        (Type::Unit, fn_ctx)
                    };
                    let fn_ctx = fn_type_ctx.join(fn_ctx);

                    Ok(Some((fn_ctx, fn_name, params, has_varargs, fn_type)))
                })
        });
        result.view(|v| {
            let msg = v.map(|_| "Routine Declaration");
            let span = match v {
                Ok(v) => v.0.span(),
                Err(err) => err.span(),
            };
            self.record(event.with_span(span), msg)
        })
    }

    pub(super) fn fn_body(
        &self,
        stream: &mut TokenStream,
    ) -> Result<Vec<Statement<ParserContext>>, CompilerError<ParserError>> {
        let mut stmts = vec![];
        while let Some(s) = self.statement(stream)? {
            stmts.push(s);
        }
        Ok(stmts)
    }

    fn co_block(
        &self,
        stream: &mut TokenStream,
    ) -> Result<Vec<Statement<ParserContext>>, CompilerError<ParserError>> {
        let mut stmts = vec![];
        while let Some(s) = self.statement_or_yield_return(stream)? {
            stmts.push(s);
        }
        Ok(stmts)
    }

    fn fn_def_params(
        &self,
        stream: &mut TokenStream,
        allow_var_args: bool,
    ) -> Result<
        (Vec<Parameter<ParserContext>>, HasVarArgs, ParserContext),
        CompilerError<ParserError>,
    > {
        let ctx = stream.next_must_be(&Lex::LParen)?.to_ctx();
        let params = self.parameter_list(stream)?;

        let has_varargs = if allow_var_args {
            stream.next_if(&Lex::VarArgs).is_some()
        } else {
            false
        };

        let ctx = stream.next_must_be(&Lex::RParen)?.to_ctx().join(ctx);

        Ok((params, has_varargs, ctx))
    }

    fn parameter_list(
        &self,
        stream: &mut TokenStream,
    ) -> Result<Vec<Parameter<ParserContext>>, CompilerError<ParserError>> {
        let params = self.id_declaration_list(stream)?;

        // Convert tuples into parameters
        let params = params
            .iter()
            .map(|(name, ty, ctx)| Parameter {
                context: *ctx,
                name: *name,
                ty: ty.clone(),
            })
            .collect();

        Ok(params)
    }

    pub(super) fn id_declaration_list(
        &self,
        stream: &mut TokenStream,
    ) -> Result<Vec<(StringId, Type, ParserContext)>, CompilerError<ParserError>> {
        let mut decls = vec![];

        while let Some(id_decl) = self.id_declaration(stream)? {
            match id_decl {
                Expression::IdentifierDeclare(ctx, id, ty) => {
                    decls.push((id, ty, ctx));
                    stream.next_if(&Lex::Comma);
                }
                _ => panic!("CRITICAL: IdDeclaration not returned by id_declaration"),
            }
        }

        Ok(decls)
    }

    pub(super) fn routine_call_params(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<(Vec<Expression<ParserContext>>, ParserContext)> {
        match stream.next_if(&Lex::LParen) {
            Some(lparen) => {
                let mut params = vec![];
                while let Some(param) = self.expression(stream)? {
                    match param {
                        exp => {
                            params.push(exp);
                            match stream.next_if(&Lex::Comma) {
                                Some(_) => {}
                                None => break,
                            };
                        }
                    }
                }

                let ctx = stream
                    .next_must_be(&Lex::RParen)?
                    .to_ctx()
                    .join(lparen.to_ctx());
                Ok(Some((params, ctx)))
            }
            _ => Ok(None),
        }
    }

    pub(super) fn path(&self, stream: &mut TokenStream) -> ParserResult<(Path, ParserContext)> {
        let (event, result) = self.new_event(Span::zero()).and_then(|| {
            let mut path = vec![];

            let mut ctx = stream.peek().map(|t| t.to_ctx()).unwrap();
            // The path "::a" is equivalent to "root::a"; it is a short way of starting an absolute path
            if stream.test_if(&Lex::PathSeparator) {
                path.push(Element::FileRoot);
            } else if stream.next_if(&Lex::PathProjectRoot).is_some() {
                path.push(Element::CanonicalRoot);
            } else if stream.next_if(&Lex::PathFileRoot).is_some() {
                path.push(Element::FileRoot);
            } else if stream.next_if(&Lex::PathSelf).is_some() {
                path.push(Element::Selph);
            } else if stream.next_if(&Lex::PathSuper).is_some() {
                path.push(Element::Super);
            } else if let Some((id, _)) = stream.next_if_id() {
                path.push(Element::Id(id));
            } else {
                return Ok(None);
            }

            while let Some(path_sep) = stream.next_if(&Lex::PathSeparator) {
                let span = match stream
                    .next_if_one_of(&[Lex::Identifier(StringId::new()), Lex::PathSuper])
                {
                    Some(Token {
                        sym: Lex::PathSuper,
                        span,
                        ..
                    }) => {
                        path.push(Element::Super);
                        span
                    }
                    Some(Token {
                        sym: Lex::Identifier(id),
                        span,
                        ..
                    }) => {
                        path.push(Element::Id(id));
                        span
                    }
                    _ => {
                        return err!(path_sep.span(), ParserError::PathExpectedIdentifier);
                    }
                };
                ctx = ctx.extend(span);
            }

            Ok(Some((path.into(), ctx)))
        });
        result.view(|v| {
            let msg = v.map(|_| "Path");
            let span = match v {
                Ok(v) => v.1.span(),
                Err(err) => err.span(),
            };
            self.record(event.with_span(span), msg)
        })
    }

    pub(super) fn identifier(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        let (event, result) = self
            .new_event(Span::zero())
            .and_then(|| match stream.next_if_id() {
                Some((id, span)) => Ok(Some(Expression::Identifier(ParserContext::new(span), id))),
                _ => Ok(None),
            });
        result.view(|v| {
            let msg = v.map(|_| "Identifier");
            self.record(event.with_span(v.span()), msg)
        })
    }

    pub(super) fn consume_type(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<(Type, ParserContext)> {
        let (event, result) = self.new_event(Span::zero()).and_then(|| {
            let is_coroutine = stream.next_if(&Lex::CoroutineDef).is_some();
            let ty = match stream.next_if(&Lex::Primitive(Primitive::U8)) {
                Some(Token {
                    sym: Lex::Primitive(prim_ty),
                    span,
                }) => {
                    let ty = match prim_ty {
                        Primitive::U8 => Some(Type::U8),
                        Primitive::U16 => Some(Type::U16),
                        Primitive::U32 => Some(Type::U32),
                        Primitive::U64 => Some(Type::U64),
                        Primitive::I8 => Some(Type::I8),
                        Primitive::I16 => Some(Type::I16),
                        Primitive::I32 => Some(Type::I32),
                        Primitive::I64 => Some(Type::I64),
                        Primitive::F64 => Some(Type::F64),
                        Primitive::Bool => Some(Type::Bool),
                        Primitive::StringLiteral => Some(Type::StringLiteral),
                    };
                    let ctx = ParserContext::new(span);
                    ty.map(|ty| (ty, ctx))
                }
                _ => match self.path(stream)? {
                    Some((path, path_ctx)) => Some((Type::Custom(path), path_ctx)),
                    _ => match self.array_type(stream)? {
                        Some((ty, ctx)) => Some((ty, ctx)),
                        None => match self.raw_pointer_type(stream)? {
                            Some((ty, ctx)) => Some((ty, ctx)),
                            None => None,
                        },
                    },
                },
            }
            .map(|(ty, ctx)| {
                if is_coroutine {
                    (Type::Coroutine(Box::new(ty)), ctx)
                } else {
                    (ty, ctx)
                }
            });
            Ok(ty)
        });
        result.view(|v| {
            let msg = v.map(|v| match v.0 {
                Type::Custom(_) => "Custom Type",
                Type::Array(..) => "Array Type",
                Type::RawPointer(..) => "Raw Pointer Type",
                _ => "Primitive Type",
            });
            let span = match v {
                Ok(ok) => ok.1.span(),
                Err(err) => err.span(),
            };
            self.record(event.with_span(span), msg)
        })
    }

    fn raw_pointer_type(&self, stream: &mut TokenStream) -> ParserResult<(Type, ParserContext)> {
        let (event, result) =
            self.new_event(Span::zero())
                .and_then(|| match stream.next_if(&Lex::Mul) {
                    Some(star) => match stream.next_if_one_of(&[Lex::Mut, Lex::Const]) {
                        Some(m) if m.sym == Lex::Mut => {
                            let ctx = star.to_ctx();
                            let ty = self.consume_type(stream)?.ok_or_else(|| {
                                CompilerError::new(ctx.span(), ParserError::RawPointerExpectedType)
                            })?;
                            let ctx = ctx.join(ty.1);
                            Ok(Some((
                                Type::RawPointer(PointerMut::Mut, Box::new(ty.0)),
                                ctx,
                            )))
                        }
                        Some(c) if c.sym == Lex::Const => {
                            let ctx = star.to_ctx();
                            let ty = self.consume_type(stream)?.ok_or_else(|| {
                                CompilerError::new(ctx.span(), ParserError::RawPointerExpectedType)
                            })?;
                            let ctx = ctx.join(ty.1);
                            Ok(Some((
                                Type::RawPointer(PointerMut::Const, Box::new(ty.0)),
                                ctx,
                            )))
                        }
                        Some(_) => Err(CompilerError::new(
                            star.span(),
                            ParserError::RawPointerExpectedConstOrMut,
                        )),
                        None => Err(CompilerError::new(
                            star.span(),
                            ParserError::RawPointerExpectedConstOrMut,
                        )),
                    },
                    None => Ok(None),
                });

        result.view(|v| {
            let msg = v.map(|_| "Raw Pointer Type");
            let span = match v {
                Ok(ok) => ok.1.span(),
                Err(err) => err.span(),
            };
            self.record(event.with_span(span), msg)
        })
    }

    fn array_type(&self, stream: &mut TokenStream) -> ParserResult<(Type, ParserContext)> {
        let (event, result) =
            self.new_event(Span::zero())
                .and_then(|| match stream.next_if(&Lex::LBracket) {
                    Some(lbracket) => {
                        let ctx = lbracket.to_ctx();
                        self.consume_type(stream)?
                            .ok_or_else(|| {
                                CompilerError::new(ctx.span(), ParserError::ArrayDeclExpectedType)
                            })
                            .and_then(|(element_ty, _)| {
                                stream.next_must_be(&Lex::Semicolon)?;

                                let len = self.expression(stream)?.ok_or_else(|| {
                                    CompilerError::new(
                                        ctx.span(),
                                        ParserError::ArrayDeclExpectedSize,
                                    )
                                })?;
                                let len = match len {
                                    Expression::U8(_, l) => l as usize,
                                    Expression::U16(_, l) => l as usize,
                                    Expression::U32(_, l) => l as usize,
                                    Expression::U64(_, l) => l as usize,
                                    Expression::I8(_, l) => l as usize,
                                    Expression::I16(_, l) => l as usize,
                                    Expression::I32(_, l) => l as usize,
                                    Expression::I64(_, l) => l as usize,
                                    _ => {
                                        return err!(
                                            len.span(),
                                            ParserError::ArrayExpectedIntLiteral
                                        )
                                    }
                                };

                                let ctx = stream.next_must_be(&Lex::RBracket)?.to_ctx().join(ctx);
                                Ok(Some((Type::Array(Box::new(element_ty), len), ctx)))
                            })
                    }
                    None => Ok(None),
                });
        result.view(|v| {
            let msg = v.map(|_| "Array Type");
            let span = match v {
                Ok(ok) => ok.1.span(),
                Err(err) => err.span(),
            };
            self.record(event.with_span(span), msg)
        })
    }

    pub(super) fn id_declaration(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        let (event, result) = self.new_event(Span::zero()).and_then(|| {
            match stream.next_ifn(vec![Lex::Identifier(StringId::new()), Lex::Colon]) {
                Some(decl_tok) => {
                    let ctx = decl_tok[0].to_ctx().join(decl_tok[1].to_ctx());
                    let id = decl_tok[0].sym.get_str().expect(
                    "CRITICAL: first token is an identifier but cannot be converted to a string",
                );
                    self.consume_type(stream).and_then(|result| {
                        Ok(result.and_then(|(ty, ty_ctx)| {
                            let ctx = ctx.join(ty_ctx);
                            Some(Expression::IdentifierDeclare(ctx, id, ty))
                        }))
                    })
                }
                None => Ok(None),
            }
        });
        result.view(|v| {
            let msg = v.map(|_| "Identifier Declaration");
            self.record(event.with_span(v.span()), msg)
        })
    }
}
