use std::sync::atomic::AtomicBool;
use std::sync::atomic::{AtomicUsize, Ordering};

use stdext::function_name;

use crate::compiler::source::HasSpan;
use crate::compiler::Span;
use crate::StringId;
use crate::{
    compiler::{
        ast::*,
        lexer::tokens::{Lex, Primitive, Token},
        CompilerError,
    },
    diagnostics::config::TracingConfig,
};

use super::{ctx_over_tokens, ParserContext};
// AST - a type(s) which is used to construct an AST representing the logic of the
// program
// Each type of node represents an expression and the only requirement is that at the
// end of computing an expression its result is in EAX
use super::{
    expression::expression,
    statement::{return_stmt, statement, statement_or_yield_return},
    ParserResult,
};
use super::{tokenstream::TokenStream, ParserError};

type HasVarArgs = bool;

pub(super) static ENABLE_TRACING: AtomicBool = AtomicBool::new(false);
pub(super) static TRACE_START: AtomicUsize = AtomicUsize::new(0);
pub(super) static TRACE_END: AtomicUsize = AtomicUsize::new(0);

pub fn set_tracing(config: TracingConfig) {
    match config {
        TracingConfig::All => {
            ENABLE_TRACING.store(true, Ordering::SeqCst);
            TRACE_START.store(0, Ordering::SeqCst);
            TRACE_END.store(0, Ordering::SeqCst);
        }
        TracingConfig::After(start) => {
            ENABLE_TRACING.store(true, Ordering::SeqCst);
            TRACE_START.store(start, Ordering::SeqCst);
            TRACE_END.store(0, Ordering::SeqCst);
        }
        TracingConfig::Before(end) => {
            ENABLE_TRACING.store(true, Ordering::SeqCst);
            TRACE_START.store(0, Ordering::SeqCst);
            TRACE_END.store(end, Ordering::SeqCst);
        }
        TracingConfig::Between(start, end) => {
            ENABLE_TRACING.store(true, Ordering::SeqCst);
            TRACE_START.store(start, Ordering::SeqCst);
            TRACE_END.store(end, Ordering::SeqCst);
        }
        TracingConfig::Only(line) => {
            ENABLE_TRACING.store(true, Ordering::SeqCst);
            TRACE_START.store(line, Ordering::SeqCst);
            TRACE_END.store(line, Ordering::SeqCst);
        }
        _ => (),
    }
}

#[macro_export]
macro_rules! trace {
    ($ts:expr) => {
        if ENABLE_TRACING.load(Ordering::SeqCst) {
            match $ts.peek() {
                None => (),
                Some(token) => {
                    if TRACE_START.load(Ordering::SeqCst) == 0
                        && TRACE_END.load(Ordering::SeqCst) == 0
                    {
                        println!("{} <- {}", function_name!(), token)
                    } else if TRACE_END.load(Ordering::SeqCst) == 0
                        && TRACE_START.load(Ordering::SeqCst) <= token.line as usize
                    {
                        println!("{} <- {}", function_name!(), token)
                    } else if TRACE_START.load(Ordering::SeqCst) == 0
                        && token.line as usize <= TRACE_END.load(Ordering::SeqCst)
                    {
                        println!("{} <- {}", function_name!(), token)
                    } else if TRACE_START.load(Ordering::SeqCst) <= token.line as usize
                        && token.line as usize <= TRACE_END.load(Ordering::SeqCst)
                    {
                        println!("{} <- {}", function_name!(), token)
                    }
                }
            }
        }
    };
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
    ASSIGN := IDENTIFIER = EXPRESSION;
    BIND := let [mut] ID_DEC := (EXPRESSION|INIT_CO)
    PRINTLN := println EXPRESSION ;
    RETURN := return [EXPRESSION] SEMICOLON
    YIELD_RETURN := yield return [EXPRESSION] SEMICOLON
    STATEMENT := [BIND] SEMICOLON
    BLOCK := STATEMENT*
    COBLOCK := [STATEMENT | YIELD_RETURN]*
    FUNCTION := fn IDENTIFIER LPAREN [ID_DEC [, ID_DEC]*] RPAREN  [LARROW PRIMITIVE] LBRACE BLOCK RETURN RBRACE
    COROUTINE := co IDENTIFIER LPAREN [ID_DEC [, ID_DEC]*] RPAREN [LARROW PRIMITIVE] LBRACE COBLOCK RETURN RBRACE
    STRUCT_INIT := IDENTIFIER LBRACE [IDENTIFIER : PRIMITIVE]* RBRACE
    STRUCT_DEF := struct IDENTIFIER LBRACE [ID_DEC]* RBRACE
    MODULES := [FUNCTION|COROUTINE|STRUCT]*

    tokenize - takes a string of text and converts it to a string of tokens
    parse - takes a string of tokens and converts it into an AST
    compile - takes an AST and converts it to assembly
*/

pub fn parse(name: StringId, tokens: &Vec<Token>) -> ParserResult<Module<ParserContext>> {
    // Create the module that represents the source code unit as a whole (usually the file)
    // give it span that covers the entire set of tokens
    let module_ctx = ctx_over_tokens(&tokens)
        .ok_or(CompilerError::new(Span::zero(), ParserError::EmptyProject))?;
    let mut module = Module::new(name, module_ctx);

    // Create the token stream.
    let mut stream = TokenStream::new(&tokens)
        .ok_or(CompilerError::new(Span::zero(), ParserError::EmptyProject))?;

    while stream.peek().is_some() {
        let start_index = stream.index();
        parse_items_into(&mut stream, &mut module)?;

        if stream.index() == start_index {
            return err!(
                stream.peek().unwrap().span(),
                ParserError::Locked(stream.peek().map(|t| t.clone()))
            );
        }
    }

    Ok(Some(module))
}

fn module(stream: &mut TokenStream) -> ParserResult<Module<ParserContext>> {
    let mod_def = match stream.next_if(&Lex::ModuleDef) {
        Some(module) => match stream.next_if_id() {
            Some((module_name, _, _)) => {
                let mut module = Module::new(module_name, module.to_ctx());
                stream.next_must_be(&Lex::LBrace)?;

                parse_items_into(stream, &mut module)?;

                let ctx = stream
                    .next_must_be(&Lex::RBrace)?
                    .to_ctx()
                    .join(*module.context());
                *module.get_context_mut() = ctx;
                Some(module)
            }
            _ => {
                return err!(module.span(), ParserError::ModExpectedName);
            }
        },
        None => None,
    };

    Ok(mod_def)
}

fn parse_items_into(
    stream: &mut TokenStream,
    module: &mut Module<ParserContext>,
) -> ParserResult<()> {
    if let Some((submods, items)) = parse_items(stream)? {
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
    stream: &mut TokenStream,
) -> ParserResult<(Vec<Module<ParserContext>>, Vec<Item<ParserContext>>)> {
    let mut modules = vec![];
    let mut items = vec![];
    while stream.peek().is_some() {
        let start_index = stream.index();
        if let Some(m) = module(stream)? {
            modules.push(m);
        }

        if let Some(f) = function_def(stream)? {
            items.push(Item::Routine(f));
        }
        if let Some(c) = coroutine_def(stream)? {
            items.push(Item::Routine(c));
        }

        if let Some(s) = struct_def(stream)? {
            items.push(Item::Struct(s));
        }

        if let Some(e) = extern_def(stream)? {
            items.push(Item::Extern(e));
        }

        if stream.index() == start_index {
            break;
        }
    }

    if modules.len() == 0 && items.len() == 0 {
        Ok(None)
    } else {
        Ok(Some((modules, items)))
    }
}

fn extern_def(stream: &mut TokenStream) -> ParserResult<Extern<ParserContext>> {
    match stream.next_if(&Lex::Extern) {
        Some(extern_tok) => match function_decl(stream, true)? {
            Some((fn_ctx, fn_name, params, has_varargs, fn_type)) => {
                if has_varargs && params.len() == 0 {
                    return err!(fn_ctx.span(), ParserError::ExternInvalidVarArgs);
                }
                stream.next_must_be(&Lex::Semicolon)?;
                Ok(Some(Extern::new(
                    fn_name,
                    extern_tok.to_ctx(),
                    params,
                    has_varargs,
                    fn_type,
                )))
            }
            None => {
                err!(extern_tok.span(), ParserError::ExternExpectedFnDecl)
            }
        },
        None => Ok(None),
    }
}

fn struct_def(stream: &mut TokenStream) -> ParserResult<StructDef<ParserContext>> {
    match stream.next_if(&Lex::Struct) {
        Some(st_def) => match stream.next_if_id() {
            Some((id, _, _)) => {
                stream.next_must_be(&Lex::LBrace)?;
                let fields = parameter_list(stream)?;
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
    }
}
fn function_def(stream: &mut TokenStream) -> ParserResult<RoutineDef<ParserContext>> {
    let (fn_ctx, fn_name, params, fn_type) = match function_decl(stream, false)? {
        Some((ctx, name, params, is_variadic, ret_ty)) => {
            if is_variadic {
                return err!(ctx.span(), ParserError::FnVarArgsNotAllowed);
            }
            (ctx, name, params, ret_ty)
        }
        None => return Ok(None),
    };

    stream.next_must_be(&Lex::LBrace)?;
    let mut stmts = fn_body(stream)?;

    match return_stmt(stream)? {
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
}

fn function_decl(
    stream: &mut TokenStream,
    allow_var_args: bool,
) -> ParserResult<(
    ParserContext,
    StringId,
    Vec<Parameter<ParserContext>>,
    HasVarArgs,
    Type,
)> {
    let mut fn_ctx = match stream.next_if(&Lex::FunctionDef) {
        Some(co) => co.to_ctx(),
        None => return Ok(None),
    };

    let (fn_name, _, fn_def_span) = stream.next_if_id().ok_or(CompilerError::new(
        fn_ctx.span(),
        ParserError::FnExpectedIdentifierAfterFn,
    ))?;
    fn_ctx = fn_ctx.extend(fn_def_span);

    let (params, has_varargs) = fn_def_params(stream, allow_var_args)?;
    let fn_type = if stream.next_if(&Lex::LArrow).is_some() {
        consume_type(stream)?
            .ok_or(CompilerError::new(
                fn_ctx.span(),
                ParserError::FnExpectedTypeAfterArrow,
            ))?
            .0
    } else {
        Type::Unit
    };

    Ok(Some((fn_ctx, fn_name, params, has_varargs, fn_type)))
}

fn coroutine_def(stream: &mut TokenStream) -> ParserResult<RoutineDef<ParserContext>> {
    let ctx = match stream.next_if(&Lex::CoroutineDef) {
        Some(co) => co.to_ctx(),
        None => return Ok(None),
    };

    let (co_name, _, _) = stream.next_if_id().ok_or(CompilerError::new(
        ctx.span(),
        ParserError::CoExpectedIdentifierAfterCo,
    ))?;
    let (params, has_varargs) = fn_def_params(stream, false)?;

    if has_varargs {
        return err!(ctx.span(), ParserError::FnVarArgsNotAllowed);
    }

    let co_type = match stream.next_if(&Lex::LArrow) {
        Some(t) => {
            consume_type(stream)?
                .ok_or(CompilerError::new(
                    t.span(),
                    ParserError::FnExpectedTypeAfterArrow,
                ))?
                .0
        }
        _ => Type::Unit,
    };

    stream.next_must_be(&Lex::LBrace)?;
    let mut stmts = co_block(stream)?;

    match return_stmt(stream)? {
        Some(ret) => stmts.push(Statement::Return(Box::new(ret))),
        None => {
            let span = stmts.last().map_or(ctx.span(), |s| s.context().span());
            return err!(
                span,
                ParserError::FnExpectedReturn(stream.peek().map(|t| t.clone()))
            );
        }
    }
    let ctx = stream.next_must_be(&Lex::RBrace)?.to_ctx().join(ctx);

    Ok(Some(RoutineDef {
        context: ctx,
        def: RoutineDefType::Coroutine,
        name: co_name,
        params,
        ret_ty: co_type,
        body: stmts,
    }))
}

pub(super) fn fn_body(
    stream: &mut TokenStream,
) -> Result<Vec<Statement<ParserContext>>, CompilerError<ParserError>> {
    trace!(stream);
    let mut stmts = vec![];
    while let Some(s) = statement(stream)? {
        stmts.push(s);
    }
    Ok(stmts)
}

fn co_block(
    stream: &mut TokenStream,
) -> Result<Vec<Statement<ParserContext>>, CompilerError<ParserError>> {
    trace!(stream);
    let mut stmts = vec![];
    while let Some(s) = statement_or_yield_return(stream)? {
        stmts.push(s);
    }
    Ok(stmts)
}

fn fn_def_params(
    stream: &mut TokenStream,
    allow_var_args: bool,
) -> Result<(Vec<Parameter<ParserContext>>, HasVarArgs), CompilerError<ParserError>> {
    trace!(stream);
    stream.next_must_be(&Lex::LParen)?;
    let params = parameter_list(stream)?;

    let has_varargs = if allow_var_args {
        stream.next_if(&Lex::VarArgs).is_some()
    } else {
        false
    };

    stream.next_must_be(&Lex::RParen)?;

    Ok((params, has_varargs))
}

fn parameter_list(
    stream: &mut TokenStream,
) -> Result<Vec<Parameter<ParserContext>>, CompilerError<ParserError>> {
    let params = id_declaration_list(stream)?;

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
    stream: &mut TokenStream,
) -> Result<Vec<(StringId, Type, ParserContext)>, CompilerError<ParserError>> {
    trace!(stream);
    let mut decls = vec![];

    while let Some(id_decl) = id_declaration(stream)? {
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
    stream: &mut TokenStream,
) -> ParserResult<(Vec<Expression<ParserContext>>, ParserContext)> {
    trace!(stream);
    match stream.next_if(&Lex::LParen) {
        Some(lparen) => {
            let mut params = vec![];
            while let Some(param) = expression(stream)? {
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

pub(super) fn path(stream: &mut TokenStream) -> ParserResult<(Path, ParserContext)> {
    trace!(stream);
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
    } else if let Some((id, _, _)) = stream.next_if_id() {
        path.push(Element::Id(id));
    } else {
        return Ok(None);
    }

    while let Some(path_sep) = stream.next_if(&Lex::PathSeparator) {
        let span =
            match stream.next_if_one_of(vec![Lex::Identifier(StringId::new()), Lex::PathSuper]) {
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
}

fn identifier(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    match stream.next_if_id() {
        Some((id, ln, span)) => Ok(Some(Expression::Identifier(
            ParserContext::new(ln, span),
            id,
        ))),
        _ => Ok(None),
    }
}

fn consume_type(stream: &mut TokenStream) -> ParserResult<(Type, ParserContext)> {
    trace!(stream);
    let is_coroutine = stream.next_if(&Lex::CoroutineDef).is_some();
    let ty = match stream.next_if(&Lex::Primitive(Primitive::U8)) {
        Some(Token {
            sym: Lex::Primitive(prim_ty),
            line,
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
                Primitive::Bool => Some(Type::Bool),
                Primitive::StringLiteral => Some(Type::StringLiteral),
            };
            let ctx = ParserContext::new(line, span);
            ty.map(|ty| (ty, ctx))
        }
        _ => match path(stream)? {
            Some((path, path_ctx)) => Some((Type::Custom(path), path_ctx)),
            _ => match array_type(stream)? {
                Some((ty, ctx)) => Some((ty, ctx)),
                None => None,
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
}

fn array_type(stream: &mut TokenStream) -> ParserResult<(Type, ParserContext)> {
    trace!(stream);
    match stream.next_if(&Lex::LBracket) {
        Some(lbracket) => {
            let ctx = lbracket.to_ctx();
            let (element_ty, _) = consume_type(stream)?.ok_or(CompilerError::new(
                ctx.span(),
                ParserError::ArrayDeclExpectedType,
            ))?;
            stream.next_must_be(&Lex::Semicolon)?;

            let len = expression(stream)?.ok_or(CompilerError::new(
                ctx.span(),
                ParserError::ArrayDeclExpectedSize,
            ))?;
            let len = match len {
                Expression::U8(_, l) => l as usize,
                Expression::U16(_, l) => l as usize,
                Expression::U32(_, l) => l as usize,
                Expression::U64(_, l) => l as usize,
                Expression::I8(_, l) => l as usize,
                Expression::I16(_, l) => l as usize,
                Expression::I32(_, l) => l as usize,
                Expression::I64(_, l) => l as usize,
                _ => return err!(len.span(), ParserError::ArrayExpectedIntLiteral),
            };

            let ctx = stream.next_must_be(&Lex::RBracket)?.to_ctx().join(ctx);
            Ok(Some((Type::Array(Box::new(element_ty), len), ctx)))
        }
        None => Ok(None),
    }
}

pub(super) fn id_declaration(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    match stream.next_ifn(vec![Lex::Identifier(StringId::new()), Lex::Colon]) {
        Some(decl_tok) => {
            let ctx = decl_tok[0].to_ctx().join(decl_tok[1].to_ctx());
            let id = decl_tok[0].sym.get_str().expect(
                "CRITICAL: first token is an identifier but cannot be converted to a string",
            );
            let (ty, ty_ctx) = consume_type(stream)?.ok_or(CompilerError::new(
                decl_tok[0].span(),
                ParserError::IdDeclExpectedType,
            ))?;
            let ctx = ctx.join(ty_ctx);
            Ok(Some(Expression::IdentifierDeclare(ctx, id, ty)))
        }
        None => Ok(None),
    }
}
