use std::sync::atomic::AtomicBool;
use std::sync::atomic::{AtomicUsize, Ordering};

use stdext::function_name;

use crate::{
    compiler::{
        ast::*,
        lexer::tokens::{Lex, Primitive, Token},
    },
    diagnostics::{config::TracingConfig, Diag, DiagData},
};
use braid_lang::result::Result;

// AST - a type(s) which is used to construct an AST representing the logic of the
// program
// Each type of node represents an expression and the only requirement is that at the
// end of computing an expression its result is in EAX
use super::tokenstream::TokenStream;
use super::{
    expression::expression,
    statement::{return_stmt, statement, statement_or_yield_return},
};

pub type ParserInfo = u32;
type HasVarArgs = bool;

impl Annotation for ParserInfo {
    fn id(&self) -> u32 {
        0
    }

    fn line(&self) -> u32 {
        *self
    }
}

impl Diag for ParserInfo {
    fn diag(&self) -> DiagData {
        DiagData::new(*self, 0)
    }
}

pub(super) type ParserResult<T> = Result<Option<T>>;

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
                        && TRACE_START.load(Ordering::SeqCst) <= token.l as usize
                    {
                        println!("{} <- {}", function_name!(), token)
                    } else if TRACE_START.load(Ordering::SeqCst) == 0
                        && token.l as usize <= TRACE_END.load(Ordering::SeqCst)
                    {
                        println!("{} <- {}", function_name!(), token)
                    } else if TRACE_START.load(Ordering::SeqCst) <= token.l as usize
                        && token.l as usize <= TRACE_END.load(Ordering::SeqCst)
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

pub struct Parser {
    current_line: usize,
    tracing: bool,
}

impl Parser {
    pub fn new(tracing: bool) -> Parser {
        Parser {
            tracing,
            current_line: 0,
        }
    }
}

pub fn parse(name: &str, tokens: &Vec<Token>) -> ParserResult<Module<u32>> {
    let mut stream = TokenStream::new(&tokens);

    let module_line = stream.peek().map_or(1, |t| t.l);
    let mut module = Module::new(&name, module_line);

    while stream.peek().is_some() {
        let start_index = stream.index();
        parse_items_into(&mut stream, &mut module).map_err(|e| format!("Parser: {}", e))?;

        if stream.index() == start_index {
            return Err(format!("Parser cannot advance past {:?}", stream.peek()));
        }
    }

    Ok(Some(module))
}

fn module(stream: &mut TokenStream) -> ParserResult<Module<u32>> {
    let mod_def = match stream.next_if(&Lex::ModuleDef) {
        Some(token) => match stream.next_if_id() {
            Some((ln, module_name)) => {
                let mut module = Module::new(&module_name, ln);
                stream.next_must_be(&Lex::LBrace)?;

                parse_items_into(stream, &mut module)?;

                stream.next_must_be(&Lex::RBrace)?;
                Some(module)
            }
            _ => {
                return Err(format!("L{}: expected name after mod keyword", token.l));
            }
        },
        None => None,
    };

    Ok(mod_def)
}

fn parse_items_into(stream: &mut TokenStream, module: &mut Module<u32>) -> ParserResult<()> {
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

fn parse_items(stream: &mut TokenStream) -> ParserResult<(Vec<Module<u32>>, Vec<Item<u32>>)> {
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

fn extern_def(stream: &mut TokenStream) -> ParserResult<Extern<u32>> {
    match stream.next_if(&Lex::Extern) {
        Some(token) => match function_decl(stream, true)? {
            Some((fn_line, fn_name, params, has_varargs, fn_type)) => {
                if has_varargs && params.len() == 0 {
                    return Err("An extern declaration must have at least one parameter before a VarArgs (...) parameter".into());
                }
                stream.next_must_be(&Lex::Semicolon)?;
                Ok(Some(Extern::new(
                    &fn_name,
                    fn_line,
                    params,
                    has_varargs,
                    fn_type,
                )))
            }
            None => Err(format!(
                "L{}: expected function declaration after extern",
                token.l
            )),
        },
        None => Ok(None),
    }
}

fn struct_def(stream: &mut TokenStream) -> ParserResult<StructDef<u32>> {
    match stream.next_if(&Lex::Struct) {
        Some(token) => match stream.next_if_id() {
            Some((line, id)) => {
                stream.next_must_be(&Lex::LBrace)?;
                let fields = parameter_list(stream)?;
                stream.next_must_be(&Lex::RBrace)?;
                Ok(Some(StructDef::new(&id, line, fields)))
            }
            None => Err(format!("L{}: expected identifer after struct", token.l)),
        },
        None => Ok(None),
    }
}

fn function_decl(
    stream: &mut TokenStream,
    allow_var_args: bool,
) -> ParserResult<(u32, String, Vec<Parameter<u32>>, HasVarArgs, Type)> {
    let fn_line = match stream.next_if(&Lex::FunctionDef) {
        Some(co) => co.l,
        None => return Ok(None),
    };

    let (fn_line, fn_name) = stream
        .next_if_id()
        .ok_or(format!("L{}: Expected identifier after fn", fn_line))?;
    let (params, has_varargs) = fn_def_params(stream, allow_var_args)?;
    let fn_type = if stream.next_if(&Lex::LArrow).is_some() {
        consume_type(stream)?.ok_or(format!("L{}: Expected type after ->", fn_line))?
    } else {
        Type::Unit
    };

    Ok(Some((fn_line, fn_name, params, has_varargs, fn_type)))
}

fn function_def(stream: &mut TokenStream) -> ParserResult<RoutineDef<u32>> {
    let (fn_line, fn_name, params, fn_type) = match function_decl(stream, false)? {
        Some((l, n, p, v, t)) => {
            if v {
                return Err("VarArgs are not allowed in Braid function definitions".into());
            }
            (l, n, p, t)
        }
        None => return Ok(None),
    };

    stream.next_must_be(&Lex::LBrace)?;
    let mut stmts = block(stream)?;

    match return_stmt(stream)? {
        Some(ret) => stmts.push(Statement::Return(Box::new(ret))),
        None => {
            return Err(format!(
                "L{}: Function must end with a return statement, got {:?}",
                stmts.last().map_or(fn_line, |s| *s.annotation()),
                stream.peek(),
            ))
        }
    }
    stream.next_must_be(&Lex::RBrace)?;

    Ok(Some(RoutineDef {
        annotations: fn_line,
        def: RoutineDefType::Function,
        name: fn_name,
        params,
        ret_ty: fn_type,
        body: stmts,
    }))
}

fn coroutine_def(stream: &mut TokenStream) -> ParserResult<RoutineDef<u32>> {
    let co_line = match stream.next_if(&Lex::CoroutineDef) {
        Some(co) => co.l,
        None => return Ok(None),
    };

    let (co_line, co_name) = stream
        .next_if_id()
        .ok_or(format!("L{}: Expected identifier after co", co_line))?;
    let (params, has_varargs) = fn_def_params(stream, false)?;

    if has_varargs {
        return Err("VarArgs are not allowed in Braid function definitions".into());
    }

    let co_type = match stream.next_if(&Lex::LArrow) {
        Some(t) => consume_type(stream)?.ok_or(format!("L{}: Expected type after ->", t.l))?,
        _ => Type::Unit,
    };

    stream.next_must_be(&Lex::LBrace)?;
    let mut stmts = co_block(stream)?;

    match return_stmt(stream)? {
        Some(ret) => stmts.push(Statement::Return(Box::new(ret))),
        None => {
            return Err(format!(
                "L{}: Coroutine must end with a return statement",
                stmts.last().map_or(co_line, |s| *s.annotation()),
            ))
        }
    }
    stream.next_must_be(&Lex::RBrace)?;

    Ok(Some(RoutineDef {
        annotations: co_line,
        def: RoutineDefType::Coroutine,
        name: co_name,
        params,
        ret_ty: co_type,
        body: stmts,
    }))
}

pub(super) fn block(stream: &mut TokenStream) -> Result<Vec<Statement<ParserInfo>>> {
    trace!(stream);
    let mut stmts = vec![];
    while let Some(s) = statement(stream)? {
        stmts.push(s);
    }
    Ok(stmts)
}

fn co_block(stream: &mut TokenStream) -> Result<Vec<Statement<ParserInfo>>> {
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
) -> Result<(Vec<Parameter<ParserInfo>>, HasVarArgs)> {
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

fn parameter_list(stream: &mut TokenStream) -> Result<Vec<Parameter<ParserInfo>>> {
    let params = id_declaration_list(stream)?;

    // Convert tuples into parameters
    let params = params
        .iter()
        .map(|(line, name, ty)| Parameter {
            annotation: *line,
            name: name.clone(),
            ty: ty.clone(),
        })
        .collect();

    Ok(params)
}

pub(super) fn id_declaration_list(stream: &mut TokenStream) -> Result<Vec<(u32, String, Type)>> {
    trace!(stream);
    let mut decls = vec![];

    while let Some(token) = id_declaration(stream)? {
        match token {
            Expression::IdentifierDeclare(line, id, ty) => {
                decls.push((line, id, ty));
                stream.next_if(&Lex::Comma);
            }
            _ => panic!("CRITICAL: IdDeclaration not returned by id_declaration"),
        }
    }

    Ok(decls)
}

fn function_call(stream: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
    trace!(stream);
    if stream.test_ifn(vec![Lex::Identifier("".into()), Lex::LParen]) {
        let (line, fn_name) = stream
            .next_if_id()
            .expect("CRITICAL: failed to get identifier");
        let params = routine_call_params(stream)?
            .ok_or(format!("L{}: expected parameters in function call", line))?;
        Ok(Some(Expression::RoutineCall(
            line,
            RoutineCall::Function,
            vec![fn_name].into(),
            params,
        )))
    } else {
        Ok(None)
    }
}

pub(super) fn routine_call_params(
    stream: &mut TokenStream,
) -> ParserResult<Vec<Expression<ParserInfo>>> {
    trace!(stream);
    match stream.next_if(&Lex::LParen) {
        Some(_) => {
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

            stream.next_must_be(&Lex::RParen)?;
            Ok(Some(params))
        }
        _ => Ok(None),
    }
}

pub(super) fn path(stream: &mut TokenStream) -> ParserResult<(u32, Path)> {
    trace!(stream);
    let mut path = vec![];

    // The path "::a" is equivalent to "root::a"; it is a short way of starting an absolute path
    if stream.next_if(&Lex::PathSeparator).is_some() {
        path.push(ROOT_PATH.into());
    }

    match stream.next_if_id() {
        Some((line, id)) => {
            path.push(id);
            while let Some(token) = stream.next_if(&Lex::PathSeparator) {
                let line = token.l;
                let (_, id) = stream.next_if_id().ok_or(format!(
                    "L{}: expect identifier after path separator '::'",
                    line
                ))?;
                path.push(id);
            }
            Ok(Some((line, path.into())))
        }
        None => Ok(None),
    }
}

fn identifier(stream: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
    trace!(stream);
    match stream.next_if_id() {
        Some((line, id)) => Ok(Some(Expression::Identifier(line, id))),
        _ => Ok(None),
    }
}

fn consume_type(stream: &mut TokenStream) -> ParserResult<Type> {
    trace!(stream);
    let is_coroutine = stream.next_if(&Lex::CoroutineDef).is_some();
    let ty = match stream.peek() {
        Some(Token {
            l: _,
            s: Lex::Primitive(primitive),
        }) => {
            let ty = match *primitive {
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
            stream.next();
            ty
        }
        _ => match path(stream)? {
            Some((_, path)) => Some(Type::Custom(path)),
            _ => match array_type(stream)? {
                Some(ty) => Some(ty),
                None => None,
            },
        },
    }
    .map(|ty| {
        if is_coroutine {
            Type::Coroutine(Box::new(ty))
        } else {
            ty
        }
    });
    Ok(ty)
}

fn array_type(stream: &mut TokenStream) -> ParserResult<Type> {
    trace!(stream);
    match stream.next_if(&Lex::LBracket) {
        Some(_) => {
            let element_ty =
                consume_type(stream)?.ok_or("Expected type in array type declaration")?;
            stream.next_must_be(&Lex::Semicolon)?;

            let len = expression(stream)?
                .ok_or("Expected size to be specified in array type declaration")?;
            let len = match len {
                Expression::U8(_, l) => l as usize,
                Expression::U16(_, l) => l as usize,
                Expression::U32(_, l) => l as usize,
                Expression::U64(_, l) => l as usize,
                Expression::I8(_, l) => l as usize,
                Expression::I16(_, l) => l as usize,
                Expression::I32(_, l) => l as usize,
                Expression::I64(_, l) => l as usize,
                _ => return Err("Expected integer literal for array size".into()),
            };

            stream.next_must_be(&Lex::RBracket)?;
            Ok(Some(Type::Array(Box::new(element_ty), len)))
        }
        None => Ok(None),
    }
}

pub(super) fn id_declaration(stream: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
    trace!(stream);
    match stream.next_ifn(vec![Lex::Identifier("".into()), Lex::Colon]) {
        Some(t) => {
            let line_id = t[0].l;
            let line_value = t[1].l;
            let id = t[0].s.get_str().expect(
                "CRITICAL: first token is an identifier but cannot be converted to a string",
            );
            let ty = consume_type(stream)?.ok_or(format!(
                "L{}: expected type after : in type declaration",
                line_value
            ))?;
            Ok(Some(Expression::IdentifierDeclare(line_id, id, ty)))
        }
        None => Ok(None),
    }
}
