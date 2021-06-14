use std::sync::atomic::AtomicBool;
use std::sync::atomic::{AtomicUsize, Ordering};

use stdext::function_name;

use crate::{
    ast::*,
    diagnostics::{config::TracingConfig, Diag, DiagData},
    lexer::tokens::{Lex, Primitive, Token},
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

pub fn parse(tokens: Vec<Token>) -> ParserResult<Module<u32>> {
    let mut stream = TokenStream::new(&tokens);
    let start_index = stream.index();
    let mut item = None;
    while stream.peek().is_some() {
        item = parse_items("root", &mut stream).map_err(|e| format!("Parser: {}", e))?;

        if stream.index() == start_index {
            return Err(format!("Parser cannot advance past {:?}", stream.peek()));
        }
    }

    Ok(item)
}

fn module(stream: &mut TokenStream) -> ParserResult<Module<u32>> {
    let mod_def = match stream.next_if(&Lex::ModuleDef) {
        Some(token) => match stream.next_if_id() {
            Some((_, module_name)) => {
                stream.next_must_be(&Lex::LBrace)?;
                let module = parse_items(&module_name, stream)?;
                stream.next_must_be(&Lex::RBrace)?;
                module
            }
            _ => {
                return Err(format!("L{}: expected name after mod keyword", token.l));
            }
        },
        None => None,
    };

    Ok(mod_def)
}

fn parse_items(name: &str, stream: &mut TokenStream) -> ParserResult<Module<u32>> {
    let module_line = stream.peek().map_or(1, |t| t.l);
    let mut parent_module = Module::new(name, module_line);
    while stream.peek().is_some() {
        let start_index = stream.index();
        if let Some(m) = module(stream)? {
            parent_module.add_module(m);
        }

        if let Some(f) = function_def(stream)? {
            parent_module.add_function(f)?;
        }
        if let Some(c) = coroutine_def(stream)? {
            parent_module.add_coroutine(c)?;
        }

        if let Some(s) = struct_def(stream)? {
            parent_module.add_struct(s)?;
        }

        if let Some(e) = extern_def(stream)? {
            parent_module.add_extern(e)?;
        }

        if stream.index() == start_index {
            break;
        }
    }

    Ok(Some(parent_module))
}

fn extern_def(stream: &mut TokenStream) -> ParserResult<Extern<u32>> {
    match stream.next_if(&Lex::Extern) {
        Some(token) => match function_decl(stream)? {
            Some((fn_line, fn_name, params, fn_type)) => {
                stream.next_must_be(&Lex::Semicolon)?;
                Ok(Some(Extern::new(&fn_name, fn_line, params, fn_type)))
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
) -> ParserResult<(u32, String, Vec<Parameter<u32>>, Type)> {
    let fn_line = match stream.next_if(&Lex::FunctionDef) {
        Some(co) => co.l,
        None => return Ok(None),
    };

    let (fn_line, fn_name) = stream
        .next_if_id()
        .ok_or(format!("L{}: Expected identifier after fn", fn_line))?;
    let params = fn_def_params(stream)?;
    let fn_type = if stream.next_if(&Lex::LArrow).is_some() {
        consume_type(stream)?.ok_or(format!("L{}: Expected type after ->", fn_line))?
    } else {
        Type::Unit
    };

    Ok(Some((fn_line, fn_name, params, fn_type)))
}

fn function_def(stream: &mut TokenStream) -> ParserResult<RoutineDef<u32>> {
    let (fn_line, fn_name, params, fn_type) = match function_decl(stream)? {
        Some((l, n, p, t)) => (l, n, p, t),
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
        ty: fn_type,
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
    let params = fn_def_params(stream)?;
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
        ty: co_type,
        body: stmts,
    }))
}

pub(super) fn block(stream: &mut TokenStream) -> Result<Vec<Statement<ParserInfo>>> {
    let mut stmts = vec![];
    while let Some(s) = statement(stream)? {
        stmts.push(s);
    }
    Ok(stmts)
}

fn co_block(stream: &mut TokenStream) -> Result<Vec<Statement<ParserInfo>>> {
    let mut stmts = vec![];
    while let Some(s) = statement_or_yield_return(stream)? {
        stmts.push(s);
    }
    Ok(stmts)
}

fn fn_def_params(stream: &mut TokenStream) -> Result<Vec<Parameter<ParserInfo>>> {
    trace!(stream);
    stream.next_must_be(&Lex::LParen)?;
    let params = parameter_list(stream)?;
    stream.next_must_be(&Lex::RParen)?;

    Ok(params)
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
        path.push("root".into());
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
                // TODO: Support i8 and i16
                Expression::Integer32(_, l) => l as usize,
                Expression::Integer64(_, l) => l as usize,
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

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::lexer::lexer::Lexer;

    #[test]
    fn parse_unary_operators() {
        for (text, expected) in
            vec![("-a", UnaryOperator::Minus), ("!a", UnaryOperator::Not)].iter()
        {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            let exp = expression(&mut stream).unwrap();
            if let Some(Expression::UnaryOp(l, op, operand)) = exp {
                assert_eq!(op, *expected);
                assert_eq!(l, 1);
                assert_eq!(*operand, Expression::Identifier(1, "a".into()));
            } else {
                panic!("No nodes returned by parser for {:?} => {:?}", text, exp)
            }
        }
    }

    #[test]
    fn parse_double_unary_operators() {
        for (text, expected) in
            vec![("--a", UnaryOperator::Minus), ("!!a", UnaryOperator::Not)].iter()
        {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            let exp = expression(&mut stream).unwrap();
            if let Some(Expression::UnaryOp(l, op, operand)) = exp {
                assert_eq!(op, *expected);
                assert_eq!(l, 1);
                if let Expression::UnaryOp(l, op, operand) = *operand {
                    assert_eq!(op, *expected);
                    assert_eq!(l, 1);
                    assert_eq!(*operand, Expression::Identifier(1, "a".into()));
                }
            } else {
                panic!("No nodes returned by parser for {:?} => {:?}", text, exp)
            }
        }
    }

    #[test]
    fn parse_arithmetic_expressions() {
        for (text, expected) in vec![
            ("2+2", BinaryOperator::Add),
            ("2-2", BinaryOperator::Sub),
            ("2*2", BinaryOperator::Mul),
            ("2/2", BinaryOperator::Div),
            ("2==2", BinaryOperator::Eq),
            ("2!=2", BinaryOperator::NEq),
            ("2<2", BinaryOperator::Ls),
            ("2<=2", BinaryOperator::LsEq),
            ("2>2", BinaryOperator::Gr),
            ("2>=2", BinaryOperator::GrEq),
        ]
        .iter()
        {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            if let Some(Expression::BinaryOp(l, op, left, right)) = expression(&mut stream).unwrap()
            {
                assert_eq!(op, *expected);
                assert_eq!(l, 1);
                assert_eq!(*left, Expression::Integer64(1, 2));
                assert_eq!(*right, Expression::Integer64(1, 2));
            } else {
                panic!("No nodes returned by parser for {}", text)
            }
        }
    }

    #[test]
    fn parse_boolean_expresions() {
        for (text, expected) in vec![
            ("true && false", BinaryOperator::BAnd),
            ("true || false", BinaryOperator::BOr),
        ]
        .iter()
        {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            if let Some(Expression::BinaryOp(l, op, left, right)) = expression(&mut stream).unwrap()
            {
                assert_eq!(op, *expected);
                assert_eq!(l, 1);
                assert_eq!(*left, Expression::Boolean(1, true));
                assert_eq!(*right, Expression::Boolean(1, false));
            } else {
                panic!("No nodes returned by parser")
            }
        }
    }

    #[test]
    fn parse_nested_arithmetic_expression() {
        let text = "(2 + 4) * 3";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        if let Some(Expression::BinaryOp(l, BinaryOperator::Mul, left, right)) =
            expression(&mut stream).unwrap()
        {
            assert_eq!(l, 1);
            match left.as_ref() {
                Expression::BinaryOp(_, BinaryOperator::Add, ll, lr) => {
                    assert_eq!(**ll, Expression::Integer64(1, 2));
                    assert_eq!(**lr, Expression::Integer64(1, 4));
                }
                _ => panic!("Expected Add syntax"),
            }
            assert_eq!(*right, Expression::Integer64(1, 3));
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_boolean_expression() {
        let text = "true || false";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        if let Some(Expression::BinaryOp(l, BinaryOperator::BOr, left, right)) =
            expression(&mut stream).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(*left, Expression::Boolean(1, true));
            assert_eq!(*right, Expression::Boolean(1, false));
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_path() {
        for (text, expected) in vec![
            ("thing", Ok(vec!["thing"])),
            ("::thing", Ok(vec!["root", "thing"])),
            ("thing::first", Ok(vec!["thing", "first"])),
            ("thing::first::second", Ok(vec!["thing", "first", "second"])),
            (
                "thing::",
                Err("L1: expect identifier after path separator '::'"),
            ),
            (
                "thing::first::",
                Err("L1: expect identifier after path separator '::'"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            match expression(&mut stream) {
                Ok(Some(Expression::Path(l, path))) => {
                    assert_eq!(l, 1);
                    match expected {
                        Ok(expected) => assert_eq!(path, expected.into()),
                        Err(msg) => assert!(false, "{}", msg),
                    }
                }
                Ok(Some(Expression::Identifier(l, id))) => {
                    assert_eq!(l, 1);
                    match expected {
                        Ok(expected) => {
                            assert_eq!(expected.len(), 1);
                            assert_eq!(id, expected[0]);
                        }
                        Err(msg) => assert!(false, "{}", msg),
                    }
                }
                Ok(Some(n)) => panic!("{} resulted in {:?}, expected {:?}", text, n, expected),
                Ok(None) => panic!("No node returned for {}, expected {:?}", text, expected),
                Err(msg) => match expected {
                    Ok(_) => assert!(false, "{}", msg),
                    Err(expected) => assert_eq!(expected, msg),
                },
            }
        }
    }

    #[test]
    fn parse_multiple_member_access() {
        for text in vec![
            "thing.first.second",
            "(thing).first.second",
            "(thing.first).second",
            "((thing.first).second)",
            "(thing.first.second)",
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            match expression(&mut stream) {
                Ok(Some(Expression::MemberAccess(l, left, right))) => {
                    assert_eq!(l, 1);
                    assert_eq!(
                        *left,
                        Expression::MemberAccess(
                            1,
                            Box::new(Expression::Identifier(1, "thing".into())),
                            "first".into()
                        ),
                        "Input: {}",
                        text,
                    );
                    assert_eq!(right, "second");
                }
                Ok(Some(n)) => panic!("{} resulted in {:?}", text, n),
                Ok(None) => panic!("No node returned for {}", text),
                Err(msg) => panic!("{} caused {}", text, msg),
            }
        }
    }

    #[test]
    fn parse_bind() {
        let text = "let x:i64 := 5;";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Statement::Bind(box b) => {
                assert_eq!(b.get_id(), "x");
                assert_eq!(b.get_type(), Type::I64);
                assert_eq!(b.is_mutable(), false);
                assert_eq!(*b.get_rhs(), Expression::Integer64(1, 5));
            }
            _ => panic!("Not a binding statement"),
        }
    }

    #[test]
    fn parse_mut_bind() {
        let text = "let mut x:i64 := 5;";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Statement::Bind(box b) => {
                assert_eq!(b.get_id(), "x");
                assert_eq!(b.get_type(), Type::I64);
                assert_eq!(b.is_mutable(), true);
                assert_eq!(*b.get_rhs(), Expression::Integer64(1, 5));
            }
            _ => panic!("Not a binding statement"),
        }
    }

    #[test]
    fn parse_primitives() {
        for (text, expected_ty) in vec![
            ("let x:u64 := 5u64;", Type::U64),
            ("let x:i8 := 5;", Type::I8),
            ("let x:i8 := 5i8;", Type::I8),
            ("let x:i16 := 5;", Type::I16),
            ("let x:i16 := 5i16;", Type::I16),
            ("let x:i32 := 5;", Type::I32),
            ("let x:i32 := 5i32;", Type::I32),
            ("let x:i64 := 5;", Type::I64),
            ("let x: bool := true;", Type::Bool),
            ("let x: string := \"hello\";", Type::StringLiteral),
            (
                "let x: [i32;5] := [1, 2, 3, 4, 5];",
                Type::Array(Box::new(Type::I32), 5),
            ),
        ]
        .iter()
        {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);

            let stm = statement(&mut stream);
            assert!(stm.is_ok(), "{}", text);

            match stm.unwrap().unwrap() {
                Statement::Bind(box b) => {
                    assert_eq!(b.get_id(), "x", "{}", text);
                    assert_eq!(b.get_type(), expected_ty, "{}", text);
                    assert_eq!(b.is_mutable(), false, "{}", text);
                }
                _ => panic!("Not a binding statement"),
            }
        }
    }

    #[test]
    fn parse_mutation() {
        let text = "mut x := 5;";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Statement::Mutate(box m) => {
                assert_eq!(m.get_id(), "x");
                assert_eq!(*m.get_rhs(), Expression::Integer64(1, 5));
            }
            _ => panic!("Not a binding statement"),
        }
    }

    #[test]
    fn parse_module_empty() {
        let text = "mod test_mod {}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(m) = module(&mut iter).unwrap() {
            assert_eq!(*m.annotation(), 1);
            assert_eq!(m.get_name(), "test_mod");
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_module_with_function() {
        let text = "mod test_fn_mod { fn test(x:i64) {return;} }";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();

        if let Some(m) = parse(tokens).unwrap() {
            assert_eq!(*m.annotation(), 1);
            assert_eq!(m.get_name(), "root");

            assert_eq!(m.get_modules().len(), 1);
            assert_eq!(m.get_functions().len(), 0);
            assert_eq!(m.get_coroutines().len(), 0);
            assert_eq!(m.get_structs().len(), 0);

            let m = &m.get_modules()[0];
            assert_eq!(*m.annotation(), 1);
            assert_eq!(m.get_name(), "test_fn_mod");

            assert_eq!(m.get_modules().len(), 0);
            assert_eq!(m.get_functions().len(), 1);
            assert_eq!(m.get_coroutines().len(), 0);
            assert_eq!(m.get_structs().len(), 0);
            if let Item::Routine(RoutineDef {
                annotations,
                def: RoutineDefType::Function,
                name,
                params,
                ty,
                body,
                ..
            }) = &m.get_functions()[0]
            {
                assert_eq!(*annotations, 1);
                assert_eq!(name, "test");
                assert_eq!(params, &vec![Parameter::new(1, "x", &Type::I64)]);
                assert_eq!(ty, &Type::Unit);
                assert_eq!(body.len(), 1);
                match &body[0] {
                    Statement::Return(box r) => assert_eq!(*r.get_value(), None),
                    _ => panic!("Wrong body, expected unit return"),
                }
            } else {
                panic!("Expected function definition")
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_module_with_coroutine() {
        let text = "mod test_co_mod { co test(x:i64) {return;} }";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(m) = module(&mut iter).unwrap() {
            assert_eq!(*m.annotation(), 1);
            assert_eq!(m.get_name(), "test_co_mod");

            assert_eq!(m.get_modules().len(), 0);
            assert_eq!(m.get_functions().len(), 0);
            assert_eq!(m.get_coroutines().len(), 1);
            assert_eq!(m.get_structs().len(), 0);

            if let Some(Item::Routine(RoutineDef {
                annotations,
                def: RoutineDefType::Coroutine,
                name,
                params,
                ty,
                body,
                ..
            })) = m.get_item("test")
            {
                assert_eq!(*annotations, 1);
                assert_eq!(name, "test");
                assert_eq!(params, &vec![Parameter::new(1, "x", &Type::I64)]);
                assert_eq!(ty, &Type::Unit);
                assert_eq!(body.len(), 1);
                match &body[0] {
                    Statement::Return(box r) => assert_eq!(*r.get_value(), None),
                    _ => panic!("Wrong body, expected unit return"),
                }
            } else {
                panic!("Expected coroutine definition")
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_module_with_struct() {
        let text = "mod test_struct_mod { struct my_struct{x: i64} }";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(m) = module(&mut iter).unwrap() {
            assert_eq!(*m.annotation(), 1);
            assert_eq!(m.get_name(), "test_struct_mod");

            assert_eq!(m.get_modules().len(), 0);
            assert_eq!(m.get_functions().len(), 0);
            assert_eq!(m.get_coroutines().len(), 0);
            assert_eq!(m.get_structs().len(), 1);

            if let Some(Item::Struct(sd)) = m.get_item("my_struct") {
                assert_eq!(*sd.annotation(), 1);
                assert_eq!(sd.get_name(), "my_struct");
                assert_eq!(sd.get_fields(), &vec![Parameter::new(1, "x", &Type::I64)]);
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_module_with_extern() {
        let text = "mod test_extern_mod { extern fn my_fn(x: i64) -> i32; }";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(m) = module(&mut iter).unwrap() {
            assert_eq!(*m.annotation(), 1);
            assert_eq!(m.get_name(), "test_extern_mod");

            assert_eq!(m.get_modules().len(), 0);
            assert_eq!(m.get_functions().len(), 0);
            assert_eq!(m.get_coroutines().len(), 0);
            assert_eq!(m.get_structs().len(), 0);
            assert_eq!(m.get_externs().len(), 1);

            if let Some(Item::Extern(e)) = m.get_item("my_fn") {
                assert_eq!(*e.annotation(), 1);
                assert_eq!(e.get_name(), "my_fn");
                assert_eq!(e.get_params(), &vec![Parameter::new(1, "x", &Type::I64)]);
                assert_eq!(e.get_return_type(), Type::I32);
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_unit_function_def() {
        let text = "fn test(x:i64) {return;}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(RoutineDef {
            annotations: l,
            def: RoutineDefType::Function,
            name,
            params,
            ty,
            body,
            ..
        }) = function_def(&mut iter).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(name, "test");
            assert_eq!(params, vec![Parameter::new(1, "x", &Type::I64)]);
            assert_eq!(ty, Type::Unit);
            assert_eq!(body.len(), 1);
            match &body[0] {
                Statement::Return(box r) => assert_eq!(*r.get_value(), None),
                _ => panic!("Wrong body, expected unit return"),
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_function_def() {
        let text = "fn test(x:i64) -> bool {return true;}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(RoutineDef {
            annotations: l,
            def: RoutineDefType::Function,
            name,
            params,
            ty,
            body,
            ..
        }) = function_def(&mut iter).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(name, "test");
            assert_eq!(params, vec![Parameter::new(1, "x", &Type::I64)]);
            assert_eq!(ty, Type::Bool);
            assert_eq!(body.len(), 1);
            match &body[0] {
                Statement::Return(box r) => {
                    assert_eq!(*r.get_value(), Some(Expression::Boolean(1, true)));
                }
                _ => panic!("No body"),
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_routine_call() {
        let text = "test(x, y)";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(Expression::RoutineCall(l, RoutineCall::Function, name, params)) =
            expression(&mut iter).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(name, vec!["test"].into());
            assert_eq!(
                params,
                vec![
                    Expression::Identifier(1, "x".into()),
                    Expression::Identifier(1, "y".into())
                ]
            );
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_routine_by_path_call() {
        let text = "self::test(x, y)";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(Expression::RoutineCall(l, RoutineCall::Function, name, params)) =
            expression(&mut iter).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(name, vec!["self", "test"].into());
            assert_eq!(
                params,
                vec![
                    Expression::Identifier(1, "x".into()),
                    Expression::Identifier(1, "y".into())
                ]
            );
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_coroutine_def() {
        let text = "co test(x:i64) -> bool {return true;}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        if let Some(m) = parse(tokens).unwrap() {
            assert_eq!(*m.annotation(), 1);
            if let Some(Item::Routine(RoutineDef {
                def: RoutineDefType::Coroutine,
                name,
                params,
                ty,
                body,
                ..
            })) = m.get_item("test")
            {
                assert_eq!(name, "test");
                assert_eq!(params, &vec![Parameter::new(1, "x", &Type::I64)]);
                assert_eq!(ty, &Type::Bool);
                assert_eq!(body.len(), 1);
                match &body[0] {
                    Statement::Return(box r) => {
                        assert_eq!(*r.get_value(), Some(Expression::Boolean(1, true)));
                    }
                    _ => panic!("No body"),
                }
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_coroutine_init() {
        let text = "let x:co i64 := init c(1, 2);";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Statement::Bind(box b) => {
                assert_eq!(b.get_id(), "x");
                assert_eq!(b.get_type(), Type::Coroutine(Box::new(Type::I64)));
                assert_eq!(
                    *b.get_rhs(),
                    Expression::RoutineCall(
                        1,
                        RoutineCall::CoroutineInit,
                        vec!["c"].into(),
                        vec![Expression::Integer64(1, 1), Expression::Integer64(1, 2)]
                    )
                );
            }
            _ => panic!("Not a binding statement"),
        }
    }

    #[test]
    fn parse_coroutine_path_init() {
        let text = "let x:co i64 := init a::b::c(1, 2);";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Statement::Bind(box b) => {
                assert_eq!(b.get_id(), "x");
                assert_eq!(b.get_type(), Type::Coroutine(Box::new(Type::I64)));
                assert_eq!(
                    *b.get_rhs(),
                    Expression::RoutineCall(
                        1,
                        RoutineCall::CoroutineInit,
                        vec!["a", "b", "c"].into(),
                        vec![Expression::Integer64(1, 1), Expression::Integer64(1, 2)]
                    )
                );
            }
            _ => panic!("Not a binding statement"),
        }
    }

    #[test]
    fn parse_yield() {
        let text = "fn test(x:i64) -> bool {return yield cor;}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(RoutineDef {
            annotations: l,
            def: RoutineDefType::Function,
            name,
            params,
            ty,
            body,
            ..
        }) = function_def(&mut iter).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(name, "test");
            assert_eq!(params, vec![Parameter::new(1, "x", &Type::I64)]);
            assert_eq!(ty, Type::Bool);
            assert_eq!(body.len(), 1);
            match &body[0] {
                Statement::Return(box r) => {
                    assert_eq!(
                        *r.get_value(),
                        Some(Expression::Yield(
                            1,
                            Box::new(Expression::Identifier(1, "cor".into()))
                        ))
                    );
                }
                _ => panic!("No body"),
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_if_expression() {
        let text = "if (x) {5} else {7}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let exp = expression(&mut stream).unwrap();
        if let Some(Expression::If {
            annotation: l,
            cond,
            if_arm,
            else_arm,
        }) = exp
        {
            assert_eq!(l, 1);
            assert_eq!(*cond, Expression::Identifier(1, "x".into()));
            if let Expression::ExpressionBlock(_l, _body, Some(final_exp)) = *if_arm {
                assert_eq!(*final_exp, Expression::Integer64(1, 5));
            } else {
                panic!("Expected Expression block");
            }

            if let Some(box Expression::ExpressionBlock(_l, _body, Some(final_exp))) = else_arm {
                assert_eq!(*final_exp, Expression::Integer64(1, 7));
            } else {
                panic!("Expected Expression block");
            }
        } else {
            panic!("No nodes returned by parser, got: {:?}", exp)
        }
    }

    #[test]
    fn parse_if_else_if_expression() {
        let text = "if (x) {5} else if (y && z) {7} else {8}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let exp = expression(&mut stream).unwrap();
        if let Some(Expression::If {
            annotation: l,
            cond,
            if_arm,
            else_arm,
        }) = exp
        {
            assert_eq!(l, 1);
            assert_eq!(*cond, Expression::Identifier(1, "x".into()));
            if let Expression::ExpressionBlock(_l, _body, Some(final_exp)) = *if_arm {
                assert_eq!(*final_exp, Expression::Integer64(1, 5));
            } else {
                panic!("Expected Expression block");
            }

            if let Some(box Expression::If {
                cond,
                if_arm,
                else_arm,
                ..
            }) = else_arm
            {
                assert_eq!(
                    *cond,
                    Expression::BinaryOp(
                        1,
                        BinaryOperator::BAnd,
                        Box::new(Expression::Identifier(1, "y".into())),
                        Box::new(Expression::Identifier(1, "z".into()))
                    )
                );
                if let Expression::ExpressionBlock(_l, _body, Some(final_exp)) = *if_arm {
                    assert_eq!(*final_exp, Expression::Integer64(1, 7));
                } else {
                    panic!("Expected Expression block");
                }

                if let Some(box Expression::ExpressionBlock(_l, _body, Some(final_exp))) = else_arm
                {
                    assert_eq!(*final_exp, Expression::Integer64(1, 8));
                } else {
                    panic!("Expected Expression block");
                }
            } else {
                panic!("Expected if statement in else arm");
            }
        } else {
            panic!("No nodes returned by parser, got: {:?}", exp)
        }
    }

    #[test]
    fn parse_while_expression() {
        let text = "while (x) {5;}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let exp = expression(&mut stream).unwrap();
        if let Some(Expression::While {
            annotation: l,
            cond,
            body,
        }) = exp
        {
            assert_eq!(l, 1);
            assert_eq!(*cond, Expression::Identifier(1, "x".into()));
            if let Expression::ExpressionBlock(_l, body, None) = *body {
                assert_eq!(
                    body[0],
                    Statement::Expression(box Expression::Integer64(1, 5))
                );
            } else {
                panic!("Expected Expression block, got {:?}", *body);
            }
        } else {
            panic!("No nodes returned by parser, got: {:?}", exp)
        }
    }

    #[test]
    fn parse_struct_def() {
        for (text, expected) in vec![
            ("struct MyStruct {}", StructDef::new("MyStruct", 1, vec![])),
            (
                "struct MyStruct {x: i64}",
                StructDef::new("MyStruct", 1, vec![Parameter::new(0, "x", &Type::I64)]),
            ),
            (
                "struct MyStruct {x: i64, y: bool}",
                StructDef::new(
                    "MyStruct",
                    1,
                    vec![
                        Parameter::new(0, "x", &Type::I64),
                        Parameter::new(0, "y", &Type::Bool),
                    ],
                ),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            if let Some(m) = module(&mut stream).unwrap() {
                assert_eq!(m.get_structs()[0], Item::Struct(expected), "{:?}", text);
            }
        }
    }

    #[test]
    fn parse_struct_init() {
        for (text, expected) in vec![
            (
                "MyStruct{}",
                Expression::StructExpression(1, vec!["MyStruct"].into(), vec![]),
            ),
            (
                "MyStruct{x: 5}",
                Expression::StructExpression(
                    1,
                    vec!["MyStruct"].into(),
                    vec![("x".into(), Expression::Integer64(1, 5))],
                ),
            ),
            (
                "MyStruct{x: 5, y: false}",
                Expression::StructExpression(
                    1,
                    vec!["MyStruct"].into(),
                    vec![
                        ("x".into(), Expression::Integer64(1, 5)),
                        ("y".into(), Expression::Boolean(1, false)),
                    ],
                ),
            ),
            (
                "MyStruct{x: 5, y: MyStruct2{z:3}}",
                Expression::StructExpression(
                    1,
                    vec!["MyStruct"].into(),
                    vec![
                        ("x".into(), Expression::Integer64(1, 5)),
                        (
                            "y".into(),
                            Expression::StructExpression(
                                1,
                                vec!["MyStruct2"].into(),
                                vec![("z".into(), Expression::Integer64(1, 3))],
                            ),
                        ),
                    ],
                ),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            let result = expression(&mut stream);
            assert_eq!(result, Ok(Some(expected)), "{:?}", text);
        }
    }

    #[test]
    fn parse_string_literals() {
        for (text, expected) in vec![
            ("fn test() -> String {return \"test\";}", "test"),
            ("fn test() -> String {return \"test 2\";}", "test 2"),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let module = parse(tokens).unwrap();
            match module {
                Some(m) => match &m.get_functions()[0] {
                    Item::Routine(RoutineDef { body, .. }) => match &body[0] {
                        Statement::Return(box r) => assert_eq!(
                            *r.get_value(),
                            Some(Expression::StringLiteral(1, expected.into()))
                        ),
                        _ => assert!(false, "Not a return statement"),
                    },
                    _ => assert!(false, "Not a return statement"),
                },
                _ => assert!(false, "Not a routine, got {:?}", module),
            }
        }
    }
}
