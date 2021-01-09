use std::sync::atomic::AtomicBool;
use std::sync::atomic::{AtomicUsize, Ordering};

use stdext::function_name;

use crate::{
    diagnostics::config::TracingConfig,
    lexer::tokens::{Lex, Primitive, Token},
};

// AST - a type(s) which is used to construct an AST representing the logic of the
// program
// Each type of node represents an expression and the only requirement is that at the
// end of computing an expression its result is in EAX
use super::pnode::PNode;
use super::pnode::PResult;
use super::tokenstream::TokenStream;
use super::{ast::*, pnode::ParserCombinator};

static ENABLE_TRACING: AtomicBool = AtomicBool::new(false);
static TRACE_START: AtomicUsize = AtomicUsize::new(0);
static TRACE_END: AtomicUsize = AtomicUsize::new(0);

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

pub fn parse(tokens: Vec<Token>) -> PResult {
    let mut stream = TokenStream::new(&tokens);
    let start_index = stream.index();
    let mut item = None;
    while stream.peek().is_some() {
        //item = module(&mut stream).map_err(|e| format!("Parser: {}", e))?;
        item = parse_items("root", &mut stream)?;

        if stream.index() == start_index {
            return Err(format!("Parser cannot advance past {:?}", stream.peek()));
        }
    }

    Ok(item)
}

fn module(stream: &mut TokenStream) -> PResult {
    let mod_def = match stream.next_if(&Lex::ModuleDef) {
        Some(token) => match stream.next_if_id() {
            Some((_, module_name)) => {
                stream.next_must_be(&Lex::LBrace)?;
                let module = parse_items(&module_name, stream)?;
                stream.next_must_be(&Lex::RBrace)?;
                module
            },
            _ => {
                return Err(format!("L{}: expected name after mod keyword", token.l));
            },
        },
        None => {
            None
        },
    };

    Ok(mod_def)
}

fn parse_items(name: &str, stream: &mut TokenStream) -> PResult {
    let mut functions = vec![];
    let mut coroutines = vec![];
    let mut structs = vec![];
    let mut modules = vec![];

    let module_line = stream.peek().map_or(1, |t| t.l);
    while stream.peek().is_some() {
        let start_index = stream.index();
        if let Some(m) = module(stream)? {
            modules.push(m);
        }

        if let Some(f) = function_def(stream)? {
            functions.push(f);
        }
        if let Some(c) = coroutine_def(stream)? {
            coroutines.push(c);
        }

        if let Some(s) = struct_def(stream)? {
            structs.push(s);
        }

        if stream.index() == start_index {
            //return Err(format!("Parser cannot advance past {:?}", stream.peek()));
            break;
        }
    }

    Ok(Some(Ast::Module {
        meta: module_line,
        name: name.into(),
        modules,
        functions,
        coroutines,
        structs,
    }))
}

fn struct_def(stream: &mut TokenStream) -> PResult {
    match stream.next_if(&Lex::Struct) {
        Some(token) => match stream.next_if_id() {
            Some((line, id)) => {
                stream.next_must_be(&Lex::LBrace)?;
                let fields = id_declaration_list(stream)?;
                stream.next_must_be(&Lex::RBrace)?;
                Ok(Some(Ast::StructDef(line, id.clone(), fields)))
            }
            None => Err(format!("L{}: expected identifer after struct", token.l)),
        },
        None => Ok(None),
    }
}

fn function_def(stream: &mut TokenStream) -> PResult {
    let fn_line = match stream.next_if(&Lex::FunctionDef) {
        Some(co) => co.l,
        None => return Ok(None),
    };

    let (fn_line, fn_name) = stream
        .next_if_id()
        .ok_or(format!("L{}: Expected identifier after fn", fn_line))?;
    let params = fn_def_params(stream)?;
    let fn_type = if stream.next_if(&Lex::LArrow).is_some() {
        consume_type(stream).ok_or(format!("L{}: Expected type after ->", fn_line))?
    } else {
        Type::Unit
    };

    stream.next_must_be(&Lex::LBrace)?;
    let mut stmts = block(stream)?;

    match return_stmt(stream)? {
        Some(ret) => stmts.push(ret),
        None => {
            return Err(format!(
                "L{}: Function must end with a return statement, got {:?}",
                stmts.last().map_or(fn_line, |s| *s.get_metadata()),
                stream.peek(),
            ))
        }
    }
    stream.next_must_be(&Lex::RBrace)?;

    Ok(Some(Ast::RoutineDef(
        fn_line,
        RoutineDef::Function,
        fn_name,
        params,
        fn_type,
        stmts,
    )))
}

fn coroutine_def(stream: &mut TokenStream) -> PResult {
    let co_line = match stream.next_if(&Lex::CoroutineDef) {
        Some(co) => co.l,
        None => return Ok(None),
    };

    let (co_line, co_name) = stream
        .next_if_id()
        .ok_or(format!("L{}: Expected identifier after co", co_line))?;
    let params = fn_def_params(stream)?;
    let co_type = match stream.next_if(&Lex::LArrow) {
        Some(t) => consume_type(stream).ok_or(format!("L{}: Expected type after ->", t.l))?,
        _ => Type::Unit,
    };

    stream.next_must_be(&Lex::LBrace)?;
    let mut stmts = co_block(stream)?;

    match return_stmt(stream)? {
        Some(ret) => stmts.push(ret),
        None => {
            return Err(format!(
                "L{}: Coroutine must end with a return statement",
                stmts.last().map_or(co_line, |s| *s.get_metadata()),
            ))
        }
    }
    stream.next_must_be(&Lex::RBrace)?;

    Ok(Some(Ast::RoutineDef(
        co_line,
        RoutineDef::Coroutine,
        co_name,
        params,
        co_type,
        stmts,
    )))
}

fn block(stream: &mut TokenStream) -> Result<Vec<PNode>, String> {
    let mut stmts = vec![];
    while let Some(s) = statement(stream)? {
        stmts.push(s);
    }
    Ok(stmts)
}

fn co_block(stream: &mut TokenStream) -> Result<Vec<PNode>, String> {
    let mut stmts = vec![];
    while let Some(s) = statement(stream).por(yield_return_stmt, stream)? {
        stmts.push(s);
    }
    Ok(stmts)
}

fn statement(stream: &mut TokenStream) -> PResult {
    let start_index = stream.index();
    let must_have_semicolon = stream.test_if_one_of(vec![Lex::Let, Lex::Mut]);
    let stm = let_bind(stream)
        .por(mutate, stream)
        .por(println_stmt, stream)
        .por(expression, stream)?;

    match stm {
        Some(stm) => match stream.next_if(&Lex::Semicolon) {
            Some(Token { l, s: _ }) => Ok(Some(Ast::Statement(l, Box::new(stm)))),
            _ => {
                if must_have_semicolon {
                    let line = *stm.get_metadata();
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

fn expression_block(stream: &mut TokenStream) -> PResult {
    match stream.next_if(&Lex::LBrace) {
        Some(token) => {
            let mut stmts = block(stream)?;

            match expression(stream)? {
                Some(e) => stmts.push(e),
                None => (),
            }

            stream.next_must_be(&Lex::RBrace)?;
            Ok(Some(Ast::ExpressionBlock(token.l, stmts)))
        }
        None => Ok(None),
    }
}

fn expression(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    logical_or(stream)
}

fn logical_or(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    binary_op(stream, &vec![Lex::BOr], logical_and)
}

fn logical_and(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    binary_op(stream, &vec![Lex::BAnd], comparison)
}

fn comparison(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    binary_op(
        stream,
        &vec![Lex::Eq, Lex::NEq, Lex::Ls, Lex::LsEq, Lex::Gr, Lex::GrEq],
        sum,
    )
}

fn sum(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    binary_op(stream, &vec![Lex::Add, Lex::Minus], term)
}

fn term(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    binary_op(stream, &vec![Lex::Mul, Lex::Div], negate)
}

fn binary_op(
    stream: &mut TokenStream,
    test: &Vec<Lex>,
    left_pattern: fn(&mut TokenStream) -> PResult,
) -> PResult {
    trace!(stream);
    match left_pattern(stream)? {
        Some(left) => match stream.next_if_one_of(test.clone()) {
            Some(op) => {
                let right = binary_op(stream, test, left_pattern)?
                    .ok_or(format!("L{}: expected expression after {}", op.l, op.s))?;
                PNode::binary_op(op.l, &op.s, Box::new(left), Box::new(right))
            }
            None => Ok(Some(left)),
        },
        None => Ok(None),
    }
}

fn negate(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    match stream.next_if_one_of(vec![Lex::Minus, Lex::Not]) {
        Some(op) => {
            let factor =
                negate(stream)?.ok_or(&format!("L{}: expected term after {}", op.l, op.s))?;
            PNode::unary_op(op.l, &op.s, Box::new(factor))
        }
        None => member_access(stream),
    }
}

fn member_access(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    match factor(stream)? {
        Some(f) => {
            let mut ma = f;
            while let Some(token) = stream.next_if(&Lex::MemberAccess) {
                let line = token.l;
                let (_, member) = stream.next_if_id().ok_or(format!(
                    "L{}: expect field name after member access '.'",
                    line
                ))?;
                ma = Ast::MemberAccess(line, Box::new(ma), member);
            }
            Ok(Some(ma))
        }
        None => Ok(None),
    }
}

fn factor(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    match stream.peek() {
        Some(Token {
            l: _,
            s: Lex::LParen,
        }) => {
            stream.next();
            let exp = expression(stream)?;
            stream.next_must_be(&Lex::RParen)?;
            Ok(exp)
        }
        _ => if_expression(stream)
            .por(expression_block, stream)
            .por(function_call_or_variable, stream)
            .por(co_yield, stream)
            .por(constant, stream),
    }
}

fn if_expression(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    Ok(match stream.next_if(&Lex::If) {
        Some(token) => {
            stream.next_must_be(&Lex::LParen)?;
            let cond = expression(stream)?.ok_or(format!(
                "L{}: Expected conditional expression after if",
                token.l
            ))?;
            stream.next_must_be(&Lex::RParen)?;

            let true_arm = expression_block(stream)?.ok_or(format!(
                "L{}: Expression in true arm of if expression",
                token.l
            ))?;
            stream.next_must_be(&Lex::Else)?;

            // check for `else if`
            let false_arm = match stream.peek() {
                Some(Token { l, s: Lex::If }) => {
                    let l = *l;
                    if_expression(stream)?
                        .ok_or(format!("L{}: Expected if expression after else if", l))?
                }
                _ => {
                    let false_arm = expression_block(stream)?.ok_or(&format!(
                        "L{}: Expression in false arm of if expression",
                        token.l
                    ))?;
                    false_arm
                }
            };
            Some(Ast::If(
                token.l,
                Box::new(cond),
                Box::new(true_arm),
                Box::new(false_arm),
            ))
        }
        _ => None,
    })
}

fn fn_def_params(stream: &mut TokenStream) -> Result<Vec<(String, Type)>, String> {
    trace!(stream);
    stream.next_must_be(&Lex::LParen)?;
    let params = id_declaration_list(stream)?;
    stream.next_must_be(&Lex::RParen)?;

    Ok(params)
}

fn id_declaration_list(stream: &mut TokenStream) -> Result<Vec<(String, Type)>, String> {
    trace!(stream);
    let mut decls = vec![];

    while let Some(token) = id_declaration(stream)? {
        match token {
            Ast::IdentifierDeclare(_line, id, ty) => {
                decls.push((id, ty));
                stream.next_if(&Lex::Comma);
            }
            _ => panic!("CRITICAL: IdDeclaration not returned by id_declaration"),
        }
    }

    Ok(decls)
}

/// LPAREN [EXPRESSION [, EXPRESSION]*] RPAREN
fn fn_call_params(stream: &mut TokenStream) -> Result<Option<Vec<PNode>>, String> {
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

fn struct_init_params(stream: &mut TokenStream) -> Result<Option<Vec<(String, PNode)>>, String> {
    trace!(stream);
    match stream.next_if(&Lex::LBrace) {
        Some(_token) => {
            let mut params = vec![];
            while let Some((line, field_name)) = stream.next_if_id() {
                stream.next_must_be(&Lex::Colon)?;
                let field_value = expression(stream)?.ok_or(format!(
                    "L{}: expected an expression to be assigned to field {}",
                    line, field_name
                ))?;
                params.push((field_name, field_value));
                match stream.next_if(&Lex::Comma) {
                    Some(_) => {}
                    None => break,
                };
            }

            stream.next_must_be(&Lex::RBrace)?;
            Ok(Some(params))
        }
        _ => Ok(None),
    }
}

fn return_stmt(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    Ok(match stream.next_if(&Lex::Return) {
        Some(token) => {
            let exp = expression(stream)?;
            stream.next_must_be(&Lex::Semicolon)?;
            match exp {
                Some(exp) => Some(Ast::Return(token.l, Some(Box::new(exp)))),
                None => Some(Ast::Return(token.l, None)),
            }
        }
        _ => None,
    })
}

fn yield_return_stmt(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    Ok(match stream.next_if(&Lex::YieldReturn) {
        Some(token) => {
            let exp = expression(stream)?;
            stream.next_must_be(&Lex::Semicolon)?;
            match exp {
                Some(exp) => Some(Ast::YieldReturn(token.l, Some(Box::new(exp)))),
                None => Some(Ast::YieldReturn(token.l, None)),
            }
        }
        _ => None,
    })
}

fn co_yield(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    match stream.next_if(&Lex::Yield) {
        Some(token) => {
            let line = token.l;
            match expression(stream)? {
                Some(coroutine) => PNode::new_yield(*coroutine.get_metadata(), Box::new(coroutine)),
                None => Err(format!("L{}: expected an identifier after yield", line)),
            }
        }
        None => Ok(None),
    }
}

fn println_stmt(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    let syntax = match stream.next_if_one_of(vec![Lex::Printiln, Lex::Prints, Lex::Printbln]) {
        Some(print) => {
            let exp = expression(stream)?
                .ok_or(format!("L{}: Expected expression after println", print.l))?;
            match print.s {
                Lex::Printiln => Some(Ast::Printiln(print.l, Box::new(exp))),
                Lex::Prints => Some(Ast::Prints(print.l, Box::new(exp))),
                Lex::Printbln => Some(Ast::Printbln(print.l, Box::new(exp))),
                _ => panic!(
                    "CRITICAL: already tested for a print token but found {}",
                    print.s
                ),
            }
        }
        _ => None,
    };
    Ok(syntax)
}

fn let_bind(stream: &mut TokenStream) -> PResult {
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
            PNode::new_bind(token.l, Box::new(id_decl), is_mutable, Box::new(exp))
        }
        None => Ok(None),
    }
}

fn mutate(stream: &mut TokenStream) -> PResult {
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
            PNode::new_mutate(tokens[0].l, &id, Box::new(exp))
        }
    }
}

fn co_init(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    match stream.next_if(&Lex::Init) {
        Some(token) => match stream.next_if_id() {
            Some((l, id)) => {
                let params = fn_call_params(stream)?
                    .ok_or(&format!("L{}: Expected parameters after coroutine name", l))?;
                Ok(Some(Ast::RoutineCall(
                    l,
                    RoutineCall::CoroutineInit,
                    vec![id].into(),
                    params,
                )))
            }
            None => Err(format!("L{}: expected identifier after init", token.l)),
        },
        _ => Ok(None),
    }
}

fn function_call_or_variable(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    // parse path
    // has function parameters
    // has struct expression parameters
    /*function_call(stream)
        .por(struct_expression, stream)
        //.por(path, stream)
        .por(identifier, stream)*/
    function_call_or_variable2(stream)
}

fn function_call_or_variable2(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    let s: Option<Ast<u32>> = match path(stream)? {
        Some((line, path)) => {
            match fn_call_params(stream)? {
                Some(params) => Some(Ast::RoutineCall(line, RoutineCall::Function, path, params.clone())),
                None => match struct_init_params(stream)? {
                    Some(params) => Some(Ast::StructExpression(line, path.last().unwrap().clone(), params.clone())),
                    None => if path.len() > 1 {
                        Some(Ast::Path(line, path.into()))
                    } else {
                        Some(Ast::Identifier(line, path.last().unwrap().clone()))
                    },
                },
            }
        }
        _ => None
    };

    Ok(s)
}

// TODO: I think what I want ot do is pull the ID/Path parsing up in to `function_call_or_variable` and then
// determine if it's a function, struct expressoin, or variable by if there is a LParen or LBrace after the path.
fn function_call(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    if stream.test_ifn(vec![Lex::Identifier("".into()), Lex::LParen]) {
        let (line, fn_name) = stream
            .next_if_id()
            .expect("CRITICAL: failed to get identifier");
        let params = fn_call_params(stream)?
            .ok_or(format!("L{}: expected parameters in function call", line))?;
        Ok(Some(Ast::RoutineCall(
            line,
            RoutineCall::Function,
            vec![fn_name].into(),
            params,
        )))
    } else {
        Ok(None)
    }
}

fn struct_expression(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    if stream.test_ifn(vec![Lex::Identifier("".into()), Lex::LBrace]) {
        let (line, struct_name) = stream
            .next_if_id()
            .expect("CRITICAL: failed to get identifier");
        let fields = struct_init_params(stream)?.ok_or(format!(
            "L{}: Expected valid field assignments in struct expression",
            line
        ))?;
        Ok(Some(Ast::StructExpression(line, struct_name, fields)))
    } else {
        Ok(None)
    }
}

fn path(stream: &mut TokenStream) -> Result<Option<(u32, Path)>, String> {
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

fn identifier(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    match stream.next_if_id() {
        Some((line, id)) => Ok(Some(Ast::Identifier(line, id))),
        _ => Ok(None),
    }
}

fn consume_type(stream: &mut TokenStream) -> Option<Type> {
    trace!(stream);
    let is_coroutine = stream.next_if(&Lex::CoroutineDef).is_some();
    match stream.peek() {
        Some(Token {
            l: _,
            s: Lex::Primitive(primitive),
        }) => {
            let ty = match *primitive {
                Primitive::I32 => Some(Type::I32),
                Primitive::Bool => Some(Type::Bool),
                Primitive::StringLiteral => Some(Type::StringLiteral),
            };
            stream.next();
            ty
        }
        Some(Token {
            l: _,
            s: Lex::Identifier(name),
        }) => {
            let ty = Some(Type::Custom(name.clone()));
            stream.next();
            ty
        }
        _ => None,
    }
    .map(|ty| {
        if is_coroutine {
            Type::Coroutine(Box::new(ty))
        } else {
            ty
        }
    })
}

fn id_declaration(stream: &mut TokenStream) -> Result<Option<PNode>, String> {
    trace!(stream);
    match stream.next_ifn(vec![Lex::Identifier("".into()), Lex::Colon]) {
        Some(t) => {
            let line_id = t[0].l;
            let line_value = t[1].l;
            let id = t[0].s.get_str().expect(
                "CRITICAL: first token is an identifier but cannot be converted to a string",
            );
            let ty = consume_type(stream).ok_or(format!(
                "L{}: expected type after : in type declaration",
                line_value
            ))?;
            Ok(Some(Ast::IdentifierDeclare(line_id, id, ty)))
        }
        None => Ok(None),
    }
}

fn constant(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    number(stream)
        .por(boolean, stream)
        .por(string_literal, stream)
}

fn number(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    match stream.next_if(&Lex::Integer(0)) {
        Some(Token {
            l,
            s: Lex::Integer(i),
        }) => Ok(Some(Ast::Integer(l, i))),
        _ => Ok(None),
    }
}

fn boolean(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    match stream.next_if(&Lex::Bool(true)) {
        Some(Token { l, s: Lex::Bool(b) }) => Ok(Some(Ast::Boolean(l, b))),
        _ => Ok(None),
    }
}

fn string_literal(stream: &mut TokenStream) -> PResult {
    trace!(stream);
    match stream.next_if(&Lex::StringLiteral("".into())) {
        Some(Token {
            l,
            s: Lex::StringLiteral(s),
        }) => Ok(Some(Ast::StringLiteral(l, s))),
        _ => Ok(None),
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
                .collect::<Result<_, _>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            let exp = expression(&mut stream).unwrap();
            if let Some(Ast::UnaryOp(l, op, operand)) = exp {
                assert_eq!(op, *expected);
                assert_eq!(l, 1);
                assert_eq!(*operand, Ast::Identifier(1, "a".into()));
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
                .collect::<Result<_, _>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            let exp = expression(&mut stream).unwrap();
            if let Some(Ast::UnaryOp(l, op, operand)) = exp {
                assert_eq!(op, *expected);
                assert_eq!(l, 1);
                if let Ast::UnaryOp(l, op, operand) = *operand {
                    assert_eq!(op, *expected);
                    assert_eq!(l, 1);
                    assert_eq!(*operand, Ast::Identifier(1, "a".into()));
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
                .collect::<Result<_, _>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            if let Some(Ast::BinaryOp(l, op, left, right)) = expression(&mut stream).unwrap() {
                assert_eq!(op, *expected);
                assert_eq!(l, 1);
                assert_eq!(*left, Ast::Integer(1, 2));
                assert_eq!(*right, Ast::Integer(1, 2));
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
                .collect::<Result<_, _>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            if let Some(Ast::BinaryOp(l, op, left, right)) = expression(&mut stream).unwrap() {
                assert_eq!(op, *expected);
                assert_eq!(l, 1);
                assert_eq!(*left, Ast::Boolean(1, true));
                assert_eq!(*right, Ast::Boolean(1, false));
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
            .collect::<Result<_, _>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        if let Some(Ast::BinaryOp(l, BinaryOperator::Mul, left, right)) =
            expression(&mut stream).unwrap()
        {
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
        let text = "true || false";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        if let Some(Ast::BinaryOp(l, BinaryOperator::BOr, left, right)) =
            expression(&mut stream).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(*left, Ast::Boolean(1, true));
            assert_eq!(*right, Ast::Boolean(1, false));
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
                ("thing::", Err("L1: expect identifier after path separator '::'")),
                ("thing::first::", Err("L1: expect identifier after path separator '::'")),
            ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_, _>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            match expression(&mut stream) {
                Ok(Some(Ast::Path(l, path))) => {
                    assert_eq!(l, 1);
                    match expected {
                        Ok(expected) =>  assert_eq!(path, expected.into()),
                        Err(msg) => assert!(false, msg),
                    }
                }
                Ok(Some(Ast::Identifier(l, id))) => {
                    assert_eq!(l, 1);
                    match expected {
                        Ok(expected) =>  {
                            assert_eq!(expected.len(), 1);
                            assert_eq!(id, expected[0]);
                        }
                        Err(msg) => assert!(false, msg),
                    }
                }
                Ok(Some(n)) => panic!("{} resulted in {:?}, expected {:?}", text, n, expected),
                Ok(None) => panic!("No node returned for {}, expected {:?}", text, expected),
                Err(msg) => {
                    match expected {
                        Ok(_) =>  assert!(false, msg),
                        Err(expected) => assert_eq!(expected, msg),
                    }
                },
            }
        }
    }

    #[test]
    fn parse_member_access() {
        for text in vec!["thing.first", "(thing).first", "(thing.first)"] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_, _>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            match member_access(&mut stream) {
                Ok(Some(Ast::MemberAccess(l, left, right))) => {
                    assert_eq!(l, 1);
                    assert_eq!(*left, Ast::Identifier(1, "thing".into()), "Input: {}", text,);
                    assert_eq!(right, "first");
                }
                Ok(Some(n)) => panic!("{} resulted in {:?}", text, n),
                Ok(None) => panic!("No node returned for {}", text),
                Err(msg) => panic!("{} caused {}", text, msg),
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
                .collect::<Result<_, _>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            match expression(&mut stream) {
                Ok(Some(Ast::MemberAccess(l, left, right))) => {
                    assert_eq!(l, 1);
                    assert_eq!(
                        *left,
                        Ast::MemberAccess(
                            1,
                            Box::new(Ast::Identifier(1, "thing".into())),
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
        let text = "let x:i32 := 5;";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Ast::Statement(_, stm) => match stm.as_ref() {
                Ast::Bind(_, id, false, p, exp) => {
                    assert_eq!(id, "x");
                    assert_eq!(*p, Type::I32);
                    assert_eq!(*exp, Box::new(PNode::Integer(1, 5)));
                }
                _ => panic!("Not a binding statement"),
            },
            _ => panic!("No body: {:?}", stm),
        }
    }

    #[test]
    fn parse_mut_bind() {
        let text = "let mut x:i32 := 5;";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Ast::Statement(_, stm) => match stm.as_ref() {
                Ast::Bind(_, id, true, p, exp) => {
                    assert_eq!(id, "x");
                    assert_eq!(*p, Type::I32);
                    assert_eq!(*exp, Box::new(PNode::Integer(1, 5)));
                }
                _ => panic!("Not a binding statement"),
            },
            _ => panic!("No body: {:?}", stm),
        }
    }
    #[test]
    fn parse_mutation() {
        let text = "mut x := 5;";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Ast::Statement(_, stm) => match stm.as_ref() {
                Ast::Mutate(_, id, exp) => {
                    assert_eq!(id, "x");
                    assert_eq!(*exp, Box::new(PNode::Integer(1, 5)));
                }
                _ => panic!("Not a binding statement"),
            },
            _ => panic!("No body: {:?}", stm),
        }
    }

    #[test]
    fn parse_printiln() {
        let text = "printiln 5;";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Ast::Statement(_, stm) => match stm.as_ref() {
                Ast::Printiln(_, exp) => {
                    assert_eq!(*exp, Box::new(PNode::Integer(1, 5)));
                }
                _ => panic!("Not a binding statement"),
            },
            _ => panic!("No body: {:?}", stm),
        }
    }

    #[test]
    fn parse_printbln() {
        let text = "printbln true;";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Ast::Statement(_, stm) => match stm.as_ref() {
                Ast::Printbln(_, exp) => {
                    assert_eq!(*exp, Box::new(PNode::Boolean(1, true)));
                }
                _ => panic!("Not a binding statement"),
            },
            _ => panic!("No body: {:?}", stm),
        }
    }

    #[test]
    fn parse_prints() {
        let text = "prints \"hello\";";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Ast::Statement(_, stm) => match stm.as_ref() {
                Ast::Prints(_, exp) => {
                    assert_eq!(*exp, Box::new(PNode::StringLiteral(1, "hello".into())));
                }
                _ => panic!("Not a binding statement"),
            },
            _ => panic!("No body: {:?}", stm),
        }
    }

    #[test]
    fn parse_module_empty() {
        let text = "mod test_mod {}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(Ast::Module{meta, name, ..}) =
            module(&mut iter).unwrap()
        {
            assert_eq!(meta, 1);
            assert_eq!(name, "test_mod");
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_module_with_function() {
        let text = "mod test_fn_mod { fn test(x:i32) {return;} }";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        
        if let Some(Ast::Module{meta, name, modules, functions, coroutines, structs}) =
            parse(tokens).unwrap()
        {
            assert_eq!(meta, 1);
            assert_eq!(name, "root");

            assert_eq!(modules.len(), 1);
            assert_eq!(functions.len(), 0);
            assert_eq!(coroutines.len(), 0);
            assert_eq!(structs.len(), 0);

            if let Ast::Module{meta, name, modules, functions, coroutines, structs} = &modules[0] {
                assert_eq!(*meta, 1);
                assert_eq!(name, "test_fn_mod");

                assert_eq!(modules.len(), 0);
                assert_eq!(functions.len(), 1);
                assert_eq!(coroutines.len(), 0);
                assert_eq!(structs.len(), 0);
                if let Ast::RoutineDef(l, RoutineDef::Function, name, params, ty, body) =
                    &functions[0]
                {
                    assert_eq!(*l, 1);
                    assert_eq!(name, "test");
                    assert_eq!(params, &vec![("x".into(), Type::I32)]);
                    assert_eq!(ty, &Type::Unit);
                    assert_eq!(body.len(), 1);
                    match &body[0] {
                        Ast::Return(_, None) => {}
                        _ => panic!("Wrong body, expected unit return"),
                    }
                } else {
                    panic!("Expected function definition")
                }
            }

        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_module_with_coroutine() {
        let text = "mod test_co_mod { co test(x:i32) {return;} }";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(Ast::Module{meta, name, modules, functions, coroutines, structs}) =
            module(&mut iter).unwrap()
        {
            assert_eq!(meta, 1);
            assert_eq!(name, "test_co_mod");
            assert_eq!(modules.len(), 0);
            assert_eq!(functions.len(), 0);
            assert_eq!(coroutines.len(), 1);
            assert_eq!(structs.len(), 0);

            if let Ast::RoutineDef(l, RoutineDef::Coroutine, name, params, ty, body) =
                &coroutines[0]
            {
                assert_eq!(*l, 1);
                assert_eq!(name, "test");
                assert_eq!(params, &vec![("x".into(), Type::I32)]);
                assert_eq!(ty, &Type::Unit);
                assert_eq!(body.len(), 1);
                match &body[0] {
                    Ast::Return(_, None) => {}
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
        let text = "mod test_struct_mod { struct my_struct{x: i32} }";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(Ast::Module{meta, name, modules, functions, coroutines, structs}) =
            module(&mut iter).unwrap()
        {
            assert_eq!(meta, 1);
            assert_eq!(name, "test_struct_mod");
            assert_eq!(modules.len(), 0);
            assert_eq!(functions.len(), 0);
            assert_eq!(coroutines.len(), 0);
            assert_eq!(structs.len(), 1);

            if let Ast::StructDef(l, name, fields) = &structs[0] {
                assert_eq!(*l, 1);
                assert_eq!(name, "my_struct");
                assert_eq!(fields, &vec![("x".into(), Type::I32)]);
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_unit_function_def() {
        let text = "fn test(x:i32) {return;}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(Ast::RoutineDef(l, RoutineDef::Function, name, params, ty, body)) =
            function_def(&mut iter).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(name, "test");
            assert_eq!(params, vec![("x".into(), Type::I32)]);
            assert_eq!(ty, Type::Unit);
            assert_eq!(body.len(), 1);
            match &body[0] {
                Ast::Return(_, None) => {}
                _ => panic!("Wrong body, expected unit return"),
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_function_def() {
        let text = "fn test(x:i32) -> bool {return true;}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(Ast::RoutineDef(l, RoutineDef::Function, name, params, ty, body)) =
            function_def(&mut iter).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(name, "test");
            assert_eq!(params, vec![("x".into(), Type::I32)]);
            assert_eq!(ty, Type::Bool);
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
    fn parse_routine_call() {
        let text = "test(x, y)";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(Ast::RoutineCall(l, RoutineCall::Function, name, params)) =
            expression(&mut iter).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(name, vec!["test"].into());
            assert_eq!(
                params,
                vec![
                    Ast::Identifier(1, "x".into()),
                    Ast::Identifier(1, "y".into())
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
            .collect::<Result<_, _>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(Ast::RoutineCall(l, RoutineCall::Function, name, params)) =
            expression(&mut iter).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(name, vec!["self", "test"].into());
            assert_eq!(
                params,
                vec![
                    Ast::Identifier(1, "x".into()),
                    Ast::Identifier(1, "y".into())
                ]
            );
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_coroutine_def() {
        let text = "co test(x:i32) -> bool {return true;}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        if let Some(Ast::Module {
            meta, coroutines, ..
        }) = parse(tokens).unwrap()
        {
            assert_eq!(meta, 1);
            if let Ast::RoutineDef(_l, RoutineDef::Coroutine, name, params, ty, body) =
                &coroutines[0]
            {
                assert_eq!(name, "test");
                assert_eq!(params, &vec![("x".into(), Type::I32)]);
                assert_eq!(ty, &Type::Bool);
                assert_eq!(body.len(), 1);
                match &body[0] {
                    Ast::Return(_, Some(exp)) => {
                        assert_eq!(*exp.as_ref(), Ast::Boolean(1, true));
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
        let text = "let x:co i32 := init c(1, 2);";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Ast::Statement(_, stm) => match stm.as_ref() {
                Ast::Bind(_, id, false, p, exp) => {
                    assert_eq!(id, "x");
                    assert_eq!(*p, Type::Coroutine(Box::new(Type::I32)));
                    assert_eq!(
                        *exp,
                        Box::new(Ast::RoutineCall(
                            1,
                            RoutineCall::CoroutineInit,
                            vec!["c"].into(),
                            vec![Ast::Integer(1, 1), Ast::Integer(1, 2)]
                        ))
                    );
                }
                _ => panic!("Not a binding statement"),
            },
            _ => panic!("No body: {:?}", stm),
        }
    }

    #[test]
    fn parse_yield() {
        let text = "fn test(x:i32) -> bool {return yield cor;}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(Ast::RoutineDef(l, RoutineDef::Function, name, params, ty, body)) =
            function_def(&mut iter).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(name, "test");
            assert_eq!(params, vec![("x".into(), Type::I32)]);
            assert_eq!(ty, Type::Bool);
            assert_eq!(body.len(), 1);
            match &body[0] {
                Ast::Return(_, Some(exp)) => {
                    assert_eq!(
                        *exp.as_ref(),
                        Ast::Yield(1, Box::new(Ast::Identifier(1, "cor".into())))
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
            .collect::<Result<_, _>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let exp = expression(&mut stream).unwrap();
        if let Some(Ast::If(l, cond, if_arm, else_arm)) = exp {
            assert_eq!(l, 1);
            assert_eq!(*cond, Ast::Identifier(1, "x".into()));
            if let Ast::ExpressionBlock(_l, body) = *if_arm {
                assert_eq!(body[0], Ast::Integer(1, 5));
            } else {
                panic!("Expected Expression block");
            }

            if let Ast::ExpressionBlock(_l, body) = *else_arm {
                assert_eq!(body[0], Ast::Integer(1, 7));
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
            .collect::<Result<_, _>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let exp = expression(&mut stream).unwrap();
        if let Some(Ast::If(l, cond, if_arm, else_arm)) = exp {
            assert_eq!(l, 1);
            assert_eq!(*cond, Ast::Identifier(1, "x".into()));
            if let Ast::ExpressionBlock(_l, body) = *if_arm {
                assert_eq!(body[0], Ast::Integer(1, 5));
            } else {
                panic!("Expected Expression block");
            }

            if let Ast::If(_l, cond, if_arm, else_arm) = *else_arm {
                assert_eq!(
                    *cond,
                    Ast::BinaryOp(
                        1,
                        BinaryOperator::BAnd,
                        Box::new(Ast::Identifier(1, "y".into())),
                        Box::new(Ast::Identifier(1, "z".into()))
                    )
                );
                if let Ast::ExpressionBlock(_l, body) = *if_arm {
                    assert_eq!(body[0], Ast::Integer(1, 7));
                } else {
                    panic!("Expected Expression block");
                }

                if let Ast::ExpressionBlock(_l, body) = *else_arm {
                    assert_eq!(body[0], Ast::Integer(1, 8));
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
    fn parse_expression_block_oneline() {
        let text = "{5}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        if let Some(Ast::ExpressionBlock(l, body)) = expression_block(&mut stream).unwrap() {
            assert_eq!(l, 1);
            assert_eq!(body.len(), 1);
            assert_eq!(body[0], Ast::Integer(1, 5));
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_expression_block_bad() {
        for (text, msg) in [
            ("{5 10 51}", "L1: Expected }, but found literal 10"),
            ("{5; 10 51}", "L1: Expected }, but found literal 51"),
            ("{5; 10 let x:i32 := 5}", "L1: Expected }, but found let"),
            (
                "{let x: i32 := 10 5}",
                "L1: Expected ;, but found literal 5",
            ),
        ]
        .iter()
        {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_, _>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            assert_eq!(
                expression_block(&mut stream),
                Err((*msg).into()),
                "{:?}",
                text
            );
        }
    }

    #[test]
    fn parse_expression_block_multiline() {
        let text = "{let x:i32 := 5; f(x); x * x}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        if let Some(Ast::ExpressionBlock(l, body)) = expression_block(&mut stream).unwrap() {
            assert_eq!(l, 1);
            assert_eq!(body.len(), 3);
            match &body[0] {
                Ast::Statement(_, stm) => match stm.as_ref() {
                    Ast::Bind(_, id, false, p, exp) => {
                        assert_eq!(id, "x");
                        assert_eq!(*p, Type::I32);
                        assert_eq!(*exp, Box::new(PNode::Integer(1, 5)));
                    }
                    _ => panic!("Not a binding statement"),
                },
                _ => panic!("No body: {:?}", &body[0]),
            }
            match &body[1] {
                Ast::Statement(
                    _,
                    box Ast::RoutineCall(_, RoutineCall::Function, fn_name, params),
                ) => {
                    assert_eq!(*fn_name, vec!["f"].into());
                    assert_eq!(params[0], Ast::Identifier(1, "x".into()));
                }
                _ => panic!("No body: {:?}", &body[1]),
            }
            match &body[2] {
                Ast::BinaryOp(_, BinaryOperator::Mul, l, r) => {
                    assert_eq!(*l.as_ref(), Ast::Identifier(1, "x".into()));
                    assert_eq!(*r.as_ref(), Ast::Identifier(1, "x".into()));
                }
                _ => panic!("No body: {:?}", &body[2]),
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_struct_def() {
        for (text, expected) in vec![
            (
                "struct MyStruct {}",
                Ast::StructDef(1, "MyStruct".into(), vec![]),
            ),
            (
                "struct MyStruct {x: i32}",
                Ast::StructDef(1, "MyStruct".into(), vec![("x".into(), Type::I32)]),
            ),
            (
                "struct MyStruct {x: i32, y: bool}",
                Ast::StructDef(
                    1,
                    "MyStruct".into(),
                    vec![("x".into(), Type::I32), ("y".into(), Type::Bool)],
                ),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_, _>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            if let Some(Ast::Module { structs, .. }) = module(&mut stream).unwrap() {
                assert_eq!(structs[0], expected, "{:?}", text);
            }
        }
    }

    #[test]
    fn parse_struct_init() {
        for (text, expected) in vec![
            (
                "MyStruct{}",
                Ast::StructExpression(1, "MyStruct".into(), vec![]),
            ),
            (
                "MyStruct{x: 5}",
                Ast::StructExpression(1, "MyStruct".into(), vec![("x".into(), Ast::Integer(1, 5))]),
            ),
            (
                "MyStruct{x: 5, y: false}",
                Ast::StructExpression(
                    1,
                    "MyStruct".into(),
                    vec![
                        ("x".into(), Ast::Integer(1, 5)),
                        ("y".into(), Ast::Boolean(1, false)),
                    ],
                ),
            ),
            (
                "MyStruct{x: 5, y: MyStruct2{z:3}}",
                Ast::StructExpression(
                    1,
                    "MyStruct".into(),
                    vec![
                        ("x".into(), Ast::Integer(1, 5)),
                        (
                            "y".into(),
                            Ast::StructExpression(
                                1,
                                "MyStruct2".into(),
                                vec![("z".into(), Ast::Integer(1, 3))],
                            ),
                        ),
                    ],
                ),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_, _>>()
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
                .collect::<Result<_, _>>()
                .unwrap();
            let ast = parse(tokens).unwrap().unwrap();
            match ast {
                Ast::Module { functions, .. } => match &functions[0] {
                    Ast::RoutineDef(.., body) => match &body[0] {
                        Ast::Return(.., Some(rv)) => {
                            assert_eq!(*rv, Box::new(Ast::StringLiteral(1, expected.into())))
                        }
                        _ => assert!(false, "Not a return statement"),
                    },
                    _ => assert!(false, "Not a return statement"),
                },
                _ => assert!(false, "Not a routine, got {:?}", ast),
            }
        }
    }
}
