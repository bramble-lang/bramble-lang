use std::sync::atomic::AtomicBool;
use std::sync::atomic::{AtomicUsize, Ordering};

use stdext::function_name;

use crate::{
    diagnostics::config::TracingConfig,
    lexer::tokens::{Lex, Primitive, Token},
    syntax::{
        expression::{Expression, RoutineCall},
        module::Module,
        path::Path,
        routinedef::{RoutineDef, RoutineDefType},
        statement::{Bind, Mutate, Printbln, Printiln, Prints, Return, Statement, YieldReturn},
        structdef::StructDef,
        ty::Type,
    },
};
use braid_lang::result::Result;

// AST - a type(s) which is used to construct an AST representing the logic of the
// program
// Each type of node represents an expression and the only requirement is that at the
// end of computing an expression its result is in EAX
use super::pnode::{ParserResult, ParserCombinator, ParserInfo};
use super::tokenstream::TokenStream;


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

        if stream.index() == start_index {
            break;
        }
    }

    Ok(Some(parent_module))
}

fn struct_def(stream: &mut TokenStream) -> ParserResult<StructDef<u32>> {
    match stream.next_if(&Lex::Struct) {
        Some(token) => match stream.next_if_id() {
            Some((line, id)) => {
                stream.next_must_be(&Lex::LBrace)?;
                let fields = id_declaration_list(stream)?;
                stream.next_must_be(&Lex::RBrace)?;
                Ok(Some(StructDef::new(&id, line, fields)))
            }
            None => Err(format!("L{}: expected identifer after struct", token.l)),
        },
        None => Ok(None),
    }
}

fn function_def(stream: &mut TokenStream) -> ParserResult<RoutineDef<u32>> {
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

    stream.next_must_be(&Lex::LBrace)?;
    let mut stmts = block(stream)?;

    match return_stmt(stream)? {
        Some(ret) => stmts.push(Statement::Return(Box::new(ret))),
        None => {
            return Err(format!(
                "L{}: Function must end with a return statement, got {:?}",
                stmts.last().map_or(fn_line, |s| *s.get_metadata()),
                stream.peek(),
            ))
        }
    }
    stream.next_must_be(&Lex::RBrace)?;

    Ok(Some(RoutineDef {
        meta: fn_line,
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
                stmts.last().map_or(co_line, |s| *s.get_metadata()),
            ))
        }
    }
    stream.next_must_be(&Lex::RBrace)?;

    Ok(Some(RoutineDef {
        meta: co_line,
        def: RoutineDefType::Coroutine,
        name: co_name,
        params,
        ty: co_type,
        body: stmts,
    }))
}

fn block(stream: &mut TokenStream) -> Result<Vec<Statement<ParserInfo>>> {
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

fn statement_or_yield_return(stream: &mut TokenStream) -> ParserResult<Statement<ParserInfo>> {
    let stm = match statement(stream)? {
        Some(n) => Some(n),
        None => match yield_return_stmt(stream)? {
            Some(yr) => Some(yr),
            None => None,
        },
    };

    Ok(stm)
}

fn statement(stream: &mut TokenStream) -> ParserResult<Statement<ParserInfo>> {
    let start_index = stream.index();
    let must_have_semicolon = stream.test_if_one_of(vec![Lex::Let, Lex::Mut]);
    let stm = match let_bind(stream)? {
        Some(bind) => Some(Statement::Bind(Box::new(bind))),
        None => match mutate(stream)? {
            Some(mutate) => Some(Statement::Mutate(Box::new(mutate))),
            None => match println_stmt(stream)? {
                Some(p) => Some(p),
                None => expression(stream)?
                    .map(|s| Statement::from_ast(s))
                    .flatten(),
            },
        },
    };

    match stm {
        Some(stm) => match stream.next_if(&Lex::Semicolon) {
            Some(Token { s: _, .. }) => Ok(Some(stm)),
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

fn expression_block(stream: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
    match stream.next_if(&Lex::LBrace) {
        Some(token) => {
            let stmts = block(stream)?;

            let final_exp = expression(stream)?.map(|e| Box::new(e));

            stream.next_must_be(&Lex::RBrace)?;
            Ok(Some(Expression::ExpressionBlock(token.l, stmts, final_exp)))
        }
        None => Ok(None),
    }
}

fn expression(stream: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
    trace!(stream);
    logical_or(stream)
}

fn logical_or(stream: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
    trace!(stream);
    binary_op(stream, &vec![Lex::BOr], logical_and)
}

fn logical_and(stream: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
    trace!(stream);
    binary_op(stream, &vec![Lex::BAnd], comparison)
}

fn comparison(stream: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
    trace!(stream);
    binary_op(
        stream,
        &vec![Lex::Eq, Lex::NEq, Lex::Ls, Lex::LsEq, Lex::Gr, Lex::GrEq],
        sum,
    )
}

fn sum(stream: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
    trace!(stream);
    binary_op(stream, &vec![Lex::Add, Lex::Minus], term)
}

fn term(stream: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
    trace!(stream);
    binary_op(stream, &vec![Lex::Mul, Lex::Div], negate)
}

fn binary_op(
    stream: &mut TokenStream,
    test: &Vec<Lex>,
    left_pattern: fn(&mut TokenStream) -> ParserResult<Expression<ParserInfo>>,
) -> ParserResult<Expression<ParserInfo>> {
    trace!(stream);
    match left_pattern(stream)? {
        Some(left) => match stream.next_if_one_of(test.clone()) {
            Some(op) => {
                let right = binary_op(stream, test, left_pattern)?
                    .ok_or(format!("L{}: expected expression after {}", op.l, op.s))?;
                Expression::binary_op(op.l, &op.s, Box::new(left), Box::new(right))
            }
            None => Ok(Some(left)),
        },
        None => Ok(None),
    }
}

fn negate(stream: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
    trace!(stream);
    match stream.next_if_one_of(vec![Lex::Minus, Lex::Not]) {
        Some(op) => {
            let factor =
                negate(stream)?.ok_or(&format!("L{}: expected term after {}", op.l, op.s))?;
            Expression::unary_op(op.l, &op.s, Box::new(factor))
        }
        None => member_access(stream),
    }
}

fn member_access(stream: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
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
                ma = Expression::MemberAccess(line, Box::new(ma), member);
            }
            Ok(Some(ma))
        }
        None => Ok(None),
    }
}

fn factor(stream: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
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

fn if_expression(stream: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
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
            Some(Expression::If(
                token.l,
                Box::new(cond),
                Box::new(true_arm),
                Box::new(false_arm),
            ))
        }
        _ => None,
    })
}

fn fn_def_params(stream: &mut TokenStream) -> Result<Vec<(String, Type)>> {
    trace!(stream);
    stream.next_must_be(&Lex::LParen)?;
    let params = id_declaration_list(stream)?;
    stream.next_must_be(&Lex::RParen)?;

    Ok(params)
}

fn id_declaration_list(stream: &mut TokenStream) -> Result<Vec<(String, Type)>> {
    trace!(stream);
    let mut decls = vec![];

    while let Some(token) = id_declaration(stream)? {
        match token {
            Expression::IdentifierDeclare(_line, id, ty) => {
                decls.push((id, ty));
                stream.next_if(&Lex::Comma);
            }
            _ => panic!("CRITICAL: IdDeclaration not returned by id_declaration"),
        }
    }

    Ok(decls)
}

/// LPAREN [EXPRESSION [, EXPRESSION]*] RPAREN
fn routine_call_params(stream: &mut TokenStream) -> ParserResult<Vec<Expression<ParserInfo>>> {
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

fn struct_init_params(stream: &mut TokenStream) -> ParserResult<Vec<(String, Expression<ParserInfo>)>> {
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

fn return_stmt(stream: &mut TokenStream) -> ParserResult<Return<ParserInfo>> {
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

fn co_yield(stream: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
    trace!(stream);
    match stream.next_if(&Lex::Yield) {
        Some(token) => {
            let line = token.l;
            match expression(stream)? {
                Some(coroutine) => Expression::new_yield(*coroutine.get_metadata(), Box::new(coroutine)),
                None => Err(format!("L{}: expected an identifier after yield", line)),
            }
        }
        None => Ok(None),
    }
}

fn println_stmt(stream: &mut TokenStream) -> ParserResult<Statement<ParserInfo>> {
    trace!(stream);
    let syntax = match stream.next_if_one_of(vec![Lex::Printiln, Lex::Prints, Lex::Printbln]) {
        Some(print) => {
            let exp = expression(stream)?
                .ok_or(format!("L{}: Expected expression after println", print.l))?;
            match print.s {
                Lex::Printiln => Some(Statement::Printiln(Box::new(Printiln::new(print.l, exp)))),
                Lex::Prints => Some(Statement::Prints(Box::new(Prints::new(print.l, exp)))),
                Lex::Printbln => Some(Statement::Printbln(Box::new(Printbln::new(print.l, exp)))),
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

fn function_call_or_variable(stream: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
    trace!(stream);
    let s: Option<Expression<u32>> = match path(stream)? {
        Some((line, path)) => match routine_call_params(stream)? {
            Some(params) => Some(Expression::RoutineCall(
                line,
                RoutineCall::Function,
                path,
                params.clone(),
            )),
            None => match struct_init_params(stream)? {
                Some(params) => Some(Expression::StructExpression(line, path, params.clone())),
                None => {
                    if path.len() > 1 {
                        Some(Expression::Path(line, path))
                    } else {
                        Some(Expression::Identifier(line, path.last().unwrap().clone()))
                    }
                }
            },
        },
        _ => None,
    };

    Ok(s)
}

// TODO: I think what I want ot do is pull the ID/Path parsing up in to `function_call_or_variable` and then
// determine if it's a function, struct expressoin, or variable by if there is a LParen or LBrace after the path.
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

fn struct_expression(stream: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
    trace!(stream);
    if stream.test_ifn(vec![Lex::Identifier("".into()), Lex::LBrace]) {
        let (line, struct_name) = path(stream)?.ok_or("CRITICAL: failed to get identifier")?;
        let fields = struct_init_params(stream)?.ok_or(format!(
            "L{}: Expected valid field assignments in struct expression",
            line
        ))?;
        Ok(Some(Expression::StructExpression(
            line,
            struct_name,
            fields,
        )))
    } else {
        Ok(None)
    }
}

fn path(stream: &mut TokenStream) -> ParserResult<(u32, Path)> {
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
                Primitive::I32 => Some(Type::I32),
                Primitive::Bool => Some(Type::Bool),
                Primitive::StringLiteral => Some(Type::StringLiteral),
            };
            stream.next();
            ty
        }
        _ => match path(stream)? {
            Some((_, path)) => Some(Type::Custom(path)),
            _ => None,
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

fn id_declaration(stream: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
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

fn constant(stream: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
    trace!(stream);
    number(stream)
        .por(boolean, stream)
        .por(string_literal, stream)
}

fn number(stream: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
    trace!(stream);
    match stream.next_if(&Lex::Integer(0)) {
        Some(Token {
            l,
            s: Lex::Integer(i),
        }) => Ok(Some(Expression::Integer(l, i))),
        _ => Ok(None),
    }
}

fn boolean(stream: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
    trace!(stream);
    match stream.next_if(&Lex::Bool(true)) {
        Some(Token { l, s: Lex::Bool(b) }) => Ok(Some(Expression::Boolean(l, b))),
        _ => Ok(None),
    }
}

fn string_literal(stream: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
    trace!(stream);
    match stream.next_if(&Lex::StringLiteral("".into())) {
        Some(Token {
            l,
            s: Lex::StringLiteral(s),
        }) => Ok(Some(Expression::StringLiteral(l, s))),
        _ => Ok(None),
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::{
        lexer::lexer::Lexer,
        syntax::{
            expression::{BinaryOperator, UnaryOperator},
            module::Item,
        },
    };

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
                assert_eq!(*left, Expression::Integer(1, 2));
                assert_eq!(*right, Expression::Integer(1, 2));
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
                    assert_eq!(**ll, Expression::Integer(1, 2));
                    assert_eq!(**lr, Expression::Integer(1, 4));
                }
                _ => panic!("Expected Add syntax"),
            }
            assert_eq!(*right, Expression::Integer(1, 3));
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
                        Err(msg) => assert!(false, msg),
                    }
                }
                Ok(Some(Expression::Identifier(l, id))) => {
                    assert_eq!(l, 1);
                    match expected {
                        Ok(expected) => {
                            assert_eq!(expected.len(), 1);
                            assert_eq!(id, expected[0]);
                        }
                        Err(msg) => assert!(false, msg),
                    }
                }
                Ok(Some(n)) => panic!("{} resulted in {:?}, expected {:?}", text, n, expected),
                Ok(None) => panic!("No node returned for {}, expected {:?}", text, expected),
                Err(msg) => match expected {
                    Ok(_) => assert!(false, msg),
                    Err(expected) => assert_eq!(expected, msg),
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
                .collect::<Result<_>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            match member_access(&mut stream) {
                Ok(Some(Expression::MemberAccess(l, left, right))) => {
                    assert_eq!(l, 1);
                    assert_eq!(
                        *left,
                        Expression::Identifier(1, "thing".into()),
                        "Input: {}",
                        text,
                    );
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
        let text = "let x:i32 := 5;";
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
                assert_eq!(b.get_type(), Type::I32);
                assert_eq!(b.is_mutable(), false);
                assert_eq!(*b.get_rhs(), Expression::Integer(1, 5));
            }
            _ => panic!("Not a binding statement"),
        }
    }

    #[test]
    fn parse_mut_bind() {
        let text = "let mut x:i32 := 5;";
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
                assert_eq!(b.get_type(), Type::I32);
                assert_eq!(b.is_mutable(), true);
                assert_eq!(*b.get_rhs(), Expression::Integer(1, 5));
            }
            _ => panic!("Not a binding statement"),
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
                assert_eq!(*m.get_rhs(), Expression::Integer(1, 5));
            }
            _ => panic!("Not a binding statement"),
        }
    }

    #[test]
    fn parse_printiln() {
        let text = "printiln 5;";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Statement::Printiln(box p) => {
                assert_eq!(*p.get_value(), Expression::Integer(1, 5));
            }
            _ => panic!("Not a binding statement"),
        }
    }

    #[test]
    fn parse_printbln() {
        let text = "printbln true;";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Statement::Printbln(box p) => {
                assert_eq!(*p.get_value(), Expression::Boolean(1, true));
            }
            _ => panic!("Not a binding statement"),
        }
    }

    #[test]
    fn parse_prints() {
        let text = "prints \"hello\";";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Statement::Prints(box p) => {
                assert_eq!(*p.get_value(), Expression::StringLiteral(1, "hello".into()));
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
            assert_eq!(*m.get_metadata(), 1);
            assert_eq!(m.get_name(), "test_mod");
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
            .collect::<Result<_>>()
            .unwrap();

        if let Some(m) = parse(tokens).unwrap() {
            assert_eq!(*m.get_metadata(), 1);
            assert_eq!(m.get_name(), "root");

            assert_eq!(m.get_modules().len(), 1);
            assert_eq!(m.get_functions().len(), 0);
            assert_eq!(m.get_coroutines().len(), 0);
            assert_eq!(m.get_structs().len(), 0);

            let m = &m.get_modules()[0];
            assert_eq!(*m.get_metadata(), 1);
            assert_eq!(m.get_name(), "test_fn_mod");

            assert_eq!(m.get_modules().len(), 0);
            assert_eq!(m.get_functions().len(), 1);
            assert_eq!(m.get_coroutines().len(), 0);
            assert_eq!(m.get_structs().len(), 0);
            if let Item::Routine(RoutineDef {
                meta,
                def: RoutineDefType::Function,
                name,
                params,
                ty,
                body,
            }) = &m.get_functions()[0]
            {
                assert_eq!(*meta, 1);
                assert_eq!(name, "test");
                assert_eq!(params, &vec![("x".into(), Type::I32)]);
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
        let text = "mod test_co_mod { co test(x:i32) {return;} }";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(m) = module(&mut iter).unwrap() {
            assert_eq!(*m.get_metadata(), 1);
            assert_eq!(m.get_name(), "test_co_mod");

            assert_eq!(m.get_modules().len(), 0);
            assert_eq!(m.get_functions().len(), 0);
            assert_eq!(m.get_coroutines().len(), 1);
            assert_eq!(m.get_structs().len(), 0);

            if let Some(Item::Routine(RoutineDef {
                meta,
                def: RoutineDefType::Coroutine,
                name,
                params,
                ty,
                body,
            })) = m.get_item("test")
            {
                assert_eq!(*meta, 1);
                assert_eq!(name, "test");
                assert_eq!(params, &vec![("x".into(), Type::I32)]);
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
        let text = "mod test_struct_mod { struct my_struct{x: i32} }";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(m) = module(&mut iter).unwrap() {
            assert_eq!(*m.get_metadata(), 1);
            assert_eq!(m.get_name(), "test_struct_mod");

            assert_eq!(m.get_modules().len(), 0);
            assert_eq!(m.get_functions().len(), 0);
            assert_eq!(m.get_coroutines().len(), 0);
            assert_eq!(m.get_structs().len(), 1);

            if let Some(Item::Struct(sd)) = m.get_item("my_struct") {
                assert_eq!(*sd.get_metadata(), 1);
                assert_eq!(sd.get_name(), "my_struct");
                assert_eq!(sd.get_fields(), &vec![("x".into(), Type::I32)]);
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
            .collect::<Result<_>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(RoutineDef {
            meta: l,
            def: RoutineDefType::Function,
            name,
            params,
            ty,
            body,
        }) = function_def(&mut iter).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(name, "test");
            assert_eq!(params, vec![("x".into(), Type::I32)]);
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
        let text = "fn test(x:i32) -> bool {return true;}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(RoutineDef {
            meta: l,
            def: RoutineDefType::Function,
            name,
            params,
            ty,
            body,
        }) = function_def(&mut iter).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(name, "test");
            assert_eq!(params, vec![("x".into(), Type::I32)]);
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
        let text = "co test(x:i32) -> bool {return true;}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        if let Some(m) = parse(tokens).unwrap() {
            assert_eq!(*m.get_metadata(), 1);
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
                assert_eq!(params, &vec![("x".into(), Type::I32)]);
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
        let text = "let x:co i32 := init c(1, 2);";
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
                assert_eq!(b.get_type(), Type::Coroutine(Box::new(Type::I32)));
                assert_eq!(
                    *b.get_rhs(),
                    Expression::RoutineCall(
                        1,
                        RoutineCall::CoroutineInit,
                        vec!["c"].into(),
                        vec![Expression::Integer(1, 1), Expression::Integer(1, 2)]
                    )
                );
            }
            _ => panic!("Not a binding statement"),
        }
    }

    #[test]
    fn parse_coroutine_path_init() {
        let text = "let x:co i32 := init a::b::c(1, 2);";
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
                assert_eq!(b.get_type(), Type::Coroutine(Box::new(Type::I32)));
                assert_eq!(
                    *b.get_rhs(),
                    Expression::RoutineCall(
                        1,
                        RoutineCall::CoroutineInit,
                        vec!["a", "b", "c"].into(),
                        vec![Expression::Integer(1, 1), Expression::Integer(1, 2)]
                    )
                );
            }
            _ => panic!("Not a binding statement"),
        }
    }

    #[test]
    fn parse_yield() {
        let text = "fn test(x:i32) -> bool {return yield cor;}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(RoutineDef {
            meta: l,
            def: RoutineDefType::Function,
            name,
            params,
            ty,
            body,
        }) = function_def(&mut iter).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(name, "test");
            assert_eq!(params, vec![("x".into(), Type::I32)]);
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
        if let Some(Expression::If(l, cond, if_arm, else_arm)) = exp {
            assert_eq!(l, 1);
            assert_eq!(*cond, Expression::Identifier(1, "x".into()));
            if let Expression::ExpressionBlock(_l, _body, Some(final_exp)) = *if_arm {
                assert_eq!(*final_exp, Expression::Integer(1, 5));
            } else {
                panic!("Expected Expression block");
            }

            if let Expression::ExpressionBlock(_l, _body, Some(final_exp)) = *else_arm {
                assert_eq!(*final_exp, Expression::Integer(1, 7));
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
        if let Some(Expression::If(l, cond, if_arm, else_arm)) = exp {
            assert_eq!(l, 1);
            assert_eq!(*cond, Expression::Identifier(1, "x".into()));
            if let Expression::ExpressionBlock(_l, _body, Some(final_exp)) = *if_arm {
                assert_eq!(*final_exp, Expression::Integer(1, 5));
            } else {
                panic!("Expected Expression block");
            }

            if let Expression::If(_l, cond, if_arm, else_arm) = *else_arm {
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
                    assert_eq!(*final_exp, Expression::Integer(1, 7));
                } else {
                    panic!("Expected Expression block");
                }

                if let Expression::ExpressionBlock(_l, _body, Some(final_exp)) = *else_arm {
                    assert_eq!(*final_exp, Expression::Integer(1, 8));
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
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        if let Some(Expression::ExpressionBlock(l, body, Some(final_exp))) =
            expression_block(&mut stream).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(body.len(), 0);
            assert_eq!(*final_exp, Expression::Integer(1, 5));
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
                .collect::<Result<_>>()
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
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        if let Some(Expression::ExpressionBlock(l, body, Some(final_exp))) =
            expression_block(&mut stream).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(body.len(), 2);
            match &body[0] {
                Statement::Bind(box b) => {
                    assert_eq!(b.get_id(), "x");
                    assert_eq!(b.get_type(), Type::I32);
                    assert_eq!(*b.get_rhs(), Expression::Integer(1, 5));
                }
                _ => panic!("Not a binding statement"),
            }
            match &body[1] {
                Statement::Expression(box Expression::RoutineCall(
                    _,
                    RoutineCall::Function,
                    fn_name,
                    params,
                )) => {
                    assert_eq!(*fn_name, vec!["f"].into());
                    assert_eq!(params[0], Expression::Identifier(1, "x".into()));
                }
                _ => panic!("No body: {:?}", &body[1]),
            }
            match final_exp {
                box Expression::BinaryOp(_, BinaryOperator::Mul, l, r) => {
                    assert_eq!(*l.as_ref(), Expression::Identifier(1, "x".into()));
                    assert_eq!(*r.as_ref(), Expression::Identifier(1, "x".into()));
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
            ("struct MyStruct {}", StructDef::new("MyStruct", 1, vec![])),
            (
                "struct MyStruct {x: i32}",
                StructDef::new("MyStruct", 1, vec![("x".into(), Type::I32)]),
            ),
            (
                "struct MyStruct {x: i32, y: bool}",
                StructDef::new(
                    "MyStruct",
                    1,
                    vec![("x".into(), Type::I32), ("y".into(), Type::Bool)],
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
                    vec![("x".into(), Expression::Integer(1, 5))],
                ),
            ),
            (
                "MyStruct{x: 5, y: false}",
                Expression::StructExpression(
                    1,
                    vec!["MyStruct"].into(),
                    vec![
                        ("x".into(), Expression::Integer(1, 5)),
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
                        ("x".into(), Expression::Integer(1, 5)),
                        (
                            "y".into(),
                            Expression::StructExpression(
                                1,
                                vec!["MyStruct2"].into(),
                                vec![("z".into(), Expression::Integer(1, 3))],
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
