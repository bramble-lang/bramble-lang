use super::parser::{ENABLE_TRACING, TRACE_END, TRACE_START};
use std::sync::atomic::Ordering;
use stdext::function_name;

use super::{
    parser::{block, path, routine_call_params, ParserContext, ParserResult},
    tokenstream::TokenStream,
};
use crate::{
    compiler::{
        ast::*,
        lexer::tokens::{Lex, Token},
    },
    trace,
};

impl ParserCombinator<ParserResult<Expression<ParserContext>>>
    for ParserResult<Expression<ParserContext>>
{
    fn por(
        &self,
        f: fn(&mut TokenStream) -> ParserResult<Expression<ParserContext>>,
        ts: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        match self {
            Ok(Some(s)) => Ok(Some(s.clone())),
            Ok(None) => f(ts),
            Err(e) => Err(e.clone()),
        }
    }

    fn pif_then(
        &self,
        cond: Vec<Lex>,
        then: fn(
            Expression<ParserContext>,
            Token,
            &mut TokenStream,
        ) -> ParserResult<Expression<ParserContext>>,
        ts: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        match self {
            Ok(Some(s)) => match ts.next_if_one_of(cond) {
                Some(result) => then(s.clone(), result, ts),
                None => Ok(Some(s.clone())),
            },
            Ok(None) => Ok(None),
            Err(e) => Err(e.clone()),
        }
    }
}

pub trait ParserCombinator<R> {
    fn por(&self, f: fn(&mut TokenStream) -> R, ts: &mut TokenStream) -> R;
    fn pif_then(
        &self,
        cond: Vec<Lex>,
        f: fn(Expression<ParserContext>, Token, &mut TokenStream) -> R,
        ts: &mut TokenStream,
    ) -> R;
}

impl Expression<ParserContext> {
    pub fn new_yield(
        line: u32,
        coroutine_value: Box<Self>,
    ) -> ParserResult<Expression<ParserContext>> {
        let i = line; //ParserInfo{l: line};
        Ok(Some(Expression::Yield(i, coroutine_value)))
    }

    pub fn unary_op(
        line: u32,
        op: &Lex,
        operand: Box<Self>,
    ) -> ParserResult<Expression<ParserContext>> {
        match op {
            Lex::Minus => Ok(Some(Expression::UnaryOp(
                line,
                UnaryOperator::Negate,
                operand,
            ))),
            Lex::Not => Ok(Some(Expression::UnaryOp(line, UnaryOperator::Not, operand))),
            _ => Err(format!("L{}: {} is not a unary operator", line, op)),
        }
    }

    pub fn binary_op(
        line: u32,
        op: &Lex,
        left: Box<Self>,
        right: Box<Self>,
    ) -> ParserResult<Expression<ParserContext>> {
        let i = line; //ParserInfo{l: line};
        match op {
            Lex::Eq => Ok(Some(Expression::BinaryOp(
                i,
                BinaryOperator::Eq,
                left,
                right,
            ))),
            Lex::NEq => Ok(Some(Expression::BinaryOp(
                i,
                BinaryOperator::NEq,
                left,
                right,
            ))),
            Lex::Ls => Ok(Some(Expression::BinaryOp(
                i,
                BinaryOperator::Ls,
                left,
                right,
            ))),
            Lex::LsEq => Ok(Some(Expression::BinaryOp(
                i,
                BinaryOperator::LsEq,
                left,
                right,
            ))),
            Lex::Gr => Ok(Some(Expression::BinaryOp(
                i,
                BinaryOperator::Gr,
                left,
                right,
            ))),
            Lex::GrEq => Ok(Some(Expression::BinaryOp(
                i,
                BinaryOperator::GrEq,
                left,
                right,
            ))),
            Lex::BAnd => Ok(Some(Expression::BinaryOp(
                i,
                BinaryOperator::BAnd,
                left,
                right,
            ))),
            Lex::BOr => Ok(Some(Expression::BinaryOp(
                i,
                BinaryOperator::BOr,
                left,
                right,
            ))),
            Lex::Add => Ok(Some(Expression::BinaryOp(
                i,
                BinaryOperator::Add,
                left,
                right,
            ))),
            Lex::Minus => Ok(Some(Expression::BinaryOp(
                i,
                BinaryOperator::Sub,
                left,
                right,
            ))),
            Lex::Mul => Ok(Some(Expression::BinaryOp(
                i,
                BinaryOperator::Mul,
                left,
                right,
            ))),
            Lex::Div => Ok(Some(Expression::BinaryOp(
                i,
                BinaryOperator::Div,
                left,
                right,
            ))),
            _ => Err(format!("L{}: {} is not a binary operator", line, op)),
        }
    }
}

fn expression_block(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
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

pub(super) fn expression(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    logical_or(stream)
}

fn logical_or(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    binary_op(stream, &vec![Lex::BOr], logical_and)
}

fn logical_and(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    binary_op(stream, &vec![Lex::BAnd], comparison)
}

fn comparison(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    binary_op(
        stream,
        &vec![Lex::Eq, Lex::NEq, Lex::Ls, Lex::LsEq, Lex::Gr, Lex::GrEq],
        sum,
    )
}

fn sum(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    binary_op(stream, &vec![Lex::Add, Lex::Minus], term)
}

fn term(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    binary_op(stream, &vec![Lex::Mul, Lex::Div], negate)
}

pub fn binary_op(
    stream: &mut TokenStream,
    test: &Vec<Lex>,
    left_pattern: fn(&mut TokenStream) -> ParserResult<Expression<ParserContext>>,
) -> ParserResult<Expression<ParserContext>> {
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

fn negate(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
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

fn member_access(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    match factor(stream)? {
        Some(f) => {
            let mut ma = f;
            while let Some(token) = stream.next_if_one_of(vec![Lex::MemberAccess, Lex::LBracket]) {
                ma = match token.s {
                    Lex::MemberAccess => stream
                        .next_if_id()
                        .map(|(_, member)| Expression::MemberAccess(token.l, Box::new(ma), member))
                        .ok_or(format!(
                            "L{}: expect field name after member access '.'",
                            token.l
                        ))?,
                    Lex::LBracket => expression(stream)?
                        .ok_or(format!(
                            "L{}: Index operator must contain valid expression",
                            token.l
                        ))
                        .map(|index| {
                            stream
                                .next_must_be(&Lex::RBracket)
                                .map(|_| Expression::ArrayAt {
                                    context: token.l,
                                    array: box ma,
                                    index: box index,
                                })
                        })??,
                    _ => {
                        return Err(format!(
                            "L{}: Expected [ or . but found {}",
                            token.l, token.s
                        ))
                    }
                };
            }

            Ok(Some(ma))
        }
        None => Ok(None),
    }
}

fn factor(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
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
            .por(while_expression, stream)
            .por(expression_block, stream)
            .por(function_call_or_variable, stream)
            .por(co_yield, stream)
            .por(constant, stream)
            .por(array_value, stream),
    }
}

fn if_expression(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    Ok(match stream.next_if(&Lex::If) {
        Some(token) => {
            stream.next_must_be(&Lex::LParen)?;
            let cond = expression(stream)?.ok_or(format!(
                "L{}: Expected conditional expression after if",
                token.l
            ))?;
            stream.next_must_be(&Lex::RParen)?;

            let if_arm = expression_block(stream)?.ok_or(format!(
                "L{}: Expression in true arm of if expression",
                token.l
            ))?;

            // check for `else if`
            let else_arm = match stream.next_if(&Lex::Else) {
                Some(_) => match stream.peek() {
                    Some(Token { l, s: Lex::If }) => {
                        let l = *l;
                        Some(
                            if_expression(stream)?
                                .ok_or(format!("L{}: Expected if expression after else if", l))?,
                        )
                    }
                    _ => {
                        let false_arm = expression_block(stream)?.ok_or(&format!(
                            "L{}: Expression in false arm of if expression",
                            token.l
                        ))?;
                        Some(false_arm)
                    }
                },
                None => None,
            };

            Some(Expression::If {
                context: token.l,
                cond: Box::new(cond),
                if_arm: Box::new(if_arm),
                else_arm: else_arm.map(|f| box f),
            })
        }
        _ => None,
    })
}

fn while_expression(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    Ok(match stream.next_if(&Lex::While) {
        Some(token) => {
            stream.next_must_be(&Lex::LParen)?;
            let cond = expression(stream)?.ok_or(format!(
                "L{}: Expected conditional expression after while",
                token.l
            ))?;
            stream.next_must_be(&Lex::RParen)?;

            let body =
                expression_block(stream)?.ok_or(format!("L{}: Expression in body", token.l))?;

            Some(Expression::While {
                context: token.l,
                cond: Box::new(cond),
                body: Box::new(body),
            })
        }
        _ => None,
    })
}

fn function_call_or_variable(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
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

fn co_yield(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    match stream.next_if(&Lex::Yield) {
        Some(token) => {
            let line = token.l;
            match expression(stream)? {
                Some(coroutine) => {
                    Expression::new_yield(*coroutine.get_context(), Box::new(coroutine))
                }
                None => Err(format!("L{}: expected an identifier after yield", line)),
            }
        }
        None => Ok(None),
    }
}

fn struct_init_params(
    stream: &mut TokenStream,
) -> ParserResult<Vec<(String, Expression<ParserContext>)>> {
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

fn array_value(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    match stream.next_if(&Lex::LBracket) {
        Some(token) => {
            let mut elements = vec![];
            // loop through comma separated list of expressions
            while let Some(element) = expression(stream)? {
                elements.push(element);
                match stream.next_if(&Lex::Comma) {
                    Some(_) => {}
                    None => break,
                };
            }
            stream.next_must_be(&Lex::RBracket)?;

            let len = elements.len();
            Ok(Some(Expression::ArrayExpression(token.l, elements, len)))
        }
        None => Ok(None),
    }
}

fn constant(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    number(stream)
        .por(boolean, stream)
        .por(string_literal, stream)
}

fn number(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    match stream.next_if_one_of(vec![
        Lex::U8(0),
        Lex::U16(0),
        Lex::U32(0),
        Lex::U64(0),
        Lex::I8(0),
        Lex::I16(0),
        Lex::I32(0),
        Lex::I64(0),
    ]) {
        Some(Token { l, s: Lex::U8(i) }) => Ok(Some(Expression::U8(l, i))),
        Some(Token { l, s: Lex::U16(i) }) => Ok(Some(Expression::U16(l, i))),
        Some(Token { l, s: Lex::U32(i) }) => Ok(Some(Expression::U32(l, i))),
        Some(Token { l, s: Lex::U64(i) }) => Ok(Some(Expression::U64(l, i))),
        Some(Token { l, s: Lex::I8(i) }) => Ok(Some(Expression::I8(l, i))),
        Some(Token { l, s: Lex::I16(i) }) => Ok(Some(Expression::I16(l, i))),
        Some(Token { l, s: Lex::I32(i) }) => Ok(Some(Expression::I32(l, i))),
        Some(Token { l, s: Lex::I64(i) }) => Ok(Some(Expression::I64(l, i))),
        Some(t) => panic!("Unexpected token: {:?}", t),
        None => Ok(None),
    }
}

fn boolean(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    match stream.next_if(&Lex::Bool(true)) {
        Some(Token { l, s: Lex::Bool(b) }) => Ok(Some(Expression::Boolean(l, b))),
        _ => Ok(None),
    }
}

fn string_literal(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
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
mod test {
    use super::*;
    use crate::compiler::{
        ast::{Statement, Type},
        Lexer,
    };
    use braid_lang::result::Result;

    #[test]
    fn parse_number() {
        for (text, expected) in vec![
            ("64u8", Expression::U8(1, 64)),
            ("64u16", Expression::U16(1, 64)),
            ("64u32", Expression::U32(1, 64)),
            ("64u64", Expression::U64(1, 64)),
            ("5i8", Expression::I8(1, 5)),
            ("5i16", Expression::I16(1, 5)),
            ("5i32", Expression::I32(1, 5)),
            ("64i64", Expression::I64(1, 64)),
            ("64", Expression::I64(1, 64)),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            match number(&mut stream) {
                Ok(Some(e)) => assert_eq!(e, expected),
                Ok(t) => panic!("Expected an {:?} but got {:?}", expected, t),
                Err(err) => panic!("Expected {:?}, but got {}", expected, err),
            }
        }
    }

    #[test]
    fn parse_array_value() {
        for (text, expected) in vec![
            (
                "[1]",
                Expression::ArrayExpression(1, vec![Expression::I64(1, 1)], 1),
            ),
            (
                "[1u8]",
                Expression::ArrayExpression(1, vec![Expression::U8(1, 1)], 1),
            ),
            (
                "[1,]",
                Expression::ArrayExpression(1, vec![Expression::I64(1, 1)], 1),
            ),
            (
                "[1, 2, 3]",
                Expression::ArrayExpression(
                    1,
                    vec![
                        Expression::I64(1, 1),
                        Expression::I64(1, 2),
                        Expression::I64(1, 3),
                    ],
                    3,
                ),
            ),
            (
                "[1, 2i8, 3]", // This is legal at the parser level (it is illegal semantically)
                Expression::ArrayExpression(
                    1,
                    vec![
                        Expression::I64(1, 1),
                        Expression::I8(1, 2),
                        Expression::I64(1, 3),
                    ],
                    3,
                ),
            ),
            (
                "[[1,],]",
                Expression::ArrayExpression(
                    1,
                    vec![Expression::ArrayExpression(
                        1,
                        vec![Expression::I64(1, 1)],
                        1,
                    )],
                    1,
                ),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            match array_value(&mut stream) {
                Ok(Some(e)) => assert_eq!(e, expected),
                Ok(t) => panic!("Expected an {:?} but got {:?}", expected, t),
                Err(err) => panic!("Expected {:?}, but got {}", expected, err),
            }
        }
    }

    #[test]
    fn parse_array_fails() {
        for (text, msg) in [
            ("[5", "L0: Expected ], but found EOF"),
            ("[5 6]", "L1: Expected ], but found i64 literal 6"),
        ]
        .iter()
        {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            assert_eq!(array_value(&mut stream), Err((*msg).into()), "{:?}", text);
        }
    }

    #[test]
    fn parse_array_at_index() {
        for (text, expected) in vec![
            //
            (
                "a[1]",
                Expression::ArrayAt {
                    context: 1,
                    array: box Expression::Identifier(1, "a".into()),
                    index: box Expression::I64(1, 1),
                },
            ),
            (
                "(a)[1]",
                Expression::ArrayAt {
                    context: 1,
                    array: box Expression::Identifier(1, "a".into()),
                    index: box Expression::I64(1, 1),
                },
            ),
            (
                "a.b[1]",
                Expression::ArrayAt {
                    context: 1,
                    array: box Expression::MemberAccess(
                        1,
                        box Expression::Identifier(1, "a".into()),
                        "b".into(),
                    ),
                    index: box Expression::I64(1, 1),
                },
            ),
            (
                "a[1].b",
                Expression::MemberAccess(
                    1,
                    box Expression::ArrayAt {
                        context: 1,
                        array: box Expression::Identifier(1, "a".into()),
                        index: box Expression::I64(1, 1),
                    },
                    "b".into(),
                ),
            ),
            (
                "a[0].b[1]",
                Expression::ArrayAt {
                    context: 1,
                    array: box Expression::MemberAccess(
                        1,
                        box Expression::ArrayAt {
                            context: 1,
                            array: box Expression::Identifier(1, "a".into()),
                            index: box Expression::I64(1, 0),
                        },
                        "b".into(),
                    ),
                    index: box Expression::I64(1, 1),
                },
            ),
            (
                "a[1][2]",
                Expression::ArrayAt {
                    context: 1,
                    array: box Expression::ArrayAt {
                        context: 1,
                        array: box Expression::Identifier(1, "a".into()),
                        index: box Expression::I64(1, 1),
                    },
                    index: box Expression::I64(1, 2),
                },
            ),
            (
                "((a)[1])[2]",
                Expression::ArrayAt {
                    context: 1,
                    array: box Expression::ArrayAt {
                        context: 1,
                        array: box Expression::Identifier(1, "a".into()),
                        index: box Expression::I64(1, 1),
                    },
                    index: box Expression::I64(1, 2),
                },
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            match expression(&mut stream) {
                Ok(Some(e)) => assert_eq!(e, expected),
                Ok(t) => panic!("Expected an {:?} but got {:?}", expected, t),
                Err(err) => panic!("Expected {:?}, but got {}", expected, err),
            }
        }
    }

    #[test]
    fn parse_array_at_index_fails() {
        for (text, msg) in [
            ("a[5", "L0: Expected ], but found EOF"),
            ("a[5 6]", "L1: Expected ], but found i64 literal 6"),
            ("a[]", "L1: Index operator must contain valid expression"),
            ("a[2 + ]", "L1: expected expression after +"),
        ]
        .iter()
        {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            assert_eq!(expression(&mut stream), Err((*msg).into()), "{:?}", text);
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
            assert_eq!(*final_exp, Expression::I64(1, 5));
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_expression_block_bad() {
        for (text, msg) in [
            ("{5 10 51}", "L1: Expected }, but found i64 literal 10"),
            ("{5; 10 51}", "L1: Expected }, but found i64 literal 51"),
            ("{5; 10 let x:i64 := 5}", "L1: Expected }, but found let"),
            (
                "{let x: i64 := 10 5}",
                "L1: Expected ;, but found i64 literal 5",
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
        let text = "{let x:i64 := 5; f(x); x * x}";
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
                    assert_eq!(b.get_type(), Type::I64);
                    assert_eq!(*b.get_rhs(), Expression::I64(1, 5));
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
}
