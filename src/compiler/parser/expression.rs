use super::{
    parser::{ENABLE_TRACING, TRACE_END, TRACE_START},
    ParserResult,
};
use std::sync::atomic::Ordering;
use stdext::function_name;

use super::{
    parser::{block, path, routine_call_params},
    tokenstream::TokenStream,
    ParserContext, ParserError,
};
use crate::{
    compiler::{
        ast::*,
        lexer::tokens::{Lex, Token},
        CompilerError,
    },
    trace, StringId,
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
        ctx: ParserContext,
        coroutine_value: Box<Self>,
    ) -> ParserResult<Expression<ParserContext>> {
        Ok(Some(Expression::Yield(ctx, coroutine_value)))
    }

    pub fn unary_op(
        ctx: ParserContext,
        op: &Lex,
        operand: Box<Self>,
    ) -> ParserResult<Expression<ParserContext>> {
        match op {
            Lex::Minus => Ok(Some(Expression::UnaryOp(
                ctx.join(*operand.context()),
                UnaryOperator::Negate,
                operand,
            ))),
            Lex::Not => Ok(Some(Expression::UnaryOp(
                ctx.join(*operand.context()),
                UnaryOperator::Not,
                operand,
            ))),
            _ => {
                err!(ctx.line(), ctx.span(), ParserError::NotAUnaryOp(op.clone()))
            }
        }
    }

    pub fn binary_op(
        op: &Lex,
        left: Box<Self>,
        right: Box<Self>,
    ) -> ParserResult<Expression<ParserContext>> {
        let ctx = left.context().join(*right.context());
        match op {
            Lex::Eq => Ok(Some(Expression::BinaryOp(
                ctx,
                BinaryOperator::Eq,
                left,
                right,
            ))),
            Lex::NEq => Ok(Some(Expression::BinaryOp(
                ctx,
                BinaryOperator::NEq,
                left,
                right,
            ))),
            Lex::Ls => Ok(Some(Expression::BinaryOp(
                ctx,
                BinaryOperator::Ls,
                left,
                right,
            ))),
            Lex::LsEq => Ok(Some(Expression::BinaryOp(
                ctx,
                BinaryOperator::LsEq,
                left,
                right,
            ))),
            Lex::Gr => Ok(Some(Expression::BinaryOp(
                ctx,
                BinaryOperator::Gr,
                left,
                right,
            ))),
            Lex::GrEq => Ok(Some(Expression::BinaryOp(
                ctx,
                BinaryOperator::GrEq,
                left,
                right,
            ))),
            Lex::BAnd => Ok(Some(Expression::BinaryOp(
                ctx,
                BinaryOperator::BAnd,
                left,
                right,
            ))),
            Lex::BOr => Ok(Some(Expression::BinaryOp(
                ctx,
                BinaryOperator::BOr,
                left,
                right,
            ))),
            Lex::Add => Ok(Some(Expression::BinaryOp(
                ctx,
                BinaryOperator::Add,
                left,
                right,
            ))),
            Lex::Minus => Ok(Some(Expression::BinaryOp(
                ctx,
                BinaryOperator::Sub,
                left,
                right,
            ))),
            Lex::Mul => Ok(Some(Expression::BinaryOp(
                ctx,
                BinaryOperator::Mul,
                left,
                right,
            ))),
            Lex::Div => Ok(Some(Expression::BinaryOp(
                ctx,
                BinaryOperator::Div,
                left,
                right,
            ))),
            _ => {
                err!(
                    ctx.line(),
                    ctx.span(),
                    ParserError::NotABinaryOp(op.clone())
                )
            }
        }
    }
}

fn expression_block(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    match stream.next_if(&Lex::LBrace) {
        Some(lbrace) => {
            let stmts = block(stream)?;

            let final_exp = expression(stream)?.map(|e| Box::new(e));

            let ctx = stream
                .next_must_be(&Lex::RBrace)?
                .to_ctx()
                .join(lbrace.to_ctx());
            Ok(Some(Expression::ExpressionBlock(ctx, stmts, final_exp)))
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
                let right = binary_op(stream, test, left_pattern)?.ok_or(CompilerError::new(
                    op.to_ctx().line(),
                    op.to_ctx().span(),
                    ParserError::ExpectedExprAfter(op.sym.clone()),
                ))?;
                Expression::binary_op(&op.sym, Box::new(left), Box::new(right))
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
            let factor = negate(stream)?.ok_or(CompilerError::new(
                op.to_ctx().line(),
                op.to_ctx().span(),
                ParserError::ExpectedTermAfter(op.sym.clone()),
            ))?;
            Expression::unary_op(op.to_ctx(), &op.sym, Box::new(factor))
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
                ma = match token.sym {
                    Lex::MemberAccess => stream
                        .next_if_id()
                        .map(|(member, _, member_span)| {
                            Expression::MemberAccess(
                                ma.context().extend(member_span),
                                Box::new(ma),
                                member,
                            )
                        })
                        .ok_or(CompilerError::new(
                            token.line,
                            token.span,
                            ParserError::MemberAccessExpectedField,
                        ))?,
                    Lex::LBracket => expression(stream)?
                        .ok_or(CompilerError::new(
                            token.to_ctx().line(),
                            token.span,
                            ParserError::IndexOpInvalidExpr,
                        ))
                        .map(|index| {
                            stream.next_must_be(&Lex::RBracket).map(|rbracket| {
                                Expression::ArrayAt {
                                    context: ma.context().join(rbracket.to_ctx()),
                                    array: box ma,
                                    index: box index,
                                }
                            })
                        })??,
                    _ => {
                        return err!(
                            token.line,
                            token.span,
                            ParserError::ExpectedButFound(
                                vec![Lex::LBracket, Lex::MemberAccess],
                                Some(token.sym.clone())
                            )
                        )
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
        Some(lparen) if lparen.sym == Lex::LParen => {
            let ctx = lparen.to_ctx();
            stream.next();
            let mut exp = expression(stream)?;
            let rparen = stream.next_must_be(&Lex::RParen)?;
            let ctx = ctx.join(rparen.to_ctx());

            // Extend the Span of exp to cover the left paren and the right
            exp.as_mut().map(|exp| {
                let ctx = exp.context().join(ctx);
                *exp.get_context_mut() = ctx;
            });

            Ok(exp)
        }
        _ => if_expression(stream)
            .por(while_expression, stream)
            .por(expression_block, stream)
            .por(function_call_or_variable, stream)
            .por(co_yield, stream)
            .por(constant, stream)
            .por(array_expression, stream),
    }
}

fn if_expression(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    Ok(match stream.next_if(&Lex::If) {
        Some(if_tok) => {
            stream.next_must_be(&Lex::LParen)?;
            let cond = expression(stream)?.ok_or(CompilerError::new(
                if_tok.line,
                if_tok.span,
                ParserError::IfExpectedConditional,
            ))?;
            stream.next_must_be(&Lex::RParen)?;

            let if_arm = expression_block(stream)?.ok_or(CompilerError::new(
                if_tok.line,
                if_tok.span,
                ParserError::IfTrueArmMissingExpr,
            ))?;

            // check for `else if`
            let else_arm = match stream.next_if(&Lex::Else) {
                Some(_) => match stream.peek() {
                    Some(Token {
                        line: l,
                        sym: Lex::If,
                        span,
                    }) => {
                        let l = *l;
                        let span = *span;
                        Some(if_expression(stream)?.ok_or(CompilerError::new(
                            l,
                            span,
                            ParserError::IfElseExpectedIfExpr,
                        ))?)
                    }
                    _ => {
                        let false_arm = expression_block(stream)?.ok_or(CompilerError::new(
                            if_tok.line,
                            if_tok.span,
                            ParserError::IfFalseArmMissingExpr,
                        ))?;
                        Some(false_arm)
                    }
                },
                None => None,
            };

            let ctx = else_arm.as_ref().map_or_else(
                || if_tok.to_ctx().join(*if_arm.context()),
                |ea| if_tok.to_ctx().join(*ea.context()),
            );

            Some(Expression::If {
                context: ctx,
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
        Some(whl) => {
            stream.next_must_be(&Lex::LParen)?;
            let cond = expression(stream)?.ok_or(CompilerError::new(
                whl.line,
                whl.span,
                ParserError::WhileExpectedConditional,
            ))?;
            stream.next_must_be(&Lex::RParen)?;

            let body = expression_block(stream)?.ok_or(CompilerError::new(
                whl.line,
                whl.span,
                ParserError::WhileMissingBody,
            ))?;

            Some(Expression::While {
                context: whl.to_ctx().join(*body.context()),
                cond: Box::new(cond),
                body: Box::new(body),
            })
        }
        _ => None,
    })
}

fn function_call_or_variable(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    let s: Option<Expression<ParserContext>> = match path(stream)? {
        Some((path, call_ctx)) => match routine_call_params(stream)? {
            Some((params, params_ctx)) => Some(Expression::RoutineCall(
                call_ctx.join(params_ctx),
                RoutineCall::Function,
                path,
                params.clone(),
            )),
            None => match struct_expression_params(stream)? {
                Some((params, params_ctx)) => Some(Expression::StructExpression(
                    call_ctx.join(params_ctx),
                    path,
                    params.clone(),
                )),
                None => {
                    if path.len() > 1 {
                        Some(Expression::Path(call_ctx, path))
                    } else {
                        if let Element::Id(sid) = path.last().unwrap() {
                            Some(Expression::Identifier(call_ctx, *sid))
                        } else {
                            return err!(
                                call_ctx.line(),
                                call_ctx.span(),
                                ParserError::PathExpectedIdentifier
                            );
                        }
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
        Some(yield_tok) => {
            let ctx = yield_tok.to_ctx();
            match expression(stream)? {
                Some(coroutine) => {
                    let ctx = ctx.join(*coroutine.context());
                    Expression::new_yield(ctx, Box::new(coroutine))
                }
                None => {
                    err!(ctx.line(), ctx.span(), ParserError::YieldExpectedIdentifier)
                }
            }
        }
        None => Ok(None),
    }
}

fn struct_expression_params(
    stream: &mut TokenStream,
) -> ParserResult<(Vec<(StringId, Expression<ParserContext>)>, ParserContext)> {
    trace!(stream);
    match stream.next_if(&Lex::LBrace) {
        Some(lbrace) => {
            let mut params = vec![];
            while let Some((field_name, ln, span)) = stream.next_if_id() {
                stream.next_must_be(&Lex::Colon)?;
                let field_value = expression(stream)?.ok_or(CompilerError::new(
                    ln,
                    span,
                    ParserError::StructExpectedFieldExpr(field_name),
                ))?;
                params.push((field_name, field_value));
                match stream.next_if(&Lex::Comma) {
                    Some(_) => {}
                    None => break,
                };
            }

            let ctx = stream
                .next_must_be(&Lex::RBrace)?
                .to_ctx()
                .join(lbrace.to_ctx());
            Ok(Some((params, ctx)))
        }
        _ => Ok(None),
    }
}

fn array_expression(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    match stream.next_if(&Lex::LBracket) {
        Some(lbracket) => {
            let mut elements = vec![];
            // loop through comma separated list of expressions
            while let Some(element) = expression(stream)? {
                elements.push(element);
                match stream.next_if(&Lex::Comma) {
                    Some(_) => {}
                    None => break,
                };
            }
            let rbracket = stream.next_must_be(&Lex::RBracket)?;

            // Compute the new span
            let ctx = lbracket.to_ctx().join(rbracket.to_ctx());

            let len = elements.len();
            Ok(Some(Expression::ArrayExpression(ctx, elements, len)))
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
        Some(Token {
            line: l,
            span,
            sym: Lex::U8(i),
            ..
        }) => Ok(Some(Expression::U8(ParserContext::new(l, span), i))),
        Some(Token {
            line: l,
            span,
            sym: Lex::U16(i),
            ..
        }) => Ok(Some(Expression::U16(ParserContext::new(l, span), i))),
        Some(Token {
            line: l,
            span,
            sym: Lex::U32(i),
            ..
        }) => Ok(Some(Expression::U32(ParserContext::new(l, span), i))),
        Some(Token {
            line: l,
            span,
            sym: Lex::U64(i),
            ..
        }) => Ok(Some(Expression::U64(ParserContext::new(l, span), i))),
        Some(Token {
            line: l,
            span,
            sym: Lex::I8(i),
            ..
        }) => Ok(Some(Expression::I8(ParserContext::new(l, span), i))),
        Some(Token {
            line: l,
            span,
            sym: Lex::I16(i),
            ..
        }) => Ok(Some(Expression::I16(ParserContext::new(l, span), i))),
        Some(Token {
            line: l,
            span,
            sym: Lex::I32(i),
            ..
        }) => Ok(Some(Expression::I32(ParserContext::new(l, span), i))),
        Some(Token {
            line: l,
            span,
            sym: Lex::I64(i),
            ..
        }) => Ok(Some(Expression::I64(ParserContext::new(l, span), i))),
        Some(t) => panic!("Unexpected token: {:?}", t),
        None => Ok(None),
    }
}

fn boolean(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    match stream.next_if(&Lex::Bool(true)) {
        Some(Token {
            line: l,
            span,
            sym: Lex::Bool(b),
            ..
        }) => Ok(Some(Expression::Boolean(ParserContext::new(l, span), b))),
        _ => Ok(None),
    }
}

fn string_literal(stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
    trace!(stream);
    match stream.next_if(&Lex::StringLiteral(StringId::new())) {
        Some(Token {
            line: l,
            span,
            sym: Lex::StringLiteral(s),
            ..
        }) => Ok(Some(Expression::StringLiteral(
            ParserContext::new(l, span),
            s,
        ))),
        _ => Ok(None),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        compiler::{
            ast::{Statement, Type},
            lexer::{tokens::Token, LexerError},
            source::Offset,
            CompilerError, Lexer, Span,
        },
        StringTable,
    };
    type LResult = std::result::Result<Vec<Token>, CompilerError<LexerError>>;

    fn new_span(low: u32, high: u32) -> Span {
        Span::new(Offset::new(low), Offset::new(high))
    }

    fn new_ctx(low: u32, high: u32) -> ParserContext {
        ParserContext::new(1, new_span(low, high))
    }

    #[test]
    fn parse_number() {
        for (text, expected) in vec![
            ("64u8", Expression::U8(new_ctx(0, 4), 64)),
            ("64u16", Expression::U16(new_ctx(0, 5), 64)),
            ("64u32", Expression::U32(new_ctx(0, 5), 64)),
            ("64u64", Expression::U64(new_ctx(0, 5), 64)),
            ("5i8", Expression::I8(new_ctx(0, 3), 5)),
            ("5i16", Expression::I16(new_ctx(0, 4), 5)),
            ("5i32", Expression::I32(new_ctx(0, 4), 5)),
            ("64i64", Expression::I64(new_ctx(0, 5), 64)),
            ("64", Expression::I64(new_ctx(0, 2), 64)),
        ] {
            let mut table = StringTable::new();
            let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            match number(&mut stream) {
                Ok(Some(e)) => assert_eq!(e, expected),
                Ok(t) => panic!("Expected an {:?} but got {:?}", expected, t),
                Err(err) => panic!("Expected {:?}, but got {:?}", expected, err),
            }
        }
    }

    #[test]
    fn parse_array_expression() {
        for (text, expected) in vec![
            (
                "[1]",
                Expression::ArrayExpression(
                    new_ctx(0, 3),
                    vec![Expression::I64(new_ctx(1, 2), 1)],
                    1,
                ),
            ),
            (
                "[1u8]",
                Expression::ArrayExpression(
                    new_ctx(0, 5),
                    vec![Expression::U8(new_ctx(1, 4), 1)],
                    1,
                ),
            ),
            (
                "[1,]",
                Expression::ArrayExpression(
                    new_ctx(0, 4),
                    vec![Expression::I64(new_ctx(1, 2), 1)],
                    1,
                ),
            ),
            (
                "[1, 2, 3]",
                Expression::ArrayExpression(
                    new_ctx(0, 9),
                    vec![
                        Expression::I64(new_ctx(1, 2), 1),
                        Expression::I64(new_ctx(4, 5), 2),
                        Expression::I64(new_ctx(7, 8), 3),
                    ],
                    3,
                ),
            ),
            (
                "[1, 2i8, 3]", // This is legal at the parser level (it is illegal semantically)
                Expression::ArrayExpression(
                    new_ctx(0, 11),
                    vec![
                        Expression::I64(new_ctx(1, 2), 1),
                        Expression::I8(new_ctx(4, 7), 2),
                        Expression::I64(new_ctx(9, 10), 3),
                    ],
                    3,
                ),
            ),
            (
                "[[1,],]",
                Expression::ArrayExpression(
                    new_ctx(0, 7),
                    vec![Expression::ArrayExpression(
                        new_ctx(1, 5),
                        vec![Expression::I64(new_ctx(2, 3), 1)],
                        1,
                    )],
                    1,
                ),
            ),
        ] {
            let mut table = StringTable::new();
            let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            match array_expression(&mut stream) {
                Ok(Some(e)) => assert_eq!(e, expected),
                Ok(t) => panic!("Expected an {:?} but got {:?}", expected, t),
                Err(err) => panic!("Expected {:?}, but got {:?}", expected, err),
            }
        }
    }

    #[test]
    fn parse_array_fails() {
        for (text, msg) in [
            (
                "[5",
                CompilerError::new(
                    0,
                    Span::zero(),
                    ParserError::ExpectedButFound(vec![Lex::RBracket], None),
                ),
            ),
            (
                "[5 6]",
                CompilerError::new(
                    1,
                    Span::zero(),
                    ParserError::ExpectedButFound(vec![Lex::RBracket], Some(Lex::I64(6))),
                ),
            ),
        ]
        .iter()
        {
            let mut table = StringTable::new();
            let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            assert_eq!(
                array_expression(&mut stream).unwrap_err(),
                *msg,
                "{:?}",
                text
            );
        }
    }

    #[test]
    fn parse_array_at_index() {
        let mut table = StringTable::new();
        let a = table.insert("a".into());
        let b = table.insert("b".into());

        for (text, expected) in vec![
            //
            (
                "a[1]",
                Expression::ArrayAt {
                    context: new_ctx(0, 4),
                    array: box Expression::Identifier(new_ctx(0, 1), a),
                    index: box Expression::I64(new_ctx(2, 3), 1),
                },
            ),
            (
                "(a)[1]",
                Expression::ArrayAt {
                    context: new_ctx(0, 6),
                    array: box Expression::Identifier(new_ctx(0, 3), a),
                    index: box Expression::I64(new_ctx(4, 5), 1),
                },
            ),
            (
                "a.b[1]",
                Expression::ArrayAt {
                    context: new_ctx(0, 6),
                    array: box Expression::MemberAccess(
                        new_ctx(0, 3),
                        box Expression::Identifier(new_ctx(0, 1), a),
                        b,
                    ),
                    index: box Expression::I64(new_ctx(4, 5), 1),
                },
            ),
            (
                "a[1].b",
                Expression::MemberAccess(
                    new_ctx(0, 6),
                    box Expression::ArrayAt {
                        context: new_ctx(0, 4),
                        array: box Expression::Identifier(new_ctx(0, 1), a),
                        index: box Expression::I64(new_ctx(2, 3), 1),
                    },
                    b,
                ),
            ),
            (
                "a[0].b[1]",
                Expression::ArrayAt {
                    context: new_ctx(0, 9),
                    array: box Expression::MemberAccess(
                        new_ctx(0, 6),
                        box Expression::ArrayAt {
                            context: new_ctx(0, 4),
                            array: box Expression::Identifier(new_ctx(0, 1), a),
                            index: box Expression::I64(new_ctx(2, 3), 0),
                        },
                        b,
                    ),
                    index: box Expression::I64(new_ctx(7, 8), 1),
                },
            ),
            (
                "a[1][2]",
                Expression::ArrayAt {
                    context: new_ctx(0, 7),
                    array: box Expression::ArrayAt {
                        context: new_ctx(0, 4),
                        array: box Expression::Identifier(new_ctx(0, 1), a),
                        index: box Expression::I64(new_ctx(2, 3), 1),
                    },
                    index: box Expression::I64(new_ctx(5, 6), 2),
                },
            ),
            (
                "((a)[1])[2]",
                Expression::ArrayAt {
                    context: new_ctx(0, 11),
                    array: box Expression::ArrayAt {
                        context: new_ctx(0, 8),
                        array: box Expression::Identifier(new_ctx(1, 4), a),
                        index: box Expression::I64(new_ctx(5, 6), 1),
                    },
                    index: box Expression::I64(new_ctx(9, 10), 2),
                },
            ),
        ] {
            println!("Test: {}", text);
            let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            match expression(&mut stream) {
                Ok(Some(e)) => assert_eq!(e, expected),
                Ok(t) => panic!("Expected an {:?} but got {:?}", expected, t),
                Err(err) => panic!("Expected {:?}, but got {:?}", expected, err),
            }
        }
    }

    #[test]
    fn parse_array_at_index_fails() {
        for (text, msg) in [
            (
                "a[5",
                CompilerError::new(
                    0,
                    Span::new(Offset::new(1), Offset::new(3)),
                    ParserError::ExpectedButFound(vec![Lex::RBracket], None),
                ),
            ),
            (
                "a[5 6]",
                CompilerError::new(
                    1,
                    Span::new(Offset::new(1), Offset::new(6)),
                    ParserError::ExpectedButFound(vec![Lex::RBracket], Some(Lex::I64(6))),
                ),
            ),
            (
                "a[]",
                CompilerError::new(
                    1,
                    Span::new(Offset::new(4), Offset::new(5)),
                    ParserError::IndexOpInvalidExpr,
                ),
            ),
            (
                "a[2 + ]",
                CompilerError::new(
                    1,
                    Span::new(Offset::new(2), Offset::new(5)),
                    ParserError::ExpectedExprAfter(Lex::Add),
                ),
            ),
        ]
        .iter()
        {
            let mut table = StringTable::new();
            let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            assert_eq!(expression(&mut stream).unwrap_err(), *msg, "{:?}", text);
        }
    }

    #[test]
    fn parse_member_access() {
        for (text, low, high) in vec![
            (" thing.first", 1, 6),
            ("(thing).first", 0, 7),
            ("(thing.first)", 1, 6),
        ] {
            let mut table = StringTable::new();
            let thing_id = table.insert("thing".into());
            let first_id = table.insert("first".into());
            let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            match member_access(&mut stream) {
                Ok(Some(Expression::MemberAccess(ctx, left, right))) => {
                    assert_eq!(ctx.line(), 1);
                    assert_eq!(
                        *left,
                        Expression::Identifier(new_ctx(low, high), thing_id),
                        "Input: {}",
                        text,
                    );
                    assert_eq!(right, first_id);
                }
                Ok(Some(n)) => panic!("{} resulted in {:?}", text, n),
                Ok(None) => panic!("No node returned for {}", text),
                Err(msg) => panic!("{} caused {:?}", text, msg),
            }
        }
    }

    #[test]
    fn parse_expression_block_oneline() {
        let text = "{5}";
        let mut table = StringTable::new();
        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        if let Some(Expression::ExpressionBlock(ctx, body, Some(final_exp))) =
            expression_block(&mut stream).unwrap()
        {
            assert_eq!(ctx, new_ctx(0, 3));
            assert_eq!(body.len(), 0);
            assert_eq!(*final_exp, Expression::I64(new_ctx(1, 2), 5));
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_expression_block_bad() {
        for (text, msg) in [
            (
                "{5 10 51}",
                CompilerError::new(
                    1,
                    Span::new(Offset::new(1), Offset::new(2)),
                    ParserError::ExpectedButFound(vec![Lex::RBrace], Some(Lex::I64(10))),
                ),
            ),
            (
                "{5; 10 51}",
                CompilerError::new(
                    1,
                    Span::new(Offset::new(4), Offset::new(6)),
                    ParserError::ExpectedButFound(vec![Lex::RBrace], Some(Lex::I64(51))),
                ),
            ),
            (
                "{5; 10 let x:i64 := 5}",
                CompilerError::new(
                    1,
                    Span::new(Offset::new(4), Offset::new(6)),
                    ParserError::ExpectedButFound(vec![Lex::RBrace], Some(Lex::Let)),
                ),
            ),
            (
                "{let x: i64 := 10 5}",
                CompilerError::new(
                    1,
                    Span::new(Offset::new(1), Offset::new(17)),
                    ParserError::ExpectedButFound(vec![Lex::Semicolon], Some(Lex::I64(5))),
                ),
            ),
        ]
        .iter()
        {
            let mut table = StringTable::new();
            let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            assert_eq!(
                expression_block(&mut stream).unwrap_err(),
                *msg,
                "{:?}",
                text
            );
        }
    }

    #[test]
    fn parse_expression_block_multiline() {
        let text = "{let x:i64 := 5; f(x); x * x}";
        let mut table = StringTable::new();
        let x = table.insert("x".into());
        let f = table.insert("f".into());
        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        if let Some(Expression::ExpressionBlock(ctx, body, Some(final_exp))) =
            expression_block(&mut stream).unwrap()
        {
            assert_eq!(ctx, new_ctx(0, 29));
            assert_eq!(body.len(), 2);
            match &body[0] {
                Statement::Bind(box b) => {
                    assert_eq!(b.get_id(), x);
                    assert_eq!(b.get_type(), Type::I64);
                    assert_eq!(*b.get_rhs(), Expression::I64(new_ctx(14, 15), 5));
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
                    assert_eq!(*fn_name, vec![Element::Id(f)].into());
                    assert_eq!(params[0], Expression::Identifier(new_ctx(19, 20), x));
                }
                _ => panic!("No body: {:?}", &body[1]),
            }
            match final_exp {
                box Expression::BinaryOp(_, BinaryOperator::Mul, l, r) => {
                    assert_eq!(*l.as_ref(), Expression::Identifier(new_ctx(23, 24), x));
                    assert_eq!(*r.as_ref(), Expression::Identifier(new_ctx(27, 28), x));
                }
                _ => panic!("No body: {:?}", &body[2]),
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }
}
