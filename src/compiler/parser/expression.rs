use super::{Parser, ParserResult};

use super::{tokenstream::TokenStream, ParserContext, ParserError};
use crate::compiler::Span;
use crate::{
    compiler::{
        ast::*,
        diagnostics::{View, View2},
        lexer::tokens::{Lex, Token},
        source::SourceIr,
        CompilerError,
    },
    StringId,
};

impl ParserCombinator<ParserResult<Expression<ParserContext>>>
    for ParserResult<Expression<ParserContext>>
{
    fn por<F: Fn(&mut TokenStream) -> ParserResult<Expression<ParserContext>>>(
        &self,
        f: F,
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
    fn por<F: Fn(&mut TokenStream) -> R>(&self, f: F, ts: &mut TokenStream) -> R;
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
                err!(ctx.span(), ParserError::NotAUnaryOp(op.clone()))
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
                err!(ctx.span(), ParserError::NotABinaryOp(op.clone()))
            }
        }
    }
}

impl<'a> Parser<'a> {
    pub(super) fn expression_block(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        match stream.next_if(&Lex::LBrace) {
            Some(lbrace) => {
                let (event, result) = self.new_event(Span::zero()).and_then(|| {
                    // Read the statements composing the expression block
                    let mut stmts = vec![];
                    while let Some(s) = self.statement(stream)? {
                        stmts.push(s);
                    }

                    // Check if the block ends in an expression rather than a statement (no semicolon post fix)
                    let final_exp = self.expression(stream)?.map(|e| Box::new(e));

                    // Compute the span that goes from the `{` to the `}`
                    let ctx = stream
                        .next_must_be(&Lex::RBrace)?
                        .to_ctx()
                        .join(lbrace.to_ctx());

                    Ok(Some(Expression::ExpressionBlock(ctx, stmts, final_exp)))
                });
                result.view(|v| self.record(event.with_span(v.span()), Ok("Expression Block")))
            }
            None => Ok(None),
        }
    }

    pub(super) fn expression(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        self.logical_or(stream)
    }

    pub(super) fn logical_or(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        self.binary_op(stream, &vec![Lex::BOr], Self::logical_and)
    }

    pub(super) fn logical_and(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        self.binary_op(stream, &vec![Lex::BAnd], Self::comparison)
    }

    pub(super) fn comparison(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        self.binary_op(
            stream,
            &vec![Lex::Eq, Lex::NEq, Lex::Ls, Lex::LsEq, Lex::Gr, Lex::GrEq],
            Self::sum,
        )
    }

    pub(super) fn sum(&self, stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
        self.binary_op(stream, &vec![Lex::Add, Lex::Minus], Self::term)
    }

    pub(super) fn term(&self, stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
        self.binary_op(stream, &vec![Lex::Mul, Lex::Div], Self::negate)
    }

    pub(super) fn binary_op(
        &self,
        stream: &mut TokenStream,
        test: &Vec<Lex>,
        left_pattern: fn(&Self, &mut TokenStream) -> ParserResult<Expression<ParserContext>>,
    ) -> ParserResult<Expression<ParserContext>> {
        match left_pattern(self, stream)? {
            Some(left) => match stream.next_if_one_of(test.clone()) {
                Some(op) => {
                    let event = self.new_event(Span::zero());
                    self.binary_op(stream, test, left_pattern)?
                        .ok_or(CompilerError::new(
                            op.span(),
                            ParserError::ExpectedExprAfter(op.sym.clone()),
                        ))
                        //.view_err(|err| self.record(event.with_span(err.span()), Err(&err)))
                        .and_then(|right| {
                            Expression::binary_op(&op.sym, Box::new(left), Box::new(right))
                        })
                        .view(|v| self.record(event.with_span(v.span()), Ok(&op.sym.to_string())))
                    //.view_err(|err| self.record(event.with_span(err.span()), Err(&err)))
                }
                None => Ok(Some(left)),
            },
            None => Ok(None),
        }
    }

    pub(super) fn negate(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        match stream.next_if_one_of(vec![Lex::Minus, Lex::Not]) {
            Some(op) => {
                let event = self.new_event(Span::zero());
                self.negate(stream)
                    .and_then(|o| {
                        // TODO: I could create an if_none_then combinator that takes a Result<Option> and does this: if_none_then: Result<Option> -> Result<>
                        o.ok_or(CompilerError::new(
                            op.span(),
                            ParserError::ExpectedTermAfter(op.sym.clone()),
                        ))
                    })
                    //.view_err(|err| self.record(event.with_span(err.span()), Err(&err)))?;
                    .and_then(|factor| Expression::unary_op(op.to_ctx(), &op.sym, Box::new(factor)))
                    // TODO: Can I make view2: Result<Option<V, E>> -> F(Result<V,E>) then not have to have the double lambdas?
                    .view3(event, |event, v| {
                        let msg = v.map(|_| {
                            if op.sym == Lex::Minus {
                                "Arithmetic Negate"
                            } else {
                                "Boolean Negate"
                            }
                        });
                        self.record(event.with_span(v.span()), msg)
                    })
                //.view_err(|err| self.record(event.with_span(err.span()), Err(&err)))
            }
            None => self.member_access(stream),
        }
    }

    pub(super) fn member_access(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        match self.factor(stream)? {
            Some(f) => {
                let event = self.new_event(Span::zero());
                let mut ma = f;
                while let Some(token) =
                    stream.next_if_one_of(vec![Lex::MemberAccess, Lex::LBracket])
                {
                    ma = match token.sym {
                        Lex::MemberAccess => stream
                            .next_if_id()
                            .map(|(member, member_span)| {
                                Expression::MemberAccess(
                                    ma.context().extend(member_span),
                                    Box::new(ma),
                                    member,
                                )
                            })
                            //.view(|v| self.record(event.with_span(v.span()), Ok("Member Access")))
                            .ok_or(CompilerError::new(
                                token.span(),
                                ParserError::MemberAccessExpectedField,
                            )),
                            //.view_err(|err| self.record(event.with_span(err.span()), Err(&err)))?,
                        Lex::LBracket => self
                            .expression(stream)?
                            .ok_or(CompilerError::new(
                                token.span(),
                                ParserError::IndexOpInvalidExpr,
                            ))
                            .and_then(|index| {
                                stream.next_must_be(&Lex::RBracket).map(|rbracket| {
                                    Expression::ArrayAt {
                                        context: ma.context().join(rbracket.to_ctx()),
                                        array: Box::new(ma),
                                        index: Box::new(index),
                                    }
                                })
                            }),
                            /*
                            .map(|v| {
                                // TODO: can I make this a view?
                                self.record(event.with_span(v.span()), Ok("Array At"));
                                v
                            })
                            .view_err(|err| self.record(event.with_span(err.span()), Err(&err)))?,*/
                        _ => {
                            return err!(
                                token.span(),
                                ParserError::ExpectedButFound(
                                    vec![Lex::LBracket, Lex::MemberAccess],
                                    Some(token.sym.clone())
                                )
                            )
                            //.view_err(|err| self.record(event.with_span(err.span()), Err(&err)));
                        }
                    }
                            /*.map(|v| {
                                // TODO: can I make this a view?
                                self.record(event.with_span(v.span()), Ok("Array At"));
                                v
                            })*/
                            ?;
                }

                Ok(Some(ma))
            }
            None => Ok(None),
        }
    }

    pub(super) fn factor(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        match stream.peek() {
            Some(lparen) if lparen.sym == Lex::LParen => {
                let event = self.new_event(Span::zero());
                let ctx = lparen.to_ctx();
                stream.next();
                let mut exp = self.expression(stream)?;
                let rparen = stream.next_must_be(&Lex::RParen)?;
                let ctx = ctx.join(rparen.to_ctx());

                // Extend the Span of exp to cover the left paren and the right
                exp.as_mut().map(|exp| {
                    let ctx = exp.context().join(ctx);
                    *exp.get_context_mut() = ctx;
                });

                Ok(exp).view(|v| self.record(event.with_span(v.span()), Ok("Expression")))
            }
            _ => self
                .if_expression(stream)
                .por(|ts| self.while_expression(ts), stream)
                .por(|ts| self.expression_block(ts), stream)
                .por(|ts| self.function_call_or_variable(ts), stream)
                .por(|ts| self.co_yield(ts), stream)
                .por(|ts| self.constant(ts), stream)
                .por(|ts| self.array_expression(ts), stream),
        }
    }

    pub(super) fn if_expression(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        let event = self.new_event(Span::zero());
        match stream.next_if(&Lex::If) {
            Some(if_tok) => {
                stream.next_must_be(&Lex::LParen).and_then(|_| {
                    let cond = self.expression(stream)?.ok_or(CompilerError::new(
                        if_tok.span(),
                        ParserError::IfExpectedConditional,
                    ))?;
                    //.view_err(|err| self.record(event.with_span(err.span()), Err(&err)))?;
                    stream.next_must_be(&Lex::RParen)?;

                    let if_arm = self.expression_block(stream)?.ok_or(CompilerError::new(
                        if_tok.span(),
                        ParserError::IfTrueArmMissingExpr,
                    ))?;
                    //.view_err(|err| self.record(event.with_span(err.span()), Err(&err)))?;

                    // check for `else if`
                    let else_arm = match stream.next_if(&Lex::Else) {
                        Some(_) => match stream.peek() {
                            Some(Token {
                                sym: Lex::If, span, ..
                            }) => {
                                let span = *span;
                                Some(
                                    self.if_expression(stream)?.ok_or(CompilerError::new(
                                        span,
                                        ParserError::IfElseExpectedIfExpr,
                                    ))?, /*.view_err(|err| {
                                             self.record(event.with_span(err.span()), Err(&err))
                                         })?,*/
                                )
                            }
                            _ => {
                                let false_arm =
                                    self.expression_block(stream)?.ok_or(CompilerError::new(
                                        if_tok.span(),
                                        ParserError::IfFalseArmMissingExpr,
                                    ))?;
                                /* .view_err(|err| {
                                    self.record(event.with_span(err.span()), Err(&err))
                                })?;*/
                                Some(false_arm)
                            }
                        },
                        None => None,
                    };

                    let ctx = else_arm.as_ref().map_or_else(
                        || if_tok.to_ctx().join(*if_arm.context()),
                        |ea| if_tok.to_ctx().join(*ea.context()),
                    );

                    Ok(Some(Expression::If {
                        context: ctx,
                        cond: Box::new(cond),
                        if_arm: Box::new(if_arm),
                        else_arm: else_arm.map(|f| Box::new(f)),
                    }))
                })
            }
            _ => Ok(None),
        }
        .view(|v| self.record(event.with_span(v.span()), Ok("If Expression")))
    }

    pub(super) fn while_expression(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        let event = self.new_event(Span::zero());
        match stream.next_if(&Lex::While) {
            Some(whl) => {
                stream.next_must_be(&Lex::LParen).and_then(|_| {
                    let cond = self.expression(stream)?.ok_or(CompilerError::new(
                        whl.span(),
                        ParserError::WhileExpectedConditional,
                    ))?;
                    //.view_err(|err| self.record(event.with_span(err.span()), Err(&err)))?;
                    stream.next_must_be(&Lex::RParen)?;

                    self.expression_block(stream)?
                        .ok_or(CompilerError::new(whl.span, ParserError::WhileMissingBody))
                        .map(|body| {
                            Some(Expression::While {
                                context: whl.to_ctx().join(*body.context()),
                                cond: Box::new(cond),
                                body: Box::new(body),
                            })
                        })
                })
            }
            _ => Ok(None),
        }
        .view(|v| self.record(event.with_span(v.span()), Ok("While")))
        //.view_err(|err| self.record(event.with_span(err.span()), Err(&err)))
    }

    pub(super) fn function_call_or_variable(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        let event = self.new_event(Span::zero());
        match self.path(stream)? {
            Some((path, call_ctx)) => match self.routine_call_params(stream)? {
                Some((params, params_ctx)) => Ok(Some(Expression::RoutineCall(
                    call_ctx.join(params_ctx),
                    RoutineCall::Function,
                    path,
                    params.clone(),
                ))),
                None => match self.struct_expression_params(stream)? {
                    Some((params, params_ctx)) => Ok(Some(Expression::StructExpression(
                        call_ctx.join(params_ctx),
                        path,
                        params.clone(),
                    ))),
                    None => {
                        if path.len() > 1 {
                            Ok(Some(Expression::Path(call_ctx, path)))
                        } else {
                            if let Element::Id(sid) = path.last().unwrap() {
                                Ok(Some(Expression::Identifier(call_ctx, *sid)))
                            } else {
                                err!(call_ctx.span(), ParserError::PathExpectedIdentifier)
                            }
                        }
                    }
                },
            },
            _ => Ok(None),
        }
        .view(|v| {
            let msg = match &v {
                Expression::Identifier(..) => "Identifier",
                Expression::Path(..) => "Path",
                Expression::StructExpression(..) => "Struct Expression",
                Expression::RoutineCall(..) => "Routine Call",
                _ => panic!("Unexpected Expression variant"),
            };
            self.record(event.with_span(v.span()), Ok(msg))
        })
        //.view_err(|err| self.record(event.with_span(err.span()), Err(&err)))
    }

    pub(super) fn co_yield(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        let event = self.new_event(Span::zero());
        match stream.next_if(&Lex::Yield) {
            Some(yield_tok) => {
                let ctx = yield_tok.to_ctx();
                match self.expression(stream)? {
                    Some(coroutine) => {
                        let ctx = ctx.join(*coroutine.context());
                        Expression::new_yield(ctx, Box::new(coroutine))
                    }
                    None => {
                        err!(ctx.span(), ParserError::YieldExpectedIdentifier)
                    }
                }
            }
            None => Ok(None),
        }
        .view(|v| self.record(event.with_span(v.span()), Ok("Yield")))
        //.view_err(|err| self.record(event.with_span(err.span()), Err(&err)))
    }

    pub(super) fn struct_expression_params(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<(Vec<(StringId, Expression<ParserContext>)>, ParserContext)> {
        let event = self.new_event(Span::zero());
        match stream.next_if(&Lex::LBrace) {
            Some(lbrace) => {
                let mut params = vec![];
                while let Some((field_name, span)) = stream.next_if_id() {
                    stream.next_must_be(&Lex::Colon)?;
                    let field_value = self.expression(stream)?.ok_or(CompilerError::new(
                        span,
                        ParserError::StructExpectedFieldExpr(field_name),
                    ))?;
                    //.view_err(|err| self.record(event.with_span(err.span()), Err(&err)))?;
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
        .view(|v| {
            self.record(
                event.with_span(v.1.span()),
                Ok("Struct Expression Parameters"),
            )
        })
    }

    pub(super) fn array_expression(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        match stream.next_if(&Lex::LBracket) {
            Some(lbracket) => {
                let event = self.new_event(Span::zero());
                let mut elements = vec![];
                // loop through comma separated list of expressions
                while let Some(element) = self.expression(stream)? {
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
                    .view(|v| self.record(event.with_span(v.span()), Ok("Array Expression")))
            }
            None => Ok(None),
        }
    }

    pub(super) fn constant(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        self.number(stream)
            .por(|ts| self.boolean(ts), stream)
            .por(|ts| self.string_literal(ts), stream)
    }

    pub(super) fn number(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        let event = self.new_event(Span::zero());
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
                span,
                sym: Lex::U8(i),
                ..
            }) => Ok(Some(Expression::U8(ParserContext::new(span), i))),
            Some(Token {
                span,
                sym: Lex::U16(i),
                ..
            }) => Ok(Some(Expression::U16(ParserContext::new(span), i))),
            Some(Token {
                span,
                sym: Lex::U32(i),
                ..
            }) => Ok(Some(Expression::U32(ParserContext::new(span), i))),
            Some(Token {
                span,
                sym: Lex::U64(i),
                ..
            }) => Ok(Some(Expression::U64(ParserContext::new(span), i))),
            Some(Token {
                span,
                sym: Lex::I8(i),
                ..
            }) => Ok(Some(Expression::I8(ParserContext::new(span), i))),
            Some(Token {
                span,
                sym: Lex::I16(i),
                ..
            }) => Ok(Some(Expression::I16(ParserContext::new(span), i))),
            Some(Token {
                span,
                sym: Lex::I32(i),
                ..
            }) => Ok(Some(Expression::I32(ParserContext::new(span), i))),
            Some(Token {
                span,
                sym: Lex::I64(i),
                ..
            }) => Ok(Some(Expression::I64(ParserContext::new(span), i))),
            Some(t) => panic!("Unexpected token: {:?}", t),
            None => Ok(None),
        }
        .view(|v| self.record(event.with_span(v.span()), Ok("Number")))
    }

    pub(super) fn boolean(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        let event = self.new_event(Span::zero());
        match stream.next_if(&Lex::Bool(true)) {
            Some(Token {
                span,
                sym: Lex::Bool(b),
                ..
            }) => Ok(Some(Expression::Boolean(ParserContext::new(span), b))),
            _ => Ok(None),
        }
        .view(|v| self.record(event.with_span(v.span()), Ok("Boolean")))
    }

    pub(super) fn string_literal(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        let event = self.new_event(Span::zero());
        match stream.next_if(&Lex::StringLiteral(StringId::new())) {
            Some(Token {
                span,
                sym: Lex::StringLiteral(s),
                ..
            }) => Ok(Some(Expression::StringLiteral(ParserContext::new(span), s))),
            _ => Ok(None),
        }
        .view(|v| self.record(event.with_span(v.span()), Ok("String")))
    }
}
