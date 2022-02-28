use super::{Parser, ParserResult};

use super::{tokenstream::TokenStream, ParserContext, ParserError};
use crate::compiler::Span;
use crate::{
    compiler::{
        ast::*,
        diagnostics::View2,
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
        cond: &[Lex],
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
        cond: &[Lex],
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
            Lex::Hat => Ok(Some(Expression::UnaryOp(
                ctx.join(*operand.context()),
                UnaryOperator::DerefRawPointer,
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
            Lex::At => Ok(Some(Expression::BinaryOp(
                ctx,
                BinaryOperator::RawPointerOffset,
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
                result.view(|v| {
                    let msg = v.map(|_| "Expression Block");
                    self.record(event.with_span(v.span()), msg)
                })
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
        self.binary_op(stream, &[Lex::BOr], Self::logical_and)
    }

    pub(super) fn logical_and(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        self.binary_op(stream, &[Lex::BAnd], Self::comparison)
    }

    pub(super) fn comparison(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        self.binary_op(
            stream,
            &[Lex::Eq, Lex::NEq, Lex::Ls, Lex::LsEq, Lex::Gr, Lex::GrEq],
            Self::sum,
        )
    }

    pub(super) fn sum(&self, stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
        self.binary_op(stream, &[Lex::Add, Lex::Minus, Lex::At], Self::term)
    }

    pub(super) fn term(&self, stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
        self.binary_op(stream, &[Lex::Mul, Lex::Div], Self::cast)
    }

    pub(super) fn binary_op(
        &self,
        stream: &mut TokenStream,
        test: &[Lex],
        left_pattern: fn(&Self, &mut TokenStream) -> ParserResult<Expression<ParserContext>>,
    ) -> ParserResult<Expression<ParserContext>> {
        let mut msg = None;
        let (event, result) =
            self.new_event(Span::zero())
                .and_then(|| match left_pattern(self, stream)? {
                    Some(left) => match stream.next_if_one_of(test) {
                        Some(op) => {
                            msg = Some(op.sym.to_string());
                            self.binary_op(stream, test, left_pattern)?
                                .ok_or(CompilerError::new(
                                    op.span(),
                                    ParserError::ExpectedExprAfter(op.sym.clone()),
                                ))
                                .and_then(|right| {
                                    Expression::binary_op(&op.sym, Box::new(left), Box::new(right))
                                })
                        }
                        None => Ok(Some(left)),
                    },
                    None => Ok(None),
                });

        result.view(|v| match msg {
            Some(msg) => self.record(event.with_span(v.span()), Ok(&msg)),
            None => self.record_noop(event.with_span(v.span())),
        })
    }

    fn address_of(&self, stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
        let (event, result) =
            self.new_event(Span::zero())
                .and_then(|| match stream.next_if(&Lex::At) {
                    Some(at) => {
                        // Check for const or mut
                        match stream.next_if_one_of(&[Lex::Const, Lex::Mut]) {
                            Some(mutability) => {
                                let at_ctx = at.to_ctx().join(mutability.to_ctx());
                                let mutability = match mutability.sym {
                                    Lex::Const => Ok(PointerMut::Const),
                                    Lex::Mut => Ok(PointerMut::Mut),
                                    _ => Err(CompilerError::new(
                                        mutability.span(),
                                        ParserError::AddressOfExpectedConstOrMut,
                                    )),
                                }?;

                                let id = self.subdata_access(stream)?.ok_or(CompilerError::new(
                                    at_ctx.span(),
                                    ParserError::ExpectedIdentifierAfter(at.sym.clone()),
                                ))?;
                                let id_ctx = *id.context();

                                let ctx = at.to_ctx().join(id_ctx);

                                Ok(Some(match mutability {
                                    PointerMut::Mut => Expression::UnaryOp(
                                        ctx,
                                        UnaryOperator::AddressMut,
                                        Box::new(id),
                                    ),
                                    PointerMut::Const => Expression::UnaryOp(
                                        ctx,
                                        UnaryOperator::AddressConst,
                                        Box::new(id),
                                    ),
                                }))
                            }
                            None => Err(CompilerError::new(
                                at.span(),
                                ParserError::AddressOfExpectedConstOrMut,
                            )),
                        }
                    }
                    None => Ok(None),
                });

        result.view(|v| {
            let msg = v.map(|_| "Address Of");
            self.record(event.with_span(v.span()), msg)
        })
    }

    pub(super) fn cast(&self, stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
        // Parser unary expressions
        let mut msg = None;
        let (event, result) = self.new_event(Span::zero()).and_then(|| {
            match self.negate(stream)? {
                Some(exp) => {
                    // Check of the `as` operator
                    match stream.next_if(&Lex::As) {
                        Some(as_tok) => {
                            msg = Some("cast");
                            // If found, then parse a type expression
                            let (ty, ty_ctx) =
                                self.consume_type(stream)?.ok_or(CompilerError::new(
                                    exp.context().join(as_tok.to_ctx()).span(),
                                    ParserError::InvalidCastTarget,
                                ))?;
                            let cast_ctx = exp.context().join(ty_ctx);

                            // Create the cast node
                            Ok(Some(Expression::TypeCast(cast_ctx, Box::new(exp), ty)))
                        }
                        None => Ok(Some(exp)),
                    }
                }
                None => Ok(None),
            }
        });

        result.view(|v| {
            match msg {
                Some(msg) => self.record(event.with_span(v.span()), Ok(&msg)),
                None => self.record_noop(event.with_span(v.span())),
            };
        })
    }

    pub(super) fn negate(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        match self.address_of(stream)? {
            Some(exp) => Ok(Some(exp)),
            None => match stream.next_if_one_of(&[Lex::Minus, Lex::Not, Lex::Hat]) {
                Some(op) => {
                    let (event, result) = self.new_event(Span::zero()).and_then(|| {
                        self.negate(stream)
                            .and_then(|o| {
                                o.ok_or(CompilerError::new(
                                    op.span(),
                                    ParserError::ExpectedTermAfter(op.sym.clone()),
                                ))
                            })
                            .and_then(|factor| {
                                Expression::unary_op(op.to_ctx(), &op.sym, Box::new(factor))
                            })
                    });

                    result.view(|v| {
                        let msg = v.map(|_| match op.sym {
                            Lex::Minus => "Arithmetic Negate",
                            Lex::Not => "Boolean Negate",
                            Lex::Hat => "Deref Raw Pointer",
                            _ => panic!("Invalid Unary Operator"),
                        });
                        self.record(event.with_span(v.span()), msg)
                    })
                }
                None => self.subdata_access(stream),
            },
        }
    }

    pub(super) fn subdata_access(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        let (event, result) =
            self.new_event(Span::zero())
                .and_then(|| match self.factor(stream)? {
                    Some(f) => self.subdata_access_sequence(f, stream),
                    None => Ok(None),
                });

        result.view(|v| match v {
            Ok(Expression::MemberAccess(..)) => self.record(
                event.with_span(v.span()),
                Ok("Subdata Access: Member Access"),
            ),
            Ok(Expression::ArrayAt { .. }) => {
                self.record(event.with_span(v.span()), Ok("Subdata Access: Array At"))
            }
            Ok(_) => self.record_noop(event.with_span(v.span())),
            Err(err) => self.record(event.with_span(v.span()), Err(err)),
        })
    }

    fn subdata_access_sequence(
        &self,
        factor: Expression<ParserContext>,
        stream: &mut TokenStream,
    ) -> Result<Option<Expression<ParserContext>>, CompilerError<ParserError>> {
        match stream.peek() {
            Some(tok) if tok.sym == Lex::MemberAccess => {
                // This panics rather than throws an error because if we see a Member Access operator
                // then what follows is either a valid member access or an error. So, if this returns
                // Ok(None) then there is an unrecoverable disconnect between what this function expects
                // and what member_access does.
                let ma = self
                    .member_access(factor, stream)?
                    .expect("Member Access Failed to Parse");
                self.subdata_access_sequence(ma, stream)
            }
            Some(tok) if tok.sym == Lex::LBracket => {
                let aa = self
                    .array_access(factor, stream)?
                    .expect("Array Access Failed to Parse");
                self.subdata_access_sequence(aa, stream)
            }
            _ => Ok(Some(factor)),
        }
    }

    fn array_access(
        &self,
        ma: Expression<ParserContext>,
        stream: &mut TokenStream,
    ) -> Result<Option<Expression<ParserContext>>, CompilerError<ParserError>> {
        if let Some(token) = stream.next_if(&Lex::LBracket) {
            self.expression(stream)?
                .ok_or(CompilerError::new(
                    token.span(),
                    ParserError::IndexOpInvalidExpr,
                ))
                .and_then(|index| {
                    stream.next_must_be(&Lex::RBracket).map(|rbracket| {
                        Some(Expression::ArrayAt {
                            context: ma.context().join(rbracket.to_ctx()),
                            array: Box::new(ma),
                            index: Box::new(index),
                        })
                    })
                })
        } else {
            Ok(None)
        }
    }

    fn member_access(
        &self,
        ma: Expression<ParserContext>,
        stream: &mut TokenStream,
    ) -> Result<Option<Expression<ParserContext>>, CompilerError<ParserError>> {
        if let Some(token) = stream.next_if(&Lex::MemberAccess) {
            stream
                .next_if_id()
                .ok_or(CompilerError::new(
                    token.span(),
                    ParserError::MemberAccessExpectedField,
                ))
                .map(|(member, member_span)| {
                    self.record_terminal(member_span, Ok("Structure Field"));
                    Some(Expression::MemberAccess(
                        ma.context().extend(member_span),
                        Box::new(ma),
                        member,
                    ))
                })
        } else {
            Ok(None)
        }
    }

    pub(super) fn factor(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        match stream.peek() {
            Some(lparen) if lparen.sym == Lex::LParen => {
                let lparen = *lparen;
                let (event, result) = self.new_event(Span::zero()).and_then(|| {
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

                    Ok(exp)
                });
                result.view(|v| {
                    let msg = v.map(|_| "Expression");
                    self.record(event.with_span(v.span()), msg)
                })
            }
            _ => self
                .if_expression(stream)
                .por(|ts| self.size_of(ts), stream)
                .por(|ts| self.while_expression(ts), stream)
                .por(|ts| self.expression_block(ts), stream)
                .por(|ts| self.function_call_or_variable(ts), stream)
                .por(|ts| self.constant(ts), stream)
                .por(|ts| self.array_expression(ts), stream),
        }
    }

    fn size_of(&self, stream: &mut TokenStream) -> ParserResult<Expression<ParserContext>> {
        // Check of size_of keyword
        match stream.next_if(&Lex::SizeOf) {
            Some(op) => {
                let (event, result) = self.new_event(Span::zero()).and_then(|| {
                    let ctx = op.to_ctx();
                    // Must have (
                    stream.next_must_be(&Lex::LParen)?;

                    // Read Type
                    let (ty, _) = self.consume_type(stream)?.ok_or(CompilerError::new(
                        ctx.span(),
                        ParserError::RawPointerExpectedType,
                    ))?;

                    // Must have )
                    let ctx = stream.next_must_be(&Lex::RParen)?.to_ctx().join(ctx);

                    // Return size_of expression
                    Ok(Some(Expression::SizeOf(ctx, Box::new(ty.clone()))))
                });
                result.view(|v| {
                    let msg = v.map(|_| "size_of");
                    self.record(event.with_span(v.span()), msg)
                })
            }
            None => Ok(None),
        }
    }

    pub(super) fn if_expression(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        match stream.next_if(&Lex::If) {
            Some(if_tok) => {
                let (event, result) = self.new_event(Span::zero()).and_then(|| {
                    stream.next_must_be(&Lex::LParen).and_then(|_| {
                        let cond = self.expression(stream)?.ok_or(CompilerError::new(
                            if_tok.span(),
                            ParserError::IfExpectedConditional,
                        ))?;
                        stream.next_must_be(&Lex::RParen)?;

                        let if_arm = self.expression_block(stream)?.ok_or(CompilerError::new(
                            if_tok.span(),
                            ParserError::IfTrueArmMissingExpr,
                        ))?;

                        // check for `else if`
                        let else_arm = match stream.next_if(&Lex::Else) {
                            Some(_) => match stream.peek() {
                                Some(Token {
                                    sym: Lex::If, span, ..
                                }) => {
                                    let span = *span;
                                    Some(self.if_expression(stream)?.ok_or(CompilerError::new(
                                        span,
                                        ParserError::IfElseExpectedIfExpr,
                                    ))?)
                                }
                                _ => {
                                    let false_arm = self.expression_block(stream)?.ok_or(
                                        CompilerError::new(
                                            if_tok.span(),
                                            ParserError::IfFalseArmMissingExpr,
                                        ),
                                    )?;
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
                });
                result.view(|v| {
                    let msg = v.map(|_| "If Expression");
                    self.record(event.with_span(v.span()), msg)
                })
            }
            _ => Ok(None),
        }
    }

    pub(super) fn while_expression(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        let (event, result) =
            self.new_event(Span::zero())
                .and_then(|| match stream.next_if(&Lex::While) {
                    Some(whl) => stream.next_must_be(&Lex::LParen).and_then(|_| {
                        let cond = self.expression(stream)?.ok_or(CompilerError::new(
                            whl.span(),
                            ParserError::WhileExpectedConditional,
                        ))?;
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
                    }),
                    _ => Ok(None),
                });
        result.view(|v| {
            let msg = v.map(|_| "While");
            self.record(event.with_span(v.span()), msg)
        })
    }

    pub(super) fn function_call_or_variable(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        let (event, result) =
            self.new_event(Span::zero())
                .and_then(|| match self.path(stream)? {
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
                });
        result.view(|v| {
            let msg = v.map(|v| match &v {
                Expression::Identifier(..) => "Identifier",
                Expression::Path(..) => "Path",
                Expression::StructExpression(..) => "Struct Expression",
                Expression::RoutineCall(..) => "Routine Call",
                _ => panic!("Unexpected Expression variant"),
            });
            self.record(event.with_span(v.span()), msg)
        })
    }

    pub(super) fn struct_expression_params(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<(Vec<(StringId, Expression<ParserContext>)>, ParserContext)> {
        let (event, result) =
            self.new_event(Span::zero())
                .and_then(|| match stream.next_if(&Lex::LBrace) {
                    Some(lbrace) => {
                        let mut params = vec![];
                        while let Some((field_name, span)) = stream.next_if_id() {
                            stream.next_must_be(&Lex::Colon)?;
                            let field_value =
                                self.expression(stream)?.ok_or(CompilerError::new(
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
                });
        result.view(|v| {
            let msg = v.map(|_| "Struct Exprssion Parameters");
            let span = match v {
                Ok(ok) => ok.1.span(),
                Err(err) => err.span(),
            };
            self.record(event.with_span(span), msg)
        })
    }

    pub(super) fn array_expression(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        match stream.next_if(&Lex::LBracket) {
            Some(lbracket) => {
                let (event, result) = self.new_event(Span::zero()).and_then(|| {
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
                });
                result.view(|v| {
                    let msg = v.map(|_| "Array Expression");
                    self.record(event.with_span(v.span()), msg)
                })
            }
            None => Ok(None),
        }
    }

    pub(super) fn constant(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        self.number(stream)
            .por(|ts| self.boolean_literal(ts), stream)
            .por(|ts| self.string_literal(ts), stream)
            .por(|ts| self.null_literal(ts), stream)
    }

    pub(super) fn number(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        let (event, result) = self.new_event(Span::zero()).and_then(|| {
            match stream.next_if_one_of(&[
                Lex::U8(0),
                Lex::U16(0),
                Lex::U32(0),
                Lex::U64(0),
                Lex::I8(0),
                Lex::I16(0),
                Lex::I32(0),
                Lex::I64(0),
                Lex::F64(0.0),
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
                Some(Token {
                    span,
                    sym: Lex::F64(i),
                    ..
                }) => Ok(Some(Expression::F64(ParserContext::new(span), i))),
                Some(t) => panic!("Unexpected token: {:?}", t),
                None => Ok(None),
            }
        });
        result.view(|v| {
            let msg = v.map(|_| "Number");
            self.record(event.with_span(v.span()), msg)
        })
    }

    pub(super) fn null_literal(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        let (event, result) =
            self.new_event(Span::zero())
                .and_then(|| match stream.next_if(&Lex::Null) {
                    Some(Token { span, .. }) => {
                        Ok(Some(Expression::Null(ParserContext::new(span))))
                    }
                    _ => Ok(None),
                });
        result.view(|v| {
            let msg = v.map(|_| "null");
            self.record(event.with_span(v.span()), msg)
        })
    }

    pub(super) fn boolean_literal(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        let (event, result) =
            self.new_event(Span::zero())
                .and_then(|| match stream.next_if(&Lex::Bool(true)) {
                    Some(Token {
                        span,
                        sym: Lex::Bool(b),
                        ..
                    }) => Ok(Some(Expression::Boolean(ParserContext::new(span), b))),
                    _ => Ok(None),
                });
        result.view(|v| {
            let msg = v.map(|_| "Boolean");
            self.record(event.with_span(v.span()), msg)
        })
    }

    pub(super) fn string_literal(
        &self,
        stream: &mut TokenStream,
    ) -> ParserResult<Expression<ParserContext>> {
        let (event, result) = self.new_event(Span::zero()).and_then(|| {
            match stream.next_if(&Lex::StringLiteral(StringId::new())) {
                Some(Token {
                    span,
                    sym: Lex::StringLiteral(s),
                    ..
                }) => Ok(Some(Expression::StringLiteral(ParserContext::new(span), s))),
                _ => Ok(None),
            }
        });
        result.view(|v| {
            let msg = v.map(|_| "String");
            self.record(event.with_span(v.span()), msg)
        })
    }
}
