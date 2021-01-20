use super::{parser::{ParserInfo, ParserResult}, tokenstream::TokenStream};
use crate::{
    lexer::tokens::{Lex, Token},
    syntax::expression::{BinaryOperator, Expression, UnaryOperator},
};

impl ParserCombinator<ParserResult<Expression<ParserInfo>>> for ParserResult<Expression<ParserInfo>> {
    fn por(&self, f: fn(&mut TokenStream) -> ParserResult<Expression<ParserInfo>>, ts: &mut TokenStream) -> ParserResult<Expression<ParserInfo>> {
        match self {
            Ok(Some(s)) => Ok(Some(s.clone())),
            Ok(None) => f(ts),
            Err(e) => Err(e.clone()),
        }
    }

    fn pif_then(
        &self,
        cond: Vec<Lex>,
        then: fn(Expression<ParserInfo>, Token, &mut TokenStream) -> ParserResult<Expression<ParserInfo>>,
        ts: &mut TokenStream,
    ) -> ParserResult<Expression<ParserInfo>> {
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
        f: fn(Expression<ParserInfo>, Token, &mut TokenStream) -> R,
        ts: &mut TokenStream,
    ) -> R;
}

impl Expression<ParserInfo> {
    pub fn new_yield(line: u32, coroutine_value: Box<Self>) -> ParserResult<Expression<ParserInfo>> {
        let i = line; //ParserInfo{l: line};
        Ok(Some(Expression::Yield(i, coroutine_value)))
    }

    pub fn unary_op(line: u32, op: &Lex, operand: Box<Self>) -> ParserResult<Expression<ParserInfo>> {
        match op {
            Lex::Minus => Ok(Some(Expression::UnaryOp(
                line,
                UnaryOperator::Minus,
                operand,
            ))),
            Lex::Not => Ok(Some(Expression::UnaryOp(line, UnaryOperator::Not, operand))),
            _ => Err(format!("L{}: {} is not a unary operator", line, op)),
        }
    }

    pub fn binary_op(line: u32, op: &Lex, left: Box<Self>, right: Box<Self>) -> ParserResult<Expression<ParserInfo>> {
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
