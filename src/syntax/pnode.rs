use crate::lexer::tokens::{Lex};
use super::ast::*;

pub type ParserInfo = u32;
pub type PResult = Result<Option<PNode>, String>;

pub type PNode = Ast<ParserInfo>;

impl PNode {
    pub fn new_yield(line: u32, coroutine_value: Box<Self>) -> Self {
        let i = line; //ParserInfo{l: line};
        Ast::Yield(i, coroutine_value)
    }

    pub fn new_bind(
        line: u32,
        id: Box<Self>,
        mutable: bool,
        exp: Box<Self>,
    ) -> Result<Self, String> {
        let i = line;
        match id.as_ref() {
            Ast::IdentifierDeclare(_, id, prim) => {
                Ok(Ast::Bind(i, id.clone(), mutable, prim.clone(), exp))
            }
            _ => Err(format!(
                "L{}: Expected type specification after {}",
                line,
                id.root_str()
            )),
        }
    }

    pub fn new_mutate(line: u32, id: &str, exp: Box<Self>) -> Result<Self, String> {
        Ok(Ast::Mutate(line, id.into(), exp))
    }

    pub fn unary_op(line: u32, op: &Lex, operand: Box<Self>) -> Result<Self, String> {
        match op {
            Lex::Minus => Ok(Ast::UnaryOp(line, UnaryOperator::Minus, operand)),
            Lex::Not => Ok(Ast::UnaryOp(line, UnaryOperator::Not, operand)),
            _ => Err(format!("L{}: {} is not a unary operator", line, op)),
        }
    }

    pub fn binary_op(
        line: u32,
        op: &Lex,
        left: Box<Self>,
        right: Box<Self>,
    ) -> Result<Self, String> {
        let i = line; //ParserInfo{l: line};
        match op {
            Lex::Eq => Ok(Ast::BinaryOp(i, BinaryOperator::Eq, left, right)),
            Lex::NEq => Ok(Ast::BinaryOp(i, BinaryOperator::NEq, left, right)),
            Lex::Ls => Ok(Ast::BinaryOp(i, BinaryOperator::Ls, left, right)),
            Lex::LsEq => Ok(Ast::BinaryOp(i, BinaryOperator::LsEq, left, right)),
            Lex::Gr => Ok(Ast::BinaryOp(i, BinaryOperator::Gr, left, right)),
            Lex::GrEq => Ok(Ast::BinaryOp(i, BinaryOperator::GrEq, left, right)),
            Lex::BAnd => Ok(Ast::BinaryOp(i, BinaryOperator::BAnd, left, right)),
            Lex::BOr => Ok(Ast::BinaryOp(i, BinaryOperator::BOr, left, right)),
            Lex::Add => Ok(Ast::BinaryOp(i, BinaryOperator::Add, left, right)),
            Lex::Minus => Ok(Ast::BinaryOp(i, BinaryOperator::Sub, left, right)),
            Lex::Mul => Ok(Ast::BinaryOp(i, BinaryOperator::Mul, left, right)),
            Lex::Div => Ok(Ast::BinaryOp(i, BinaryOperator::Div, left, right)),
            _ => Err(format!("L{}: {} is not a binary operator", line, op)),
        }
    }
}