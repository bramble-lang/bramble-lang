use super::{ast::*, tokenstream::TokenStream};
use crate::lexer::tokens::{Lex, Token};

pub type ParserInfo = u32;
pub type PResult = Result<Option<PNode>, String>;

impl Fluent for PResult {
    fn por(&self, f: fn(&mut TokenStream) -> PResult, ts: &mut TokenStream) -> PResult {
        match self {
            Ok(Some(s)) => Ok(Some(s.clone())),
            Ok(None) => f(ts),
            Err(e) => Err(e.clone()),
        }
    }

    fn pif_then(
        &self,
        cond: Vec<Lex>,
        then: fn(PNode, Token, &mut TokenStream) -> PResult,
        ts: &mut TokenStream,
    ) -> PResult {
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

pub trait Fluent {
    fn por(&self, f: fn(&mut TokenStream) -> PResult, ts: &mut TokenStream) -> PResult;
    fn pif_then(
        &self,
        cond: Vec<Lex>,
        f: fn(PNode, Token, &mut TokenStream) -> PResult,
        ts: &mut TokenStream,
    ) -> PResult;
}

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
    ) -> PResult {
        let i = line; //ParserInfo{l: line};
        match op {
            Lex::Eq => Ok(Some(Ast::BinaryOp(i, BinaryOperator::Eq, left, right))),
            Lex::NEq => Ok(Some(Ast::BinaryOp(i, BinaryOperator::NEq, left, right))),
            Lex::Ls => Ok(Some(Ast::BinaryOp(i, BinaryOperator::Ls, left, right))),
            Lex::LsEq => Ok(Some(Ast::BinaryOp(i, BinaryOperator::LsEq, left, right))),
            Lex::Gr => Ok(Some(Ast::BinaryOp(i, BinaryOperator::Gr, left, right))),
            Lex::GrEq => Ok(Some(Ast::BinaryOp(i, BinaryOperator::GrEq, left, right))),
            Lex::BAnd => Ok(Some(Ast::BinaryOp(i, BinaryOperator::BAnd, left, right))),
            Lex::BOr => Ok(Some(Ast::BinaryOp(i, BinaryOperator::BOr, left, right))),
            Lex::Add => Ok(Some(Ast::BinaryOp(i, BinaryOperator::Add, left, right))),
            Lex::Minus => Ok(Some(Ast::BinaryOp(i, BinaryOperator::Sub, left, right))),
            Lex::Mul => Ok(Some(Ast::BinaryOp(i, BinaryOperator::Mul, left, right))),
            Lex::Div => Ok(Some(Ast::BinaryOp(i, BinaryOperator::Div, left, right))),
            _ => Err(format!("L{}: {} is not a binary operator", line, op)),
        }
    }
}
