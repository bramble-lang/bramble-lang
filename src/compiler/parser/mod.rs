mod error;
mod statement;
mod tests;
mod tokenstream;

pub mod expression;
pub mod parser;

pub use error::ParserError;

use crate::diagnostics::{Diag, DiagData};

use super::{ast::Context, lexer::tokens::Token, CompilerError, Span};

type ParserResult<T> = Result<Option<T>, CompilerError<ParserError>>;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ParserContext {
    span: Span,
    line: u32,
}

impl ParserContext {
    pub fn new(line: u32, span: Span) -> ParserContext {
        ParserContext { line, span }
    }
}

impl Context for ParserContext {
    fn id(&self) -> u32 {
        0
    }

    fn line(&self) -> u32 {
        self.line
    }
}

impl Diag for ParserContext {
    fn diag(&self) -> DiagData {
        DiagData::new(self.line, 0)
    }
}

impl Token {
    fn to_ctx(&self) -> ParserContext {
        ParserContext {
            line: self.l,
            span: self.span,
        }
    }
}
