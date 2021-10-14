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

    /// Creates a new ParserContext with a Span that covers this span and the
    /// span in `a`.
    pub fn join(&self, b: ParserContext) -> ParserContext {
        let line = self.line.min(b.line);
        let span = Span::cover(self.span, b.span);
        ParserContext::new(line, span)
    }

    /// Creates anew ParserContext which has a span that's extended to cover
    /// the given Span.
    pub fn extend(&self, b: Span) -> ParserContext {
        ParserContext::new(self.line, Span::cover(self.span, b))
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
