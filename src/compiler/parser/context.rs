use crate::{
    compiler::{ast::Context, lexer::tokens::Token, source::SourceIr, Span},
    diagnostics::{Diag, DiagData},
};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ParserContext {
    span: Span,
    line: u32,
}

impl ParserContext {
    pub fn new(line: u32, span: Span) -> ParserContext {
        ParserContext { line, span }
    }

    /// Joins this [`ParserContext`] with `a`, such that the span will cover
    /// both contexts.
    pub fn join(self, b: ParserContext) -> ParserContext {
        let line = self.line.min(b.line);
        let span = Span::cover(self.span, b.span);
        ParserContext::new(line, span)
    }

    /// Extends the span of this [`ParserContext`] such that it also covers the
    /// span described by `b`.
    pub fn extend(self, b: Span) -> ParserContext {
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

    fn span(&self) -> Span {
        self.span
    }
}

impl Diag for ParserContext {
    fn diag(&self) -> DiagData {
        DiagData::new(self.line, 0)
    }
}

impl Token {
    pub fn to_ctx(&self) -> ParserContext {
        ParserContext {
            line: 0,
            span: self.span(),
        }
    }
}
