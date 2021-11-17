use crate::{
    compiler::{ast::Context, lexer::tokens::Token, source::SourceIr, Span},
    diagnostics::{Diag, DiagData},
};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ParserContext {
    span: Span,
}

impl ParserContext {
    pub fn new(line: u32, span: Span) -> ParserContext {
        ParserContext { span }
    }

    /// Joins this [`ParserContext`] with `a`, such that the span will cover
    /// both contexts.
    pub fn join(self, b: ParserContext) -> ParserContext {
        let span = Span::cover(self.span, b.span);
        ParserContext::new(0, span)
    }

    /// Extends the span of this [`ParserContext`] such that it also covers the
    /// span described by `b`.
    pub fn extend(self, b: Span) -> ParserContext {
        ParserContext::new(0, Span::cover(self.span, b))
    }
}

impl Context for ParserContext {
    fn id(&self) -> u32 {
        0
    }

    fn line(&self) -> u32 {
        0
    }

    fn span(&self) -> Span {
        self.span
    }
}

impl Diag for ParserContext {
    fn diag(&self) -> DiagData {
        DiagData::new(0, 0)
    }
}

impl Token {
    pub fn to_ctx(&self) -> ParserContext {
        ParserContext { span: self.span() }
    }
}
