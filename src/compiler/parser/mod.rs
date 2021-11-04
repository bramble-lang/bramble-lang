mod context;
mod error;
mod statement;
mod tests;
mod tokenstream;

pub mod expression;
pub mod parser;

pub use context::ParserContext;
pub use error::ParserError;

use super::{diagnostics::Logger, lexer::tokens::Token, CompilerError};

type ParserResult<T> = Result<Option<T>, CompilerError<ParserError>>;

/// Compute the minimum span that covers all the tokens in the slice.
fn ctx_over_tokens(tokens: &[Token]) -> Option<ParserContext> {
    // The vector of tokens is assumed to be ordered by their Offsets and that no
    // two tokens have intersecting spans.
    tokens
        .first()
        .and_then(|f| tokens.last().map(|l| f.to_ctx().join(l.to_ctx())))
}

pub struct Parser<'a> {
    logger: &'a Logger<'a>,
}
