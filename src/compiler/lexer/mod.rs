use super::CompilerError;

mod tests;

mod error;
pub(crate) mod lexer;
pub(crate) mod tokens;

pub use error::*;

type LexerResult<T> = Result<T, CompilerError<LexerError>>;
