mod context;
mod error;
mod statement;
mod tests;
mod tokenstream;

pub mod expression;
pub mod parser;

pub use context::ParserContext;
pub use error::ParserError;

use super::CompilerError;

type ParserResult<T> = Result<Option<T>, CompilerError<ParserError>>;
