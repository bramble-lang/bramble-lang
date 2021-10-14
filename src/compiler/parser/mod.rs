mod error;
mod statement;
mod tests;
mod tokenstream;

pub mod expression;
pub mod parser;

pub use error::ParserError;

use super::CompilerError;

type ParserResult<T> = Result<Option<T>, CompilerError<ParserError>>;

pub type ParserContext = u32;
