pub mod ast;
mod error;
pub mod import;
pub mod lexer;
pub mod llvm;
pub mod parser;
pub mod semantics;

pub use error::CompilerError;

pub use lexer::lexer::Lexer;

use crate::StringTable;

/**
Format trait for rendering any Compiler value into a human readable form.
Specifically, this will handle converting any [`StringId`]s into their associated
string value.
*/
pub trait CompilerDisplay {
    /// Uses the given [`StringTable`] to render the associated Compiler type into a
    /// human readable format.
    fn fmt(&self, st: &StringTable) -> Result<String, String>;
}
