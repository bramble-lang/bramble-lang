// Contains a macro and therefore this must be imported before all other modules
mod error;

pub mod ast;
pub mod import;
pub mod lexer;
pub mod llvm;
pub mod parser;
pub mod semantics;
pub mod stringtable;

pub use error::CompilerError;

pub use lexer::lexer::Lexer;

use crate::{StringTable, StringTableError};

/**
Format trait for rendering any Compiler value into a human readable form.
Specifically, this will handle converting any [`StringId`]s into their associated
string value.
*/
pub trait CompilerDisplay {
    /// Uses the given [`StringTable`] to render the associated Compiler type into a
    /// human readable format.
    fn fmt(&self, st: &StringTable) -> Result<String, CompilerDisplayError>;
}

/// Error that gets thrown if formatting a Compiler value for human readability
/// fails.
#[derive(Debug)]
pub enum CompilerDisplayError {
    StringIdNotFound,
}

impl From<StringTableError> for CompilerDisplayError {
    fn from(ste: StringTableError) -> Self {
        match ste {
            StringTableError::NotFound => Self::StringIdNotFound,
        }
    }
}

/// Represents a single char from a source code file.  This includes the character
/// and the global offset of the character (which points to the specific source
/// code file and location within that file of this character)
struct SourceChar {
    c: char,
}

impl SourceChar {
    pub fn new(c: char) -> SourceChar {
        SourceChar { c }
    }

    pub fn char(&self) -> char {
        self.c
    }
}
