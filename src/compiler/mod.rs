// Modules that are needed only within the compiler
mod error; // Contains a macro and therefore this must be imported before all other modules
mod source;

// Modules which should be accessible outside of  the [`compiler`] module
pub mod ast;
pub mod diagnostics;
pub mod import;
pub mod lexer;
pub mod llvm;
mod mir;
pub mod parser;
pub mod semantics;
pub mod stringtable;

// Expose certain compiler items outside of the module because they are key parts
// of the interface between the compiler and modules which use the compiler.
pub use error::CompilerError;
pub use lexer::lexer::Lexer;
pub use mir::{transform, MirProject, ProgramTraverser};
pub use source::{Source, SourceCharIter, SourceError, SourceMap, SourceMapError, Span};

// Import items for use within the compiler submodule which are not needed outside
use source::SourceChar;

use crate::{StringTable, StringTableError};

/**
Format trait for rendering any Compiler value into a human readable form.
Specifically, this will handle converting any [`StringId`]s into their associated
string value.
*/
pub trait CompilerDisplay {
    /// Uses the given [`StringTable`] to render the associated Compiler type into a
    /// human readable format.
    fn fmt(&self, sm: &SourceMap, st: &StringTable) -> Result<String, CompilerDisplayError>;
}

/// Error that gets thrown if formatting a Compiler value for human readability
/// fails.
#[derive(Debug)]
pub enum CompilerDisplayError {
    StringIdNotFound,
    SourceError(SourceError),
}

impl From<StringTableError> for CompilerDisplayError {
    fn from(ste: StringTableError) -> Self {
        match ste {
            StringTableError::NotFound => Self::StringIdNotFound,
        }
    }
}
