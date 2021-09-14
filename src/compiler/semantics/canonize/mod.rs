mod canonize;
mod foreach_mut;

pub use canonize::*;

use crate::compiler::CompilerError;

use super::SemanticError;

type CanonizeResult<T> = Result<T, CompilerError<SemanticError>>;
