#![macro_use]
use crate::StringTable;

use super::{CompilerDisplay, CompilerDisplayError};

/// Represents all errors that are generated from within the Compiler
/// module and its submodules.
///
/// This type captures common metadata which is necessarily present for
/// all errors which are caused by input source code.  E.g. the line #
/// that the error occurs on. This also handles formatting all error messages
/// with the universal metadata along with the inner metadata.
///
/// The inner error allows metadata which is specific to a submodule within
/// the compiler. E.g., the errors themselves are submodule specific and
/// are stored in the `inner` field.
#[derive(Clone, Debug, PartialEq)]
pub struct CompilerError<IE: CompilerDisplay> {
    /// The line in the source code which caused the error
    line: u32,

    /// An inner error value that contains more specific error information
    inner: IE,
}

impl<IE> CompilerError<IE>
where
    IE: CompilerDisplay,
{
    pub fn new(line: u32, inner: IE) -> Self {
        CompilerError { line, inner }
    }

    /// Moves the Line number and Inner error out of the wrapping [CompilerError].
    /// This is to allow conversion of one type of [CompilerError] to another type, by
    /// converting the `inner` type into a new type and then creating a new [CompilerError]
    /// wrapper.
    pub fn take(self) -> (u32, IE) {
        (self.line, self.inner)
    }
}

impl<IE> CompilerDisplay for CompilerError<IE>
where
    IE: CompilerDisplay,
{
    fn fmt(&self, st: &StringTable) -> Result<String, CompilerDisplayError> {
        let inner = self.inner.fmt(st)?;
        Ok(format!("L{}: {}", self.line, inner))
    }
}

/// Helper macro to get rid of repitition of boilerplate code.
//#[macro_export]
macro_rules! err {
    ($ln: expr, $kind: expr) => {
        Err(CompilerError::new($ln, $kind))
    };
}
