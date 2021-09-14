use crate::StringTable;

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
pub struct CompilerError<IE: CompilerErrorDisplay> {
    line: u32,
    inner: IE,
}

impl<IE> CompilerError<IE>
where
    IE: CompilerErrorDisplay,
{
    pub fn new(line: u32, inner: IE) -> Self {
        CompilerError { line, inner }
    }

    pub fn inner(self) -> IE {
        self.inner
    }

    pub fn line(&self) -> u32 {
        self.line
    }
}

impl<IE> CompilerErrorDisplay for CompilerError<IE>
where
    IE: CompilerErrorDisplay,
{
    fn format(&self, st: &StringTable) -> Result<String, String> {
        let inner = self.inner.format(st)?;
        Ok(format!("L{}: {}", self.line, inner))
    }
}

pub trait CompilerErrorDisplay {
    fn format(&self, st: &StringTable) -> Result<String, String>;
}
