pub mod manifest;
pub mod project;

pub use manifest::Manifest;
pub use project::*;

use crate::{
    compiler::{ast::Type, CompilerDisplay, CompilerDisplayError},
    StringTableError,
};

/// Errors that can occur when writing or reading a Manifest file.
#[derive(Debug)]
pub enum ManifestError {
    /// When attempting to write a Compiler type to a manifest file and a [`StringId`] member
    /// cannot be found in the [`StringTable`].
    StringIdNotFound,

    /// Some types ought not be written to a Manifest: this error is thrown when one of those
    /// types is written.
    CannotConvertType(Type),

    /// Error that is generated if a given string cannot be parsed into a valid [`Path`]
    ParsePathError,
}

impl CompilerDisplay for ManifestError {
    fn fmt(&self, st: &crate::StringTable) -> Result<String, CompilerDisplayError> {
        Ok(match self {
            ManifestError::StringIdNotFound => {
                format!("Could not write Manifest file: StringId was not found in StringTable")
            }
            ManifestError::CannotConvertType(ty) => format!("Cannot convert type: {}", ty.fmt(st)?),
            ManifestError::ParsePathError => {
                format!("A Path contained in the Manifest file was invalid")
            }
        })
    }
}

impl From<CompilerDisplayError> for ManifestError {
    fn from(cde: CompilerDisplayError) -> Self {
        match cde {
            CompilerDisplayError::StringIdNotFound => Self::StringIdNotFound,
        }
    }
}

impl From<StringTableError> for ManifestError {
    fn from(ste: StringTableError) -> Self {
        match ste {
            StringTableError::NotFound => Self::StringIdNotFound,
        }
    }
}
