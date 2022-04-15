use crate::StringId;

use super::ast::{Path, Type};

/// Items which are imported from external libraries, projects, or modules.
/// These definitions come from the manifests generated when those external
/// artifacts were compiled.
///
/// This value allows the compiler to semantically check that external items
/// are used correctly.
pub struct Import {
    /// Structs imported from an external artifact
    pub structs: Vec<ImportStructDef>,

    /// Functions imported from an external artifact
    pub funcs: Vec<ImportRoutineDef>,
}

/// Describes the parameter list and the return type of a function defined
/// in an external module or library.
pub struct ImportRoutineDef {
    /// The canonical path of this routine within it's host module
    path: Path,

    /// The parameter list of this routine
    params: Vec<(StringId, Type)>,

    /// The type that this routine resolves to
    ty: Type,
}

impl ImportRoutineDef {
    pub fn new(path: Path, params: Vec<(StringId, Type)>, ty: Type) -> ImportRoutineDef {
        ImportRoutineDef { path, params, ty }
    }

    /// The canonical path of this routine within it's host module
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// The parameter list of this routine
    pub fn params(&self) -> &[(StringId, Type)] {
        &self.params
    }

    /// The type that this routine resolves to
    pub fn ty(&self) -> &Type {
        &self.ty
    }
}

/// A structure which is imported from an external module or library
pub struct ImportStructDef {
    /// The canonical path of this structure within it's host module
    path: Path,

    /// The field list of this structure
    fields: Vec<(StringId, Type)>,
}

impl ImportStructDef {
    pub fn new(path: Path, fields: Vec<(StringId, Type)>) -> ImportStructDef {
        ImportStructDef { path, fields }
    }

    /// The canonical path of this structure within it's host module
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// The field list of this structure
    pub fn fields(&self) -> &[(StringId, Type)] {
        &self.fields
    }
}
