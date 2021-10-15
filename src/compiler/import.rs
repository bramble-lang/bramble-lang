use super::{
    ast::{Path, StructDef, Type},
    semantics::semanticnode::SemanticContext,
};

/// Items which are imported from external libraries, projects, or modules.
/// These definitions come from the manifests generated when those external
/// artifacts were compiled.
///
/// This value allows the compiler to semantically check that external items
/// are used correctly.
pub struct Import {
    /// Structs imported from an external artifact
    pub structs: Vec<StructDef<SemanticContext>>,

    /// Functions imported from an external artifact
    pub funcs: Vec<ImportRoutineDef>,
}

/// Describes the parameter list and the return type of a function defined
/// in an external module or library.
pub struct ImportRoutineDef {
    /// The canonical path of the routine within it's host module
    pub path: Path,

    /// The parameter list of the routine
    pub params: Vec<Type>,

    /// The type that the routine resolves to
    pub ty: Type,
}

impl ImportRoutineDef {
    pub fn new(path: Path, params: Vec<Type>, ty: Type) -> ImportRoutineDef {
        ImportRoutineDef { path, params, ty }
    }
}
