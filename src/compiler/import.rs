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
    pub funcs: Vec<(Path, Vec<Type>, Type)>,
}
