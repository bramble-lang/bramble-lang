//! Items and functions for helping translate imported items into LLVM IR

use crate::compiler::{
    ast::{Parameter, StructDef, Type},
    import::ImportStructDef,
    parser::ParserContext,
    semantics::semanticnode::SemanticContext,
    Span,
};

/// Convert an Imported structure definition to the internal compilre structure
/// definition.  This is needed by the LLVM layer so that it can store a table
/// of all structure definitions.
impl From<&ImportStructDef> for StructDef<SemanticContext> {
    fn from(isd: &ImportStructDef) -> Self {
        let name = isd.path().item().unwrap();

        let struct_def_ctx = SemanticContext::new_local(
            0,
            ParserContext::new(0, Span::zero()),
            Type::StructDef(isd.fields().into()),
        );

        let fields = isd
            .fields()
            .iter()
            .map(|(name, ty)| {
                Parameter::new(
                    SemanticContext::new_local(0, ParserContext::new(0, Span::zero()), ty.clone()),
                    *name,
                    ty,
                )
            })
            .collect();

        StructDef::new(name, struct_def_ctx, fields)
    }
}
