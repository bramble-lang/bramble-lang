//! Converts the Bramble AST to the CFG MIR representation used for
//! dataflow analyses; such as, lifetime checking, variable initialization,
//! consistency rules checking, and so on.

// Transformer
// This process takes the AST for a compilation unit and transforms it into the
// CFG MIR used for dataflow analysis and LLVM IR generation by the Bramble
// compiler.

use log::debug;

use crate::{
    compiler::{
        ast::*,
        semantics::semanticnode::SemanticContext,
    },
};

use super::{
    super::{
        project::MirProject,
    },
    TransformError, function::FuncTransformer,
};

/// Transform a [`Module`] into its MIR representation and add all items to the
/// given [`MirProject`].
pub fn module_transform(
    module: &Module<SemanticContext>,
    project: &mut MirProject,
) -> Result<(), TransformError> {
    debug!("Transform module: {:?}", module.context().canonical_path());

    // Add all the types in this module
    for sd in module.get_structs() {
        if let Item::Struct(sd) = sd {
            project.add_struct_def(sd)?;
        }
    }

    let funcs = module.get_functions();

    for f in funcs {
        match f {
            crate::compiler::ast::Item::Routine(r) => {
                let ft = FuncTransformer::new(r.context().canonical_path(), project);
                let p = ft.transform(r);
                project.add_func(p)?;
            }
            crate::compiler::ast::Item::Struct(_) => todo!(),
            crate::compiler::ast::Item::Extern(_) => todo!(),
        }
    }

    Ok(())
}