//! Converts the Bramble AST to the CFG MIR representation used for
//! dataflow analyses; such as, lifetime checking, variable initialization,
//! consistency rules checking, and so on.

// Transformer
// This process takes the AST for a compilation unit and transforms it into the
// CFG MIR used for dataflow analysis and LLVM IR generation by the Bramble
// compiler.

use log::debug;

use crate::compiler::{ast::*, mir::ir::Procedure, semantics::semanticnode::SemanticContext};

use super::{super::project::MirProject, function::FuncTransformer, TransformError};

/// Transform a [`Module`] into its MIR representation and add all items to the
/// given [`MirProject`].
pub fn transform(
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

    let funcs: Vec<_> = module
        .get_functions()
        .iter()
        .filter_map(|f| {
            if let Item::Routine(r) = f {
                Some(r)
            } else {
                None
            }
        })
        .collect();

    // Add function declarations so that function calls can safely look up
    // the correct DefId
    add_fn_declarations(project, &funcs)?;

    // Iterate through each function an construct its MIR and then update
    // it's static definition with the MIR
    transform_fns(project, &funcs)?;

    Ok(())
}

fn add_fn_declarations(
    project: &mut MirProject,
    funcs: &[&RoutineDef<SemanticContext>],
) -> Result<(), TransformError> {
    for f in funcs {
        let p = Procedure::new(
            f.context().canonical_path(),
            f.context().ty(),
            f.context().span(),
        );
        project.add_func(p)?;
    }

    Ok(())
}

fn transform_fns(
    project: &mut MirProject,
    funcs: &[&RoutineDef<SemanticContext>],
) -> Result<(), TransformError> {
    for f in funcs {
        let ft = FuncTransformer::new(f.context().canonical_path(), project);
        let p = ft.transform(f);
        project.add_func(p)?;
    }

    Ok(())
}
