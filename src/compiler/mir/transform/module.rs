//! Converts the Bramble AST to the CFG MIR representation used for
//! dataflow analyses; such as, lifetime checking, variable initialization,
//! consistency rules checking, and so on.

// Transformer
// This process takes the AST for a compilation unit and transforms it into the
// CFG MIR used for dataflow analysis and LLVM IR generation by the Bramble
// compiler.

use log::debug;

use crate::compiler::{
    ast::*,
    mir::ir::{ArgDecl, Procedure},
    semantics::semanticnode::SemanticContext,
};

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

    let externs: Vec<_> = module
        .get_externs()
        .iter()
        .filter_map(|e| {
            if let Item::Extern(e) = e {
                Some(e)
            } else {
                None
            }
        })
        .collect();

    add_extern_declarations(project, &externs)?;

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
    // its static definition with the MIR
    transform_fns(project, &funcs)?;

    Ok(())
}

fn add_extern_declarations(
    project: &mut MirProject,
    externs: &[&Extern<SemanticContext>],
) -> Result<(), TransformError> {
    for e in externs {
        // convert args into MIR args
        let args: Vec<_> = e
            .params
            .iter()
            .map(|p| ArgDecl::new(p.name, p.context().ty(), p.context().span()))
            .collect();

        let p = Procedure::new_extern(
            e.context().canonical_path(),
            args,
            e.has_varargs,
            e.get_return_type(),
            e.context().span(),
        );
        project.add_func(p)?;
    }

    Ok(())
}

fn add_fn_declarations(
    project: &mut MirProject,
    funcs: &[&RoutineDef<SemanticContext>],
) -> Result<(), TransformError> {
    for f in funcs {
        // convert args into MIR args
        let args: Vec<_> = f
            .params
            .iter()
            .map(|p| ArgDecl::new(p.name, p.context().ty(), p.context().span()))
            .collect();

        let p = Procedure::new(
            f.context().canonical_path(),
            args,
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
