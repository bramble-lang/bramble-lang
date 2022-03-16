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
    add_struct_defs_to_typetable(project, module)?;
    add_types_to_typetable(project, module)?;
    add_extern_declarations(project, module)?;
    add_fn_declarations(project, module)?;

    // Lower the AST to its MIR form
    transform_fns(project, module)?;

    Ok(())
}

fn add_types_to_typetable(
    project: &mut MirProject,
    module: &Module<SemanticContext>,
) -> Result<(), TransformError> {
    PostOrderIter::new(module)
        .map(|n| project.add_type(n.context().ty()))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(())
}

fn add_struct_defs_to_typetable(
    project: &mut MirProject,
    module: &Module<SemanticContext>,
) -> Result<(), TransformError> {
    for sd in module.get_structs() {
        if let Item::Struct(sd) = sd {
            project.add_struct_def(sd)?;
        }
    }

    Ok(())
}

fn add_extern_declarations(
    project: &mut MirProject,
    module: &Module<SemanticContext>,
) -> Result<(), TransformError> {
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

    for e in externs {
        // convert args into MIR args
        let args: Vec<_> = e
            .params
            .iter()
            .map(|p| {
                let ty = project.find_type(p.context().ty()).unwrap_or_else(|| {
                    panic!("Cannot find type in project for parameter: {:?}", p)
                });
                ArgDecl::new(p.name, ty, p.context().span())
            })
            .collect();

        let ret_ty = project
            .find_type(e.get_return_type())
            .expect("Cannot find return type");

        let p = Procedure::new_extern(
            e.context().canonical_path(),
            args,
            e.has_varargs,
            ret_ty,
            e.context().span(),
        );
        project.add_func(p)?;
    }

    Ok(())
}

fn add_fn_declarations(
    project: &mut MirProject,
    module: &Module<SemanticContext>,
) -> Result<(), TransformError> {
    let funcs = module.get_functions().iter().filter_map(|f| {
        if let Item::Routine(r) = f {
            Some(r)
        } else {
            None
        }
    });

    for f in funcs {
        // convert args into MIR args
        let args: Vec<_> = f
            .params
            .iter()
            .map(|p| {
                let ty = project
                    .find_type(p.context().ty())
                    .expect("Cannot find type in Project");
                ArgDecl::new(p.name, ty, p.context().span())
            })
            .collect();

        let ret_ty = project
            .find_type(f.context().ty())
            .expect("Cannot find return type");

        let p = Procedure::new(
            f.context().canonical_path(),
            args,
            ret_ty,
            f.context().span(),
        );
        project.add_func(p)?;
    }

    Ok(())
}

fn transform_fns(
    project: &mut MirProject,
    module: &Module<SemanticContext>,
) -> Result<(), TransformError> {
    let funcs = module.get_functions().iter().filter_map(|f| {
        if let Item::Routine(r) = f {
            Some(r)
        } else {
            None
        }
    });

    for f in funcs {
        let ft = FuncTransformer::new(f.context().canonical_path(), project);
        let p = ft.transform(f);
        project.add_func(p)?;
    }

    Ok(())
}
