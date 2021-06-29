use serde::{Deserialize, Serialize};

use crate::{
    ast::{Module, Node, Path, StructDef, Type},
    semantics::semanticnode::SemanticAnnotations,
};

#[derive(Serialize, Deserialize, Debug)]
struct Routine {
    path: Path,
    params: Vec<Type>,
    ty: Type,
}

impl Routine {
    fn from_ast(r: &crate::ast::Item<SemanticAnnotations>) -> Routine {
        Routine {
            path: r.annotation().get_canonical_path().clone(),
            params: r
                .to_routine()
                .expect(&format!(
                    "Unexpected item type: a non-function was returned by deep_get_functions: {}",
                    r.get_name(),
                ))
                .params
                .iter()
                .map(|p| p.ty.clone())
                .collect(),
            ty: r.annotation().ty().clone(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Manifest {
    routines: Vec<Routine>,
    structs: Vec<StructDef<SemanticAnnotations>>,
}

impl Manifest {
    pub fn extract(module: &Module<SemanticAnnotations>) -> Manifest {
        // Get list of all functions contained within a module and their paths
        let funcs = module.deep_get_functions();
        let items = funcs.iter().map(|f| Routine::from_ast(f)).collect();

        // Get list of all structures contained within the module
        let structs = module
            .deep_get_structs()
            .into_iter()
            .map(|s| s.clone())
            .collect();

        // Create the manifest
        Manifest {
            routines: items,
            structs,
        }
    }

    pub fn get_functions(&self) -> Vec<(Path, Vec<Type>, Type)> {
        self.routines
            .iter()
            .map(|i| (i.path.clone(), i.params.clone(), i.ty.clone()))
            .collect()
    }

    pub fn get_structs(&self) -> &Vec<StructDef<SemanticAnnotations>> {
        &self.structs
    }

    /// Loads a manifest from the given file.
    pub fn read(file: &mut std::fs::File) -> Result<Manifest, serde_yaml::Error> {
        let manifest: Manifest = serde_yaml::from_reader(file)?;
        Ok(manifest)
    }

    /// Writes the Manifest to the given file
    pub fn write(&self, file: &mut std::fs::File) -> Result<(), serde_yaml::Error> {
        // Write manifest to file
        serde_yaml::to_writer(file, self)
    }
}
