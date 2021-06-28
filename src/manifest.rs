use serde::{Deserialize, Serialize};

use crate::{
    ast::{Module, Node, Path, Type},
    semantics::semanticnode::SemanticAnnotations,
};

#[derive(Serialize, Deserialize, Debug)]
struct Item {
    path: Path,
    params: Vec<Type>,
    ty: Type,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Manifest {
    items: Vec<Item>,
}

impl Manifest {
    pub fn from_module(module: &Module<SemanticAnnotations>) -> Manifest {
        // Get list of all functions contained within a module and their paths
        let funcs = module.deep_get_functions();

        // map to a Vector of manifest items
        let items = funcs
            .iter()
            .map(|f| Item {
                path: f.annotation().get_canonical_path().clone(),
                params: f
                    .to_routine()
                    .unwrap()
                    .params
                    .iter()
                    .map(|p| p.ty.clone())
                    .collect(),
                ty: f.annotation().ty().clone(),
            })
            .collect();

        // Create the manifest
        Manifest { items }
    }

    pub fn get_items(&self) -> Vec<(Path, Vec<Type>, Type)> {
        self.items
            .iter()
            .map(|i| (i.path.clone(), i.params.clone(), i.ty.clone()))
            .collect()
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
