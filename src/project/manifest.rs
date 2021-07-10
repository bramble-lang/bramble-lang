use serde::{Deserialize, Serialize};

use crate::compiler::{
    ast::{Item, Module, Node, Path, RoutineDef, StructDef, Type},
    semantics::semanticnode::SemanticAnnotations,
};

#[derive(Serialize, Deserialize, Debug)]
pub struct Manifest {
    routines: Vec<RoutineDef<SemanticAnnotations>>,
    structs: Vec<StructDef<SemanticAnnotations>>,
}

impl Manifest {
    pub fn new(
        routines: &[RoutineDef<SemanticAnnotations>],
        structs: &[StructDef<SemanticAnnotations>],
    ) -> Manifest {
        Manifest {
            routines: routines.into(),
            structs: structs.into(),
        }
    }

    pub fn extract(module: &Module<SemanticAnnotations>) -> Manifest {
        // Get list of all functions contained within a module and their paths
        let routines = module
            .deep_get_functions()
            .iter()
            .map(|f| match f {
                Item::Routine(rd) => rd.clone(),
                _ => panic!("Unexpected: got an Item that was not a RoutineDef"),
            })
            .collect();

        // Get list of all structures contained within the module
        let structs = module
            .deep_get_structs()
            .into_iter()
            .map(|s| s.clone())
            .collect();

        // Create the manifest
        Manifest { routines, structs }
    }

    pub fn get_functions(&self) -> Vec<(Path, Vec<Type>, Type)> {
        self.routines
            .iter()
            .map(|i| {
                (
                    i.annotation().get_canonical_path().clone(),
                    i.params.iter().map(|p| p.ty.clone()).clone().collect(),
                    i.ret_ty.clone(),
                )
            })
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
