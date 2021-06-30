use std::fmt;

use serde::de::{self, Deserializer, MapAccess, SeqAccess, Visitor};
use serde::{ser::SerializeStruct, Deserialize, Serialize, Serializer};

use crate::{
    ast::{Module, Node, Path, RoutineDef, StructDef, Type},
    semantics::semanticnode::SemanticAnnotations,
};

#[derive(Serialize, Deserialize, Debug)]
pub struct Manifest {
    routines: Vec<RoutineDef<SemanticAnnotations>>,
    structs: Vec<StructDef<SemanticAnnotations>>,
}

impl Manifest {
    pub fn extract(module: &Module<SemanticAnnotations>) -> Manifest {
        // Get list of all functions contained within a module and their paths
        let routines = module
            .deep_get_functions()
            .iter()
            .map(|f| match f {
                crate::ast::Item::Routine(rd) => rd.clone(),
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
                    i.ty.clone(),
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

impl Serialize for RoutineDef<SemanticAnnotations> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("RoutineDef", 3)?;
        state.serialize_field("name", self.get_name())?;
        state.serialize_field("params", self.get_params())?;
        state.serialize_field("ty", self.get_return_type())?;
        state.serialize_field("annotations", self.annotation())?;
        state.end()
    }
}

impl<'de> Deserialize<'de> for RoutineDef<SemanticAnnotations> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        const NAME: &str = "name";
        const PARAMS: &str = "params";
        const TY: &str = "ty";
        const ANNOTATIONS: &str = "annotations";

        #[derive(serde::Deserialize)]
        #[serde(field_identifier, rename_all = "lowercase")]
        enum Field {
            Name,
            Params,
            Ty,
            Annotations,
        }

        struct RoutineDefVisitor;

        impl<'de> Visitor<'de> for RoutineDefVisitor {
            type Value = RoutineDef<SemanticAnnotations>;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct RoutineDef")
            }

            fn visit_seq<V>(self, mut seq: V) -> Result<RoutineDef<SemanticAnnotations>, V::Error>
            where
                V: SeqAccess<'de>,
            {
                let name: String = seq
                    .next_element()?
                    .ok_or_else(|| de::Error::invalid_length(0, &self))?;
                let params = seq
                    .next_element()?
                    .ok_or_else(|| de::Error::invalid_length(0, &self))?;
                let ret_ty = seq
                    .next_element()?
                    .ok_or_else(|| de::Error::invalid_length(0, &self))?;
                let annotations = seq
                    .next_element()?
                    .ok_or_else(|| de::Error::invalid_length(0, &self))?;

                let rd = RoutineDef::new_function(&name, annotations, params, ret_ty, vec![]);
                Ok(rd)
            }

            fn visit_map<V>(self, mut map: V) -> Result<RoutineDef<SemanticAnnotations>, V::Error>
            where
                V: MapAccess<'de>,
            {
                let mut name: Option<String> = None;
                let mut params = None;
                let mut ret_ty = None;
                let mut annotations = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Name => {
                            if name.is_some() {
                                return Err(de::Error::duplicate_field(NAME));
                            }
                            name = Some(map.next_value()?);
                        }
                        Field::Params => {
                            if params.is_some() {
                                return Err(de::Error::duplicate_field(PARAMS));
                            }
                            params = Some(map.next_value()?);
                        }
                        Field::Ty => {
                            if ret_ty.is_some() {
                                return Err(de::Error::duplicate_field(TY));
                            }
                            ret_ty = Some(map.next_value()?);
                        }
                        Field::Annotations => {
                            if annotations.is_some() {
                                return Err(de::Error::duplicate_field(ANNOTATIONS));
                            }
                            annotations = Some(map.next_value()?);
                        }
                    }
                }

                let name = name.ok_or_else(|| de::Error::missing_field(NAME))?;
                let params = params.ok_or_else(|| de::Error::missing_field(PARAMS))?;
                let ret_ty = ret_ty.ok_or_else(|| de::Error::missing_field(TY))?;
                let annotations =
                    annotations.ok_or_else(|| de::Error::missing_field(ANNOTATIONS))?;

                Ok(RoutineDef::new_function(
                    &name,
                    annotations,
                    params,
                    ret_ty,
                    vec![],
                ))
            }
        }

        const FIELDS: &'static [&'static str] = &["name", "params", "ty", "annotations"];
        deserializer.deserialize_struct("RoutineDef", FIELDS, RoutineDefVisitor)
    }
}