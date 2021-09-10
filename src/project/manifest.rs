use serde::{Deserialize, Serialize};

use crate::{
    compiler::{
        ast::{Item, Module, Node, Path, RoutineDef, RoutineDefType, StructDef, Type},
        import::Import,
        semantics::semanticnode::SemanticContext,
    },
    StringTable,
};

#[derive(Serialize, Deserialize, Debug)]
pub struct Manifest {
    routines: Vec<ManifestRoutineDef>,
    structs: Vec<ManifestStructDef>,
}

impl Manifest {
    pub fn new(
        st: &StringTable,
        routines: &[RoutineDef<SemanticContext>],
        structs: &[StructDef<SemanticContext>],
    ) -> Result<Self, String> {
        let routines = routines
            .iter()
            .map(|r| ManifestRoutineDef::from_rd(st, r))
            .collect::<Result<Vec<_>, String>>()?;
        let structs = structs
            .iter()
            .map(|s| ManifestStructDef::from_sd(st, s))
            .collect::<Result<Vec<_>, String>>()?;

        Ok(Manifest { routines, structs })
    }

    pub fn extract(st: &StringTable, module: &Module<SemanticContext>) -> Result<Self, String> {
        // Get list of all functions contained within a module and their paths
        let routines: Vec<_> = module
            .deep_get_functions()
            .iter()
            .map(|f| match f {
                Item::Routine(rd) => rd.clone(),
                _ => panic!("Unexpected: got an Item that was not a RoutineDef"),
            })
            .collect();

        // Get list of all structures contained within the module
        let structs: Vec<_> = module
            .deep_get_structs()
            .into_iter()
            .map(|s| s.clone())
            .collect();

        // Create the manifest
        Self::new(st, &routines, &structs)
    }

    pub fn to_import(&self) -> Import {
        todo!()
    }

    pub fn get_functions(&self) -> Vec<(Path, Vec<Type>, Type)> {
        todo!()
    }

    pub fn get_structs(&self) -> &Vec<StructDef<SemanticContext>> {
        todo!()
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

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
struct ManifestRoutineDef {
    name: String,
    canon_path: String,
    def: ManifestRoutineDefType,
    params: Vec<ManifestType>,
    ret_ty: ManifestType,
}

impl ManifestRoutineDef {
    fn from_rd(st: &StringTable, rd: &RoutineDef<SemanticContext>) -> Result<Self, String> {
        let name = st
            .get(rd.name)
            .ok_or(format!("Could not find routine name"))?
            .into();
        let params = rd
            .params
            .iter()
            .map(|p| ManifestType::from_ty(st, &p.ty))
            .collect::<Result<_, String>>()?;
        let def = ManifestRoutineDefType::from_def(rd.def);
        let ret_ty = ManifestType::from_ty(st, &rd.ret_ty)?;
        let canon_path = path_to_string(st, rd.get_context().get_canonical_path())?;

        Ok(ManifestRoutineDef {
            name,
            params,
            def,
            ret_ty,
            canon_path,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
struct ManifestStructDef {
    name: String,
    canon_path: String,
    fields: Vec<(String, ManifestType)>,
}

impl ManifestStructDef {
    fn from_sd(st: &StringTable, sd: &StructDef<SemanticContext>) -> Result<Self, String> {
        let name = st
            .get(sd.get_name())
            .ok_or(format!("Could not find the name of the struct"))?
            .into();
        let canon_path = path_to_string(st, sd.get_context().get_canonical_path())?;
        let fields = sd
            .get_fields()
            .iter()
            .map(|f| {
                let name = st.get(f.name).ok_or(format!("Could not find field name"));
                let fty = ManifestType::from_ty(st, &f.ty);
                name.and_then(|name| fty.and_then(|fty| Ok((name.into(), fty))))
            })
            .collect::<Result<Vec<_>, String>>()?;

        Ok(ManifestStructDef {
            name,
            canon_path,
            fields,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
enum ManifestType {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    Bool,
    StringLiteral,
    Array(Box<ManifestType>, usize),
    Unit,
    Custom(String),
}

impl ManifestType {
    fn from_ty(st: &StringTable, ty: &Type) -> Result<Self, String> {
        let man_ty = match ty {
            Type::U8 => Self::U8,
            Type::U16 => Self::U16,
            Type::U32 => Self::U32,
            Type::U64 => Self::U64,
            Type::I8 => Self::I8,
            Type::I16 => Self::I16,
            Type::I32 => Self::I32,
            Type::I64 => Self::I64,
            Type::Bool => Self::Bool,
            Type::StringLiteral => Self::StringLiteral,
            Type::Array(box el_ty, sz) => Self::Array(box Self::from_ty(st, el_ty)?, *sz),
            Type::Unit => Self::Unit,
            Type::Custom(p) => Self::Custom(path_to_string(st, p)?),
            _ => {
                return Err(format!(
                    "Type cannot be converted to manifest type: {:?}",
                    ty
                ))
            }
        };

        Ok(man_ty)
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
enum ManifestRoutineDefType {
    Function,
    Coroutine,
}

impl ManifestRoutineDefType {
    fn from_def(def: RoutineDefType) -> Self {
        match def {
            RoutineDefType::Function => Self::Function,
            RoutineDefType::Coroutine => Self::Coroutine,
        }
    }
}

fn path_to_string(st: &StringTable, p: &Path) -> Result<String, String> {
    let ps = p
        .iter()
        .map(|e| match e {
            crate::compiler::ast::Element::FileRoot => Some("root"),
            crate::compiler::ast::Element::CanonicalRoot => Some("project"),
            crate::compiler::ast::Element::Selph => Some("self"),
            crate::compiler::ast::Element::Super => Some("super"),
            crate::compiler::ast::Element::Id(id) => st.get(*id),
        })
        .collect::<Option<Vec<&str>>>()
        .ok_or(format!("Failed to convert Path to String"))?
        .join("::");
    Ok(ps)
}
