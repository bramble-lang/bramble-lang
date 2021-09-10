use serde::{Deserialize, Serialize};

use crate::{
    compiler::{
        ast::{Item, Module, Node, Parameter, Path, RoutineDef, RoutineDefType, StructDef, Type},
        import::Import,
        semantics::semanticnode::SemanticContext,
    },
    StringTable,
};

/// Data type used to represent items from a compiled artifact which can be imported by other
/// projects.
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

    /// Convert a Manifest of a Braid artifact to set of definitions which can be used
    /// by the compiler for imported items.
    pub fn to_import(&self, st: &mut StringTable) -> Import {
        let structs = self.structs.iter().map(|s| s.to_sd(st)).collect();
        let funcs = self.routines.iter().map(|r| r.to_rd(st)).collect();

        Import { structs, funcs }
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

/// Represent a routine definition in a way which can be serialized to a manifest file
/// and deserialized from a manifest file.
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

    fn to_rd(&self, st: &mut StringTable) -> (Path, Vec<Type>, Type) {
        let path = string_to_path(st, &self.canon_path);
        let params = self.params.iter().map(|p| p.to_ty(st)).collect();
        let ret_ty = self.ret_ty.to_ty(st);

        (path, params, ret_ty)
    }
}

/// Represent a structure definition in a way which can be serialized to a manifest file
/// and deserialized from a manifest file.
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

    fn to_sd(&self, st: &mut StringTable) -> StructDef<SemanticContext> {
        let mut ctx = SemanticContext::new(0, 0, Type::Unit);
        ctx.set_canonical_path(string_to_path(st, &self.canon_path));

        let name = st.insert(self.name.clone());
        let fields = self
            .fields
            .iter()
            .map(|(fnm, fty)| Parameter {
                context: ctx.clone(),
                name: st.insert(fnm.into()),
                ty: fty.to_ty(st),
            })
            .collect();

        StructDef::new(name, ctx, fields)
    }
}

/// Represent a Type in the manifest file
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

    fn to_ty(&self, st: &mut StringTable) -> Type {
        match self {
            ManifestType::U8 => Type::U8,
            ManifestType::U16 => Type::U16,
            ManifestType::U32 => Type::U32,
            ManifestType::U64 => Type::U64,
            ManifestType::I8 => Type::I8,
            ManifestType::I16 => Type::I16,
            ManifestType::I32 => Type::I32,
            ManifestType::I64 => Type::I64,
            ManifestType::Bool => Type::Bool,
            ManifestType::StringLiteral => Type::StringLiteral,
            ManifestType::Array(_, _) => todo!(),
            ManifestType::Unit => Type::Unit,
            ManifestType::Custom(p) => Type::Custom(string_to_path(st, p)),
        }
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

    fn to_def(&self) -> RoutineDefType {
        match self {
            ManifestRoutineDefType::Function => RoutineDefType::Function,
            ManifestRoutineDefType::Coroutine => RoutineDefType::Coroutine,
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

fn string_to_path(st: &mut StringTable, p: &str) -> Path {
    use crate::compiler::ast::Element;
    let p = p.split("::");

    let mut path = Path::new();
    for e in p {
        match e {
            "self" => path.push(Element::Selph),
            "super" => path.push(Element::Super),
            "root" => path.push(Element::FileRoot),
            "project" => path.push(Element::CanonicalRoot),
            e => path.push(Element::Id(st.insert(e.into()))),
        }
    }

    path
}
