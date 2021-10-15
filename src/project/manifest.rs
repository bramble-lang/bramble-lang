use serde::{Deserialize, Serialize};

use crate::{
    compiler::{
        ast::{Element, Item, Module, Node, Path, RoutineDef, RoutineDefType, StructDef, Type},
        import::{Import, ImportRoutineDef, ImportStructDef},
        semantics::semanticnode::SemanticContext,
        CompilerDisplay, CompilerDisplayError,
    },
    StringTable,
};

use super::ManifestError;

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
    ) -> Result<Self, ManifestError> {
        let routines = routines
            .iter()
            .map(|r| ManifestRoutineDef::from_rd(r, st))
            .collect::<Result<Vec<_>, ManifestError>>()?;
        let structs = structs
            .iter()
            .map(|s| ManifestStructDef::from_sd(s, st))
            .collect::<Result<Vec<_>, ManifestError>>()?;

        Ok(Manifest { routines, structs })
    }

    pub fn extract(
        st: &StringTable,
        module: &Module<SemanticContext>,
    ) -> Result<Self, ManifestError> {
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
    pub fn to_import(self, st: &mut StringTable) -> Result<Import, ManifestError> {
        let structs = self
            .structs
            .into_iter()
            .map(|s| s.to_sd(st))
            .collect::<Result<_, _>>()?;
        let funcs = self
            .routines
            .into_iter()
            .map(|r| r.to_rd(st))
            .collect::<Result<_, _>>()?;

        Ok(Import { structs, funcs })
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
    fn from_rd(rd: &RoutineDef<SemanticContext>, st: &StringTable) -> Result<Self, ManifestError> {
        let name = st.get(rd.name)?.into();
        let params = rd
            .params
            .iter()
            .map(|p| ManifestType::from_ty(st, &p.ty))
            .collect::<Result<_, ManifestError>>()?;
        let def = ManifestRoutineDefType::from_def(rd.def);
        let ret_ty = ManifestType::from_ty(st, &rd.ret_ty)?;
        let canon_path = path_to_string(st, rd.get_context().canonical_path())?;

        Ok(ManifestRoutineDef {
            name,
            params,
            def,
            ret_ty,
            canon_path,
        })
    }

    fn to_rd(self, st: &mut StringTable) -> Result<ImportRoutineDef, ManifestError> {
        let path = string_to_path(st, &self.canon_path)?;
        let params = self
            .params
            .iter()
            .map(|p| Ok(p.to_ty(st)?))
            .collect::<Result<_, ManifestError>>()?;
        let ret_ty = self.ret_ty.to_ty(st)?;

        Ok(ImportRoutineDef::new(path, params, ret_ty))
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
    fn from_sd(sd: &StructDef<SemanticContext>, st: &StringTable) -> Result<Self, ManifestError> {
        let name = st.get(sd.get_name())?.into();
        let canon_path = path_to_string(st, sd.get_context().canonical_path())?;
        let fields = sd
            .get_fields()
            .iter()
            .map(|f| {
                let name = st.get(f.name).map_err(|e| e.into());
                let fty = ManifestType::from_ty(st, &f.ty);
                name.and_then(|name| fty.and_then(|fty| Ok((name.into(), fty))))
            })
            .collect::<Result<Vec<_>, ManifestError>>()?;

        Ok(ManifestStructDef {
            name,
            canon_path,
            fields,
        })
    }

    fn to_sd(self, st: &mut StringTable) -> Result<ImportStructDef, ManifestError> {
        let canon_path = string_to_path(st, &self.canon_path)?;
        let fields = self
            .fields
            .iter()
            .map(|(fnm, fty)| Ok((st.insert(fnm.into()), fty.to_ty(st)?)))
            .collect::<Result<Vec<_>, ManifestError>>()?;

        Ok(ImportStructDef::new(canon_path, fields))
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
    fn from_ty(st: &StringTable, ty: &Type) -> Result<Self, ManifestError> {
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
            _ => return Err(ManifestError::CannotConvertType(ty.clone())),
        };

        Ok(man_ty)
    }

    fn to_ty(&self, st: &mut StringTable) -> Result<Type, ManifestError> {
        let cty = match self {
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
            ManifestType::Array(box el_ty, sz) => Type::Array(box el_ty.to_ty(st)?, *sz),
            ManifestType::Unit => Type::Unit,
            ManifestType::Custom(p) => Type::Custom(string_to_path(st, p)?),
        };

        Ok(cty)
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
enum ManifestRoutineDefType {
    Function,
    Coroutine,
}

impl ManifestRoutineDefType {
    /// Convert a Compiler Routine Definition Type value to a Manifest Routine Definition Type value.
    fn from_def(def: RoutineDefType) -> Self {
        match def {
            RoutineDefType::Function => Self::Function,
            RoutineDefType::Coroutine => Self::Coroutine,
        }
    }

    /// Convert a Manifest Routine Definition Type to a Compiler Routine Definition Type
    fn to_def(&self) -> RoutineDefType {
        match self {
            ManifestRoutineDefType::Function => RoutineDefType::Function,
            ManifestRoutineDefType::Coroutine => RoutineDefType::Coroutine,
        }
    }
}

/// Convert a Compiler Path value into the Manifest file string format.
fn path_to_string(st: &StringTable, p: &Path) -> Result<String, CompilerDisplayError> {
    p.fmt(st)
}

/// Convert a Manifest file Path string to a Compiler Path value.
fn string_to_path(st: &mut StringTable, p: &str) -> Result<Path, ManifestError> {
    /// Tests that an element is a valid identifier
    fn is_element_valid(el: &str) -> Result<(), ManifestError> {
        let cs = el.chars().collect::<Vec<_>>();
        if cs.len() == 0 {
            // Element must have at least one character
            Err(ManifestError::PathElementIsEmpty)
        } else {
            if !(cs[0].is_alphabetic() || cs[0] == '_') {
                // Element can only start with a letter or underscore
                Err(ManifestError::PathElementStartsWithInvalidChar(cs[0]))
            } else {
                // Element can only contain alphanumerics and _
                match cs.iter().find(|&&c| !(c.is_alphanumeric() || c == '_')) {
                    Some(c) => Err(ManifestError::PathElementContainsInvalidChar(*c)),
                    None => Ok(()),
                }
            }
        }
    }

    // Check if this is a canonical path, and remove the $ if it is
    let (p, is_canonical) = match p.strip_prefix("$") {
        Some(stripped) => (stripped, true),
        None => (p, false),
    };
    let elements = p.split("::");

    let mut path = vec![];
    if is_canonical {
        path.push(Element::CanonicalRoot)
    }

    for el in elements {
        is_element_valid(el)?;

        match el {
            "self" => path.push(Element::Selph),
            "super" => path.push(Element::Super),
            "root" => path.push(Element::FileRoot),
            e => path.push(Element::Id(st.insert(e.into()))),
        }
    }

    Ok(path.into())
}
