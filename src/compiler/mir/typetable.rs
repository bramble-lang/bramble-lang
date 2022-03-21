//! The system for keeping track of and defining types in the Bramble MIR.

use std::fmt::Display;

use crate::{
    compiler::{
        ast::{Node, Parameter, Path, PointerMut, StructDef, Type},
        semantics::semanticnode::SemanticContext,
    },
    StringId,
};

pub struct TypeTable {
    table: Vec<MirTypeDef>,
}

impl TypeTable {
    pub fn new() -> TypeTable {
        let table = vec![
            MirTypeDef::Base(MirBaseType::Unit),
            MirTypeDef::Base(MirBaseType::Null),
            MirTypeDef::Base(MirBaseType::U8),
            MirTypeDef::Base(MirBaseType::U16),
            MirTypeDef::Base(MirBaseType::U32),
            MirTypeDef::Base(MirBaseType::U64),
            MirTypeDef::Base(MirBaseType::I8),
            MirTypeDef::Base(MirBaseType::I16),
            MirTypeDef::Base(MirBaseType::I32),
            MirTypeDef::Base(MirBaseType::I64),
            MirTypeDef::Base(MirBaseType::F64),
            MirTypeDef::Base(MirBaseType::Bool),
            MirTypeDef::Base(MirBaseType::StringLiteral),
        ];

        TypeTable { table }
    }

    /// Given a [`TypeId`] this returns the corresponding [`MirTypeDef`].
    pub fn get(&self, id: TypeId) -> &MirTypeDef {
        &self.table[id.0 as usize]
    }

    /// Returns a mutable [`MirTypeDef`] reference.
    fn get_mut(&mut self, id: TypeId) -> &mut MirTypeDef {
        &mut self.table[id.0 as usize]
    }

    /// Adds the given [`Type`] to the type table. If this type references any type which is
    /// not in the table then it will also add the referenced type to the table. Structures will
    /// be added as [`MirStructDef::Declared`].
    ///
    /// If the given [`Type`] is already in the table, then this will return the [`TypeId`] for
    /// that occurance, rather than add another entry.
    pub fn add(&mut self, ty: &Type) -> Result<TypeId, TypeTableError> {
        // Check if ty is already in the table
        if let Some(id) = self.find(ty) {
            return Ok(id);
        }

        // Create a MirType using the referenced TypeIds and add to the table
        let mir_ty = match ty {
            Type::Array(el, sz) => MirTypeDef::Array {
                ty: self.add(el)?,
                sz: *sz,
            },
            Type::RawPointer(mutable, target) => MirTypeDef::RawPointer {
                mutable: *mutable,
                target: self.add(target)?,
            },
            Type::Custom(path) => {
                if path.is_canonical() {
                    MirTypeDef::Structure {
                        path: path.clone(),
                        def: MirStructDef::Declared,
                    }
                } else {
                    return Err(TypeTableError::ExpectedCanonicalPath);
                }
            }
            _ => return Err(TypeTableError::BaseTypesMissing),
        };

        self.table.push(mir_ty);

        // Return the index of the new type as a TypeId
        Ok(TypeId(self.table.len() as u32 - 1))
    }

    /// Adds the definition for the given structure. This will use the path in the
    /// [`SemanticContext`] as the canonical path for the structure.
    pub fn add_struct_def(
        &mut self,
        sd: &StructDef<SemanticContext>,
    ) -> Result<TypeId, TypeTableError> {
        // Convert the fields of the structure to MIR Fields
        let fields = sd
            .get_fields()
            .iter()
            .map(|f| f.to_field(self))
            .collect::<Result<_, _>>()?;

        // Search the table for a structure with the same canonical path
        if let Some(id) = self.find_by_path(sd.context().canonical_path())? {
            if let MirTypeDef::Structure { def, .. } = self.get_mut(id) {
                // If a match is found
                // check if it is not defined
                if *def == MirStructDef::Declared {
                    *def = MirStructDef::Defined(fields);
                    Ok(id)
                } else {
                    Err(TypeTableError::StrutureAlreadyDefined)
                }
            } else {
                // If it is, then return an error
                Err(TypeTableError::ExpectedStructure)
            }
        } else {
            // If no match found, then create a new structure entry and add to the table
            self.table.push(MirTypeDef::Structure {
                path: sd.context().canonical_path().clone(),
                def: MirStructDef::Defined(fields),
            });
            Ok(TypeId(self.table.len() as u32 - 1))
        }
    }

    /// Given a [`Path`] this will search the table for a user defined type
    /// with a matching canonical path.
    pub fn find_by_path(&self, path: &Path) -> Result<Option<TypeId>, TypeTableError> {
        // If the given path is not canonical then return None
        if !path.is_canonical() {
            return Err(TypeTableError::ExpectedCanonicalPath);
        }

        Ok(self
            .table
            .iter()
            .position(|ty| match ty {
                MirTypeDef::Structure { path: p, .. } => p == path,
                _ => false,
            })
            .map(|idx| TypeId(idx as u32)))
    }

    /// Searches the table for the given [`Type`] and returns the [`TypeId`] if found. [`Type::Custom`]
    /// this will search through the table for a type with the same canonical path, it may return as
    /// [`MirStructDef::Declared`] or [`MirStructDef::Defined`].
    pub fn find(&self, ty: &Type) -> Option<TypeId> {
        // If ty is not found, then return None
        let mir_ty = match ty {
            Type::Unit
            | Type::Null
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::F64
            | Type::Bool
            | Type::StringLiteral => {
                // If ty is a base type, then convert to MirBaseType and find in the table
                let base =
                    MirBaseType::parse(ty).expect("Should never be able to get a non-base type");
                MirTypeDef::Base(base)
            }
            Type::Array(el_ty, sz) => {
                // If ty is an array type, then get the TypeId for the element type and search the table for a matching (TypeId, size) tuple
                let el_ty_id = self.find(el_ty)?;
                MirTypeDef::Array {
                    ty: el_ty_id,
                    sz: *sz,
                }
            }
            Type::RawPointer(mutable, target) => {
                // If ty is a raw pointer, then get the TypeId for the target type and search for a pointer to the type id
                let target_id = self.find(target)?;
                MirTypeDef::RawPointer {
                    mutable: *mutable,
                    target: target_id,
                }
            }
            Type::Custom(path) => {
                // if ty is a structure, then search for an entry with a matching canonical path
                MirTypeDef::Structure {
                    path: path.clone(),
                    def: MirStructDef::Declared,
                }
            }

            // The presence of this wildcard points indicates that the Type enum has variants that no longer make sense
            // and should be refactored out.
            _ => panic!("Invalid Type: {:?}", ty),
        };

        for idx in 0..self.table.len() {
            if self.table[idx] == mir_ty {
                return Some(TypeId(idx as u32));
            }
        }

        None
    }

    /// Will return true if every Structure is [`MirStructDef::Defined`] and every referenced
    /// [`TypeId`] is within the bounds of the table.
    pub fn is_complete(&self) -> bool {
        // Check that every entry is fully defined
        for entry in &self.table {
            if !self.is_defined(entry) {
                return false;
            }
        }

        true
    }

    /// Returns true if the given type from this Type Table is defined. Defined means that
    /// this [`TypeId`] exists in the table, any type it references exists in the table, and
    /// (if it is a Structure) the structure is [`MirStructDef::Defined`] and not [`MirStructDef::Declared`].
    fn is_defined(&self, ty: &MirTypeDef) -> bool {
        let max_id = TypeId(self.table.len() as u32);
        match ty {
            MirTypeDef::Array { ty, .. } if *ty >= max_id => return false,
            MirTypeDef::RawPointer { target, .. } if *target >= max_id => return false,
            MirTypeDef::Structure { def, .. } if *def == MirStructDef::Declared => return false,
            MirTypeDef::Structure {
                def: MirStructDef::Defined(fields),
                ..
            } if fields.iter().any(|f| f.ty >= max_id) => return false,
            _ => (),
        }
        true
    }
}

impl Display for TypeTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (id, ty) in self.table.iter().enumerate() {
            f.write_fmt(format_args!("{} -> {}\n", id, ty))?
        }

        Ok(())
    }
}

impl Parameter<SemanticContext> {
    /// Converts this [`Parameter`] to a [`Field`].
    fn to_field(&self, table: &mut TypeTable) -> Result<Field, TypeTableError> {
        let id = table.add(&self.ty)?;
        Ok(Field {
            name: self.name,
            ty: id,
        })
    }
}

/// These are the most basic types available and correspond to the arithmetic and operand
/// types avaible on the CPU. All other types in Bramble are constructed from these types.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum MirBaseType {
    Unit,
    Null,
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F64,
    Bool,
    StringLiteral,
}

impl MirBaseType {
    fn parse(ty: &Type) -> Option<Self> {
        let base = match ty {
            Type::Unit => Self::Unit,
            Type::Null => Self::Null,
            Type::U8 => Self::U8,
            Type::U16 => Self::U16,
            Type::U32 => Self::U32,
            Type::U64 => Self::U64,
            Type::I8 => Self::I8,
            Type::I16 => Self::I16,
            Type::I32 => Self::I32,
            Type::I64 => Self::I64,
            Type::F64 => Self::F64,
            Type::Bool => Self::Bool,
            Type::StringLiteral => Self::StringLiteral,

            _ => return None,
        };
        Some(base)
    }
}

impl Display for MirBaseType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MirBaseType::Unit => f.write_str("()"),
            MirBaseType::Null => f.write_str("null"),
            MirBaseType::U8 => f.write_str("u8"),
            MirBaseType::U16 => f.write_str("u16"),
            MirBaseType::U32 => f.write_str("u32"),
            MirBaseType::U64 => f.write_str("u64"),
            MirBaseType::I8 => f.write_str("i8"),
            MirBaseType::I16 => f.write_str("i16"),
            MirBaseType::I32 => f.write_str("i32"),
            MirBaseType::I64 => f.write_str("i64"),
            MirBaseType::F64 => f.write_str("f64"),
            MirBaseType::Bool => f.write_str("bool"),
            MirBaseType::StringLiteral => f.write_str("string"),
        }
    }
}

/// Defines a single type that exists within a Bramble program.
#[derive(Debug, Clone)]
pub enum MirTypeDef {
    Base(MirBaseType),
    Array { ty: TypeId, sz: usize },
    RawPointer { mutable: PointerMut, target: TypeId },
    Structure { path: Path, def: MirStructDef },
}

impl MirTypeDef {
    /// If this is a [`Structure`](MirTypeDef::Structure) variant then
    /// return a reference to the underlying [`MirStructDef`]. Otherwise,
    /// return [`None`](Option::None).
    pub fn get_struct_def(&self) -> Option<&MirStructDef> {
        match self {
            Self::Structure { def, .. } => Some(def),
            _ => None,
        }
    }
}

impl PartialEq for MirTypeDef {
    /// Checks if two MirTypeDef values are logically the same type. Two [`MirTypeDef::Structure`] values
    /// are equal if their [`Paths`](Path) are equal.
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Base(l0), Self::Base(r0)) => l0 == r0,
            (Self::Array { ty: l_ty, sz: l_sz }, Self::Array { ty: r_ty, sz: r_sz }) => {
                l_ty == r_ty && l_sz == r_sz
            }
            (
                Self::RawPointer {
                    mutable: l_mutable,
                    target: l_target,
                },
                Self::RawPointer {
                    mutable: r_mutable,
                    target: r_target,
                },
            ) => l_mutable == r_mutable && l_target == r_target,
            (Self::Structure { path: l_path, .. }, Self::Structure { path: r_path, .. }) => {
                l_path == r_path
            }
            _ => false,
        }
    }
}

impl Display for MirTypeDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MirTypeDef::Base(base) => f.write_fmt(format_args!("{}", base)),
            MirTypeDef::Array { ty, sz } => f.write_fmt(format_args!("[{}; {}]", ty, sz)),
            MirTypeDef::RawPointer { mutable, target } => {
                f.write_fmt(format_args!("*{} {}", mutable, target))
            }
            MirTypeDef::Structure { path, def } => f.write_fmt(format_args!("{}", path)),
        }
    }
}

/// The Unique Identifier for a type within a Bramble program. Every type,
/// including base types, is given a TypeId.  MIR uses the TypeId to annotate
/// the types of MIR values and variables.
///
/// [`TypeId`] is implemented as a single [`u32`] to conserve memory and to
/// optimize comparison operations. Using this design requires that all types,
/// including base types, be added to the [`TypeTable`], but because the implementation
/// of [`TypeTable`] and [`TypeId`] is completely invisible to users, this
/// was deemed acceptable.
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub struct TypeId(u32);

impl Display for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}

/// Provides the definition of a structure type. A structure may be referenced in another
/// type before it is defined; in such a case, the structure is added to the type table
/// as [`StructDef::Declared`].  When the structure is defined, the value will change to
/// [`StructDef::Defined`], which will include the field definitions.
#[derive(Debug, PartialEq, Clone)]
pub enum MirStructDef {
    /// A placeholder value that is used so that a TypeId can be given to a structure name
    /// before the structure has been fully defined.
    Declared,

    /// The definition of a structure.
    Defined(Vec<Field>),
}

impl MirStructDef {
    /// If this structure has a matching field, then return the [`FieldId`].
    /// Otherwise, return [`Option::None`].  This will also return [`Option::None`] if
    /// this structure is in the [`MirStructDef::Declared`] state.
    pub fn find_field(&self, name: StringId) -> Option<(FieldId, &Field)> {
        match self {
            Self::Declared => None,
            Self::Defined(fields) => fields
                .iter()
                .enumerate()
                .find(|(_, f)| f.name == name)
                .map(|(fid, f)| (FieldId::new(fid as u32), f)),
        }
    }
}

/// Represents a field in a structure definition. This encode the name of the field
/// and the type of the field.  The position of the field, in memory layout, is encoded
/// by [`MirStructDef::Defined`] by storing the fields as an ordered vector.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Field {
    pub name: StringId,
    pub ty: TypeId,
}

/// Identifies a specific field within a [`MirTypeDef::Structure`]. To be useful, the [`FieldId`]
/// must be coupled with a [`TypeId`] that refers to a [`MirTypeDef::Structure`] type in the
/// [`TypeTable`]. This [`FieldId`] uniquely identifies a specific field in the structure referred to by [`TypeId`].
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct FieldId(u32);

impl FieldId {
    fn new(id: u32) -> FieldId {
        FieldId(id)
    }
}

impl Display for FieldId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}

#[derive(Debug)]
pub enum TypeTableError {
    /// The user gave a relative path rather than a canonical path
    ExpectedCanonicalPath,

    /// A base type (e.g., u32) is missing from the table
    BaseTypesMissing,

    /// Attempting to add a definition to a structure that already has a definition
    StrutureAlreadyDefined,

    /// Attempting to add a definition to a type that is not a structure
    ExpectedStructure,
}

#[cfg(test)]
impl From<FieldId> for u32 {
    /// This is used in unit tests to validate that the correct fields are being used
    /// in the MIR of a member access. It is only built for `test` because there is no
    /// other case where code outside of this module should be able to directly access
    /// the value of a [`FieldId`].
    ///
    /// Implementation of the From trait was chosen because it isolates the privacy
    /// violation as much as logically possible from the code that is used in the compiler
    /// itself.
    fn from(f: FieldId) -> Self {
        f.0
    }
}
