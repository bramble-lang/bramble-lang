//! The system for keeping track of and defining types in the Bramble MIR.

use crate::{
    compiler::{
        ast::{Path, PointerMut, StructDef, Type},
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

    pub fn get(&self, id: TypeId) -> &MirTypeDef {
        &self.table[id.0 as usize]
    }

    /// Adds the given [`Type`] to the type table. If this type references any type which is
    /// not in the table then it will also add the referenced type to the table. Structures will
    /// be added as [`MirStructDef::Declared`].
    pub fn add(&mut self, ty: &Type) -> TypeId {
        // Check if ty is already in the table
        if let Some(id) = self.find(ty) {
            return id;
        }

        // Create a MirType using the referenced TypeIds and add to the table
        let mir_ty = match ty {
            Type::Array(el, sz) => MirTypeDef::Array {
                ty: self.add(el),
                sz: *sz,
            },
            Type::RawPointer(mutable, target) => MirTypeDef::RawPointer {
                mutable: *mutable,
                target: self.add(target),
            },
            Type::Custom(path) => {
                if path.is_canonical() {
                    MirTypeDef::Structure {
                        path: path.clone(),
                        def: MirStructDef::Declared,
                    }
                } else {
                    panic!("Exected canonical path on custom type")
                }
            }
            _ => panic!("Base types must be in the type table before any other type is added"),
        };

        self.table.push(mir_ty);

        // Return the index of the new type as a TypeId
        TypeId(self.table.len() as u32 - 1)
    }

    /// Adds the definition for the given structure. This will use the path in the
    /// [`SemanticContext`] as the canonical path for the structure.
    pub fn add_struct_def(&mut self, def: &StructDef<SemanticContext>) -> TypeId {
        // Search the table for a structure with the same canonical path
        // If no match found, then create a new structure entry and add to the table
        // If a match is found
        // check if it is already Defined
        // If it is, then return an error
        // otherwise, add the definition
        todo!()
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

            // Unnecessary
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
        // Iterate through every entry in the table
        // If it references another type, check that the type id exists
        // If it is a structure, check that it is Defined and not Declared
        todo!()
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

/// Defines a single type that exists within a Bramble program.
#[derive(Debug, Clone)]
pub enum MirTypeDef {
    Base(MirBaseType),
    Array { ty: TypeId, sz: usize },
    RawPointer { mutable: PointerMut, target: TypeId },
    Structure { path: Path, def: MirStructDef },
}

impl PartialEq for MirTypeDef {
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
            (
                Self::Structure {
                    path: l_path,
                    def: _l_def,
                },
                Self::Structure {
                    path: r_path,
                    def: _r_def,
                },
            ) => l_path == r_path,
            _ => false,
        }
    }
}

/// The Unique Identifier for a type within a Bramble program. Every type,
/// including base types, is given a TypeId.  MIR uses the TypeId to annotate
/// the types of MIR values and variables.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct TypeId(u32);

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

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Field {
    pub name: StringId,
    pub ty: TypeId,
}

#[cfg(test)]
mod tests {
    use crate::compiler::{ast::Element, parser::ParserContext, Span};

    use super::*;

    #[test]
    fn base_types() {
        let table = TypeTable::new();

        for ty in [
            Type::Unit,
            Type::Null,
            Type::U8,
            Type::U16,
            Type::U32,
            Type::U64,
            Type::I8,
            Type::I16,
            Type::I32,
            Type::I64,
            Type::F64,
            Type::Bool,
            Type::StringLiteral,
        ] {
            table.find(&ty).unwrap();
        }
    }

    #[test]
    fn add_array_type() {
        let mut table = TypeTable::new();
        for ty in [
            Type::Unit,
            Type::Null,
            Type::U8,
            Type::U16,
            Type::U32,
            Type::U64,
            Type::I8,
            Type::I16,
            Type::I32,
            Type::I64,
            Type::F64,
            Type::Bool,
            Type::StringLiteral,
        ] {
            let expected = MirTypeDef::Array {
                ty: table.find(&ty).unwrap(),
                sz: 4,
            };

            let arr = Type::Array(Box::new(ty), 4);
            let tid = table.add(&arr);

            let actual = table.get(tid);

            assert_eq!(actual, &expected)
        }
    }

    #[test]
    fn add_raw_pointer_type() {
        let mut table = TypeTable::new();
        for ty in [
            Type::Unit,
            Type::Null,
            Type::U8,
            Type::U16,
            Type::U32,
            Type::U64,
            Type::I8,
            Type::I16,
            Type::I32,
            Type::I64,
            Type::F64,
            Type::Bool,
            Type::StringLiteral,
        ] {
            for mutable in [PointerMut::Const, PointerMut::Mut] {
                let expected = MirTypeDef::RawPointer {
                    mutable,
                    target: table.find(&ty).unwrap(),
                };

                let ptr = Type::RawPointer(mutable, Box::new(ty.clone()));
                let tid = table.add(&ptr);

                let actual = table.get(tid);

                assert_eq!(actual, &expected)
            }
        }
    }

    #[test]
    fn add_structure_def() {
        let mut table = TypeTable::new();


        let path: Path = vec![Element::CanonicalRoot, Element::Id(StringId::new())].into();
        let decl = table.add(&Type::Custom(path.clone()));

        let actual = table.get(decl);

        let expected = MirTypeDef::Structure{
            path: path.clone(),
            def: MirStructDef::Declared,
        };
        assert_eq!(actual, &expected);

        // Test adding a definition to the structure
        let sd = StructDef::new(StringId::new(), SemanticContext::new_local(0, ParserContext::new(Span::zero()), Type::Unit), vec![]);
    }
}
