//! The system for keeping track of and defining types in the Bramble MIR.

use crate::{
    compiler::{ast::{Path, PointerMut, Type, StructDef}, semantics::semanticnode::SemanticContext},
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

    /// Adds the given [`Type`] to the type table. If this type references any type which is
    /// not in the table then it will also add the referenced type to the table. Structures will
    /// be added as [`MirStructDef::Declared`].
    pub fn add(&mut self, ty: &Type) -> TypeId {
        // Check if ty is already in the table
        // if not, then add every component type
        // Create a MirType using the referenced TypeIds and add to the table
        // Return the index of the new type as a TypeId
        todo!()
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

    /// Searches the table for the given [`Type`] and returns the [`TypeId`] if found.
    pub fn get(&self, ty: &Type) -> Option<TypeId> {
        // If ty is a base type, then convert to MirBaseType and find in the table
        // If ty is an array type, then get the TypeId for the element type and search the table for a matching (TypeId, size) tuple
        // If ty is a raw pointer, then get the TypeId for the target type and search for a pointer to the type id
        // if ty is a structure, then search for an entry with a matching canonical path
        // If ty is not found, then return None
        todo!()
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

/// Defines a single type that exists within a Bramble program.
#[derive(Debug, PartialEq, Clone)]
pub enum MirTypeDef {
    Base(MirBaseType),
    Array { ty: TypeId, sz: usize },
    RawPointer { mutable: PointerMut, target: TypeId },
    Structure { path: Path, def: MirStructDef },
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
