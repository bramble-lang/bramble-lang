//! The system for keeping track of and defining types in the Bramble MIR.

use crate::{compiler::ast::{PointerMut, Path}, StringId};

pub struct TypeTable {
    table: Vec<MirTypeDef>,
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
    Array{ty: TypeId, sz: usize},
    RawPointer{mutable: PointerMut, target: TypeId},
    Structure{path: Path, def: StructDef},
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
pub enum StructDef {
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