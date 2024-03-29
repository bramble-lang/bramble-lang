/*!
Represents an entire Bramble program, including imported libraries,
in MIR form.
*/

use std::fmt::Display;

use crate::{
    compiler::{
        ast::{Path, StructDef, Type},
        import::ImportStructDef,
        semantics::semanticnode::SemanticContext,
    },
    StringId,
};

use super::{
    ir::Procedure,
    typetable::{MirTypeDef, TypeId, TypeTable, TypeTableError},
};

/// Represents everything involved in compiling the current target compilation
/// unit (executable, library, etc.).
///
/// This corresponds to the Root of the Canonical Path
struct MirProgram;

/// The representation of the full program, including imports
///
/// This corresponds to the Project component of the Canonical Path
/// Manages all of the Types and Functions which exist within a single project
pub struct MirProject {
    /// Table of all the [`Types`](Type) defined during compilation.
    types: TypeTable,

    /// Table of all static defined values which can be referenced by code.
    static_defs: StaticDefinitions,
}

impl MirProject {
    pub fn new() -> MirProject {
        MirProject {
            types: TypeTable::new(),
            static_defs: StaticDefinitions::new(),
        }
    }

    /// Searches the [`TypeTable`] for the [`TypeId`] of the given
    /// [`Type`].
    pub fn find_type(&self, ty: &Type) -> Option<TypeId> {
        self.types.find(ty)
    }

    /// Get the [`MirTypeDef`] associted with the given [`TypeId`].
    pub fn get_type(&self, ty: TypeId) -> &MirTypeDef {
        self.types.get(ty)
    }

    /// Addsa  [`Type`] to the underlying [`TypeTable`].
    pub fn add_type(&mut self, ty: &Type) -> Result<TypeId, TypeTableError> {
        self.types.add(ty)
    }

    /// Returns true if the given type is signed, false otherwise.
    pub fn is_signed(&self, ty: TypeId) -> bool {
        match self.get_type(ty) {
            MirTypeDef::Base(base) => match base {
                super::MirBaseType::I8
                | super::MirBaseType::I16
                | super::MirBaseType::I32
                | super::MirBaseType::I64
                | super::MirBaseType::F64 => true,
                super::MirBaseType::Bool
                | super::MirBaseType::StringLiteral
                | super::MirBaseType::Unit
                | super::MirBaseType::Null
                | super::MirBaseType::U8
                | super::MirBaseType::U16
                | super::MirBaseType::U32
                | super::MirBaseType::U64 => false,
            },
            MirTypeDef::Array { .. } => false,
            MirTypeDef::RawPointer { .. } => false,
            MirTypeDef::Structure { .. } => false,
        }
    }

    /// Will return the width of the given type, if it is a base type or a pointer
    pub fn width(&self, ty: TypeId) -> Option<u64> {
        match self.get_type(ty) {
            MirTypeDef::Base(base) => match base {
                super::MirBaseType::Bool | super::MirBaseType::I8 | super::MirBaseType::U8 => {
                    Some(8)
                }
                super::MirBaseType::I16 | super::MirBaseType::U16 => Some(16),
                super::MirBaseType::I32 | super::MirBaseType::U32 => Some(32),
                super::MirBaseType::I64 | super::MirBaseType::F64 | super::MirBaseType::U64 => {
                    Some(64)
                }
                super::MirBaseType::Null | super::MirBaseType::StringLiteral => Some(64),
                super::MirBaseType::Unit => None,
            },
            MirTypeDef::Array { .. } => None,
            MirTypeDef::RawPointer { .. } => Some(64),
            MirTypeDef::Structure { .. } => None,
        }
    }

    /// Adds a new Structure definition to the [`MirProject`].
    pub fn add_struct_def(
        &mut self,
        sd: &StructDef<SemanticContext>,
    ) -> Result<TypeId, TypeTableError> {
        self.types.add_struct_def(sd)
    }

    /// Adds a new Imported Structure definition to the [`MirProject`].
    pub fn add_import_struct_def(
        &mut self,
        sd: &ImportStructDef,
    ) -> Result<TypeId, TypeTableError> {
        self.types.add_import_struct_def(sd)
    }

    /// Add a function to the table of static definitions. The MIR associated with
    /// the function can be added at a later time.
    pub fn add_func(&mut self, func: Procedure) -> Result<DefId, StaticDefinitionError> {
        self.static_defs.add_fn(func)
    }

    /// Get the definition of a specific static item.
    pub fn get_def(&self, id: DefId) -> &StaticItem {
        self.static_defs.get(id)
    }

    /// Get the definition of a specific static item.
    pub fn get_def_fn(&self, id: DefId) -> Option<&Procedure> {
        match self.static_defs.get(id) {
            StaticItem::Function(p) => Some(p),
            StaticItem::StringLiteral(_) => None,
        }
    }

    /// Adds a new [String Literal](StringId) to the static definition table of this project
    pub fn add_string_literal(&mut self, id: StringId) -> Result<DefId, StaticDefinitionError> {
        self.static_defs.add_string_literal(id)
    }

    /// Will return a [`StringId`] if the given [`DefId`] maps to a string literal, otherwise,
    /// this will return [`None`].
    pub fn get_def_string(&self, id: DefId) -> Option<StringId> {
        match self.static_defs.get(id) {
            StaticItem::Function(_) => None,
            StaticItem::StringLiteral(s) => Some(*s),
        }
    }

    /// Search the set of static definitions for an item with a [path](Path) that is equal
    /// to the given path.
    pub fn find_def(&self, path: &Path) -> Option<DefId> {
        self.static_defs.find(path)
    }

    /// Returns an [`Iterator`] over all the functions defined within this
    /// project.
    pub fn function_iter(&self) -> impl Iterator<Item = (DefId, &Procedure)> {
        self.static_defs.function_iter()
    }

    /// Returns an [`Iterator`] over all the types defined within this project.
    pub fn type_iter(&self) -> impl Iterator<Item = (TypeId, &MirTypeDef)> {
        self.types.iter()
    }
}

impl Display for MirProject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Print all the functions in the project
        for (idx, def) in self.static_defs.defs.iter().enumerate() {
            match def {
                StaticItem::Function(func) => {
                    f.write_fmt(format_args!("DefId: {} ::: {}\n", idx, func))?
                }
                StaticItem::StringLiteral(s) => {
                    f.write_fmt(format_args!("DefId: {} ::: StringID: {}\n", idx, s))?
                }
            }
        }

        // Print the type table
        f.write_str("\nTypes:\n")?;
        f.write_fmt(format_args!("{}", self.types))
    }
}

/// Represents definitions of static items within this project
/// This includes: functions and static variables and static constants.
/// This also includes static strings.
#[derive(Default)]
struct StaticDefinitions {
    defs: Vec<StaticItem>,
}

impl StaticDefinitions {
    /// Create a new table for keeping track of the static definition in a program
    fn new() -> StaticDefinitions {
        StaticDefinitions::default()
    }

    /// Will add the given procedure to the the StaticDefinitions table and return the
    /// [`DefId`] that has been assigned to the procedure. If an item with the same
    /// canonical path already exists in the table, then overrwrite with the new value
    /// and return the associated [`DefId`].
    fn add_fn(&mut self, func: Procedure) -> Result<DefId, StaticDefinitionError> {
        // Search through defs for item with the same canonical path
        if let Some(idx) = self.find(func.path()) {
            // If Found
            // If the item is not a procedure, then return an error
            // If the item is a procedure, then replace with the new value and return the DefId
            match &mut self.defs[idx.0 as usize] {
                StaticItem::Function(def) => {
                    *def = func;
                    Ok(idx)
                }
                StaticItem::StringLiteral(_) => Err(StaticDefinitionError::NotFunction),
            }
        } else {
            // If _not_ found then add to defs and return the DefId
            Ok(self.add_item(StaticItem::Function(func)))
        }
    }

    /// Adds a [`String Literal`](StringId) to the static memory table of this project.
    fn add_string_literal(&mut self, id: StringId) -> Result<DefId, StaticDefinitionError> {
        if let Some(s) = self.find_string_literal(id) {
            Ok(s)
        } else {
            Ok(self.add_item(StaticItem::StringLiteral(id)))
        }
    }

    fn add_item(&mut self, item: StaticItem) -> DefId {
        self.defs.push(item);
        let idx = self.defs.len() - 1;
        DefId::new(idx as u32)
    }

    /// Search this table for an item with the given [`Path`]. If one is found, then
    /// return the [`DefId`] of the item. Otherwise, return [`None`](`Option::None`).
    fn find(&self, path: &Path) -> Option<DefId> {
        let pos = self.defs.iter().position(|i| match i {
            StaticItem::Function(f) => f.path() == path,
            StaticItem::StringLiteral(_) => false,
        })?;
        Some(DefId::new(pos as u32))
    }

    /// Given a [`StringId`] return its static memory [`DefId`], if the [`StringId`]
    /// is not in the static memory abstraction then this will return [`None`].
    fn find_string_literal(&self, id: StringId) -> Option<DefId> {
        let pos = self.defs.iter().position(|i| match i {
            StaticItem::Function(_) => false,
            StaticItem::StringLiteral(s) => *s == id,
        })?;
        Some(DefId::new(pos as u32))
    }

    /// Return a reference to the item with the given [`DefId`].
    fn get(&self, id: DefId) -> &StaticItem {
        &self.defs[id.0 as usize]
    }

    /// Return an iterator over the functions that are defined in a  MIR Program.
    fn function_iter(&self) -> impl Iterator<Item = (DefId, &Procedure)> {
        self.defs.iter().enumerate().filter_map(|(id, i)| match i {
            StaticItem::Function(f) => Some((DefId(id as u32), f)),
            StaticItem::StringLiteral(_) => None,
        })
    }

    fn string_literal_iter(&self) -> impl Iterator<Item = (DefId, &StringId)> {
        self.defs.iter().enumerate().filter_map(|(id, i)| match i {
            StaticItem::Function(_) => None,
            StaticItem::StringLiteral(s) => Some((DefId(id as u32), s)),
        })
    }
}

/// Uniquely identifies an item that exists in the static memory of a program
/// e.g., a function or static variable.
#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone, Default)]
pub struct DefId(u32);

impl DefId {
    pub fn new(id: u32) -> DefId {
        DefId(id)
    }
}

impl Display for DefId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}

/// A static item, this could be a function or data.  Data in turn can be a
/// variable or a constant.
#[derive(Debug, PartialEq)]
pub enum StaticItem {
    Function(Procedure),
    StringLiteral(StringId),
}

#[derive(Debug)]
pub enum StaticDefinitionError {
    NotFunction,
}
