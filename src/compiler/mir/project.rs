/*!
Represents an entire Bramble program, including imported libraries,
in MIR form.
*/

use std::fmt::Display;

use crate::compiler::{
    ast::{Path, StructDef, Type},
    semantics::semanticnode::SemanticContext,
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

    /// Adds a new Structure definition to the [`MirProject`].
    pub fn add_struct_def(
        &mut self,
        sd: &StructDef<SemanticContext>,
    ) -> Result<(), TypeTableError> {
        self.types.add_struct_def(sd)?;
        Ok(())
    }

    /// Add a function to the table of static definitions. The MIR associated with
    /// the function can be added at a later time.
    pub fn add_func(&mut self, func: Procedure) -> Result<DefId, ()> {
        self.static_defs.add_fn(func)
    }

    /// Get the definition of a specific static item.
    pub fn get_def(&self, id: DefId) -> &StaticItem {
        self.static_defs.get(id)
    }

    /// Search the set of static definitions for an item with a [path](Path) that is equal
    /// to the given path.
    pub fn find_def(&mut self, path: &Path) -> Option<DefId> {
        self.static_defs.find(path)
    }
}

impl Display for MirProject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for def in &self.static_defs.defs {
            match def {
                StaticItem::Function(func) => f.write_fmt(format_args!("{}", func))?,
            }
        }
        Ok(())
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
    fn add_fn(&mut self, func: Procedure) -> Result<DefId, ()> {
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
            }
        } else {
            // If _not_ found then add to defs and return the DefId
            self.defs.push(StaticItem::Function(func));
            let idx = self.defs.len() - 1;
            Ok(DefId::new(idx as u32))
        }
    }

    /// Search this table for an item with the given [`Path`]. If one is found, then
    /// return the [`DefId`] of the item. Otherwise, return [`None`](`Option::None`).
    fn find(&self, path: &Path) -> Option<DefId> {
        let pos = self.defs.iter().position(|i| match i {
            StaticItem::Function(f) => f.path() == path,
        })?;
        Some(DefId::new(pos as u32))
    }

    /// Return a reference to the item with the given [`DefId`].
    fn get(&self, id: DefId) -> &StaticItem {
        &self.defs[id.0 as usize]
    }
}

/// Uniquely identifies an item that exists in the static memory of a program
/// e.g., a function or static variable.
#[derive(PartialEq, Debug, Copy, Clone, Default)]
pub struct DefId(u32);

impl DefId {
    fn new(id: u32) -> DefId {
        DefId(id)
    }
}

/// A static item, this could be a function or data.  Data in turn can be a
/// variable or a constant.
#[derive(Debug, PartialEq)]
pub enum StaticItem {
    Function(Procedure),
}