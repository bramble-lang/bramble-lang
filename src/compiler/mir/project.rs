/*!
Represents and entire Bramble program, including imported libraries,
in MIR form.
*/

use crate::compiler::{ast::{Type, StructDef}, semantics::semanticnode::SemanticContext};

use super::{typetable::{TypeTable, TypeId, MirTypeDef, TypeTableError}, ir::Procedure};

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
    types: TypeTable,
}

impl MirProject {
    pub fn new() -> MirProject {
        MirProject {
            types: TypeTable::new(),
        }
    }

    /// Searches the [`TypeTable`] for the [`TypeId`] of the given
    /// [`Type`].
    pub fn find_type(&self, ty: &Type) -> Option<TypeId> {
        self.types.find(ty)
    }

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
}

/// Represents definitions of static items within this project
/// This includes: functions and static variables and static constants.
/// This also includes static strings.
struct StaticDefinitions {
    defs: Vec<()>,
}

/// Uniquely identifies an item that exists in the static memory of a program
/// e.g., a function or static variable.
#[derive(Debug, Copy, Clone, Default)]
struct DefId(u32);

impl DefId {
    fn new(id: u32) -> DefId {
        DefId(id)
    }
}

/// A static item, this could be a function or data.  Data in turn can be a
/// variable or a constant.
enum StaticItem{
    Function(Procedure),
}
