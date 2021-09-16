mod expression;
mod extern_decl;
mod module;
mod node;
mod parameter;
mod path;
mod routinedef;
mod statement;
mod structdef;
mod ty;

pub use self::expression::{BinaryOperator, Expression, RoutineCall, UnaryOperator};
pub use self::extern_decl::{Extern, HasVarArgs};
pub use self::module::{Item, Module};
pub use self::node::{Context, MapPreOrder, Node, NodeType, PostOrderIter, PreOrderIter};
pub use self::parameter::Parameter;
pub use self::path::{Element, Path, CANONICAL_ROOT, ROOT_PATH, SELF};
pub use self::routinedef::{RoutineDef, RoutineDefType};
pub use self::statement::{Bind, Mutate, Return, Statement, YieldReturn};
pub use self::structdef::StructDef;
pub use self::ty::Type;

use super::lexer::stringtable::StringId;
use super::{CompilerDisplay, CompilerDisplayError};

pub const MAIN_MODULE: &str = "main";

/// Covers errors that can happen when creating or modifying an AST value.
#[derive(Clone, Debug, PartialEq)]
pub enum AstError {
    ModuleAlreadyContains(StringId),
}

impl CompilerDisplay for AstError {
    fn fmt(&self, st: &crate::StringTable) -> Result<String, CompilerDisplayError> {
        match self {
            AstError::ModuleAlreadyContains(sid) => {
                let s = st.get(*sid)?;
                Ok(format!("{} already exists in module", s))
            }
        }
    }
}

/// Errors that can be generated when using or creating Path values.
#[derive(Clone, Debug, PartialEq)]
pub enum PathError {
    SubsedingRoot,
}

impl CompilerDisplay for PathError {
    fn fmt(&self, _st: &crate::StringTable) -> Result<String, CompilerDisplayError> {
        match self {
            PathError::SubsedingRoot => Ok(format!(
                "Use of super would exceed the current depth of the path"
            )),
        }
    }
}
