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

#[derive(Clone, Debug, PartialEq)]
pub enum AstError {
    ModuleAlreadyContains(StringId),
    PathTooSuper,
}

impl CompilerDisplay for AstError {
    fn fmt(&self, st: &crate::StringTable) -> Result<String, CompilerDisplayError> {
        match self {
            AstError::ModuleAlreadyContains(sid) => {
                let s = st.get(*sid)?;
                Ok(format!("{} already exists in module", s))
            }
            AstError::PathTooSuper => Ok(format!(
                "Use of super would exceed the current depth of the path"
            )),
        }
    }
}
