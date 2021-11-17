use std::fmt::Display;

mod iter;
mod map;

use crate::compiler::source::SourceIr;
use crate::compiler::Span;
use crate::StringId;

pub use self::iter::{PostOrderIter, PreOrderIter};
pub use self::map::MapPreOrder;

use super::routinedef::RoutineDefType;

/// Trait that categorizes all AST IR types and through which general operations
/// on an AST can be written.
pub trait Node<M: Context>: SourceIr {
    fn node_type(&self) -> NodeType;
    fn context(&self) -> &M;
    fn get_context_mut(&mut self) -> &mut M;
    fn children(&self) -> Vec<&dyn Node<M>>;
    fn name(&self) -> Option<StringId>;

    fn iter_postorder(&self) -> PostOrderIter<M>;
    fn iter_preorder(&self) -> PreOrderIter<M>;
}

/// Describes the specific type of an AST node.  This is used because when
/// writing code against the [`Node`] trait, the specific node type information
/// is lost.
#[derive(Debug)]
pub enum NodeType {
    Module,
    RoutineDef(RoutineDefType),
    StructDef,
    Parameter,
    Expression,
    Statement,
    RoutineCall,
    BinOp,
    Extern,
}

impl Display for NodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NodeType::Module => f.write_str("module"),
            NodeType::RoutineDef(RoutineDefType::Function) => f.write_str("fn"),
            NodeType::RoutineDef(RoutineDefType::Coroutine) => f.write_str("co"),
            NodeType::StructDef => f.write_str("struct"),
            NodeType::Parameter => f.write_str("parameter"),
            NodeType::Expression => f.write_str("exp"),
            NodeType::Statement => f.write_str("stm"),
            NodeType::RoutineCall => f.write_str("call"),
            NodeType::BinOp => f.write_str("bin op"),
            NodeType::Extern => f.write_str("extern"),
        }
    }
}

/// A [`Context`] is a type which contains contextual information related to
/// a specific stage or analysis performed by the compiler.
pub trait Context {
    fn id(&self) -> u32;
    fn span(&self) -> Span;
}
