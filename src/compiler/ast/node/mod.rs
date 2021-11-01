use std::fmt::Display;

mod iter;
mod map;

use crate::compiler::source::HasSpan;
use crate::compiler::Span;
use crate::StringId;

pub use self::iter::{PostOrderIter, PreOrderIter};
pub use self::map::MapPreOrder;

use super::routinedef::RoutineDefType;

pub trait Node<M: Context>: HasSpan {
    fn node_type(&self) -> NodeType;
    fn context(&self) -> &M;
    fn get_context_mut(&mut self) -> &mut M;
    fn children(&self) -> Vec<&dyn Node<M>>;
    fn name(&self) -> Option<StringId>;

    fn iter_postorder(&self) -> PostOrderIter<M>;
    fn iter_preorder(&self) -> PreOrderIter<M>;
}

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

pub trait Context {
    fn id(&self) -> u32;
    fn line(&self) -> u32;
    fn span(&self) -> Span;
}
