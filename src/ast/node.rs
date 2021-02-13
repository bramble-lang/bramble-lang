use std::fmt::Display;

use super::annotate::{Annotation, iter::{PostOrderIter, PreOrderIter}};

pub trait Node<M: Annotation> {
    fn node_type(&self) -> NodeType;
    fn annotation(&self) -> &M;
    fn annotation_mut(&mut self) -> &mut M;
    fn children(&self) -> Vec<&dyn Node<M>>;
    fn name(&self) -> Option<&str>;

    fn iter_postorder(&self) -> PostOrderIter<M>;
    fn iter_preorder(&self) -> PreOrderIter<M>;
}

pub enum NodeType {
    Module,
    FnDef,
    CoroutineDef,
    StructDef,
    Parameter,
    Expression,
    Statement,
    RoutineCall,
    BinOp,
}

impl Display for NodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NodeType::Module => f.write_str("module"),
            NodeType::FnDef => f.write_str("fn"),
            NodeType::CoroutineDef => f.write_str("co"),
            NodeType::StructDef => f.write_str("struct"),
            NodeType::Parameter => f.write_str("parameter"),
            NodeType::Expression => f.write_str("exp"),
            NodeType::Statement => f.write_str("stm"),
            NodeType::RoutineCall => f.write_str("call"),
            NodeType::BinOp => f.write_str("bin op")
        }
    }
}