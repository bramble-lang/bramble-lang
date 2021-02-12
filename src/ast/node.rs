use super::annotate::{Annotation, iter::PostOrderIter};

pub trait Node<M: Annotation> {
    fn node_type(&self) -> NodeType;
    fn annotation(&self) -> &M;
    fn annotation_mut(&mut self) -> &mut M;
    fn children(&self) -> Vec<&dyn Node<M>>;
    fn name(&self) -> Option<&str>;

    fn iter(&self) -> PostOrderIter<M>;
}

pub enum NodeType {
    Module,
    FnDef,
    CoroutineDef,
    StructDef,
    Parameter,
    Expression,
    Statement,
}