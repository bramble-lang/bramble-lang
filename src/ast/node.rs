pub trait Node<M> {
    fn node_type(&self) -> NodeType;
    fn annotation(&self) -> &M;
    fn annotation_mut(&mut self) -> &mut M;
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