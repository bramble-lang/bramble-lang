pub trait Node<M> {
    fn annotation(&self) -> &M;
    fn annotation_mut(&mut self) -> &mut M;
}
