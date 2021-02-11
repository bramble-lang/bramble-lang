pub trait Node<M> {
    fn get_annotation(&self) -> &M;
    fn get_annotation_mut(&mut self) -> & mut M;
}