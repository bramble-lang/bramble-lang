use crate::ast::node::Node;

use super::Annotation;

pub struct PostOrderIter<'a, A>
where
    A: std::fmt::Debug + Annotation,
{
    stack: Vec<&'a dyn Node<A>>,
    out: Vec<&'a dyn Node<A>>,
}

impl<'a, A> PostOrderIter<'a, A> where A: std::fmt::Debug + Annotation {
    pub fn new(node: &'a dyn Node<A>) -> PostOrderIter<'a, A> {
        PostOrderIter {
            stack: vec![node],
            out: vec![],
        }
    }

    pub fn next(&mut self) -> Option<&'a dyn Node<A>> {
        match self.stack.pop() {
            Some(n) => (),
            None => ()
        }

        None
    }
}