use crate::ast::node::Node;

use super::Annotation;

pub struct PostOrderIter<'a, A>
where
    A: Annotation,
{
    out: Vec<&'a dyn Node<A>>,
}

/**
Performs a Post Order traversal of an AST and applies the given function,
`f`, on each node.  This construct exists to allow a user to easily traverse
through an AST using `for` loops and print out diagnostic or debugging
information. Using this iterator will transform the AST into an ordered
list of nodes and does not preserve the AST topology

For Compiler transformations, use the `MapPostOrder` construct.
*/
impl<'a, A> PostOrderIter<'a, A>
where
    A: Annotation,
{
    pub fn new(node: &'a dyn Node<A>) -> PostOrderIter<'a, A> {
        let mut stack = vec![node];
        let mut out = vec![];

        // Build a Post Ordered vector of references to Nodes
        while let Some(n) = stack.pop() {
            out.push(n);
            let children = n.children();
            for c in children {
                stack.push(c);
            }
        }

        PostOrderIter { out }
    }
}

impl<'a, A> Iterator for PostOrderIter<'a, A>
where
    A: std::fmt::Debug + Annotation,
{
    type Item = &'a dyn Node<A>;
    fn next(&mut self) -> Option<Self::Item> {
        self.out.pop()
    }
}

/**
Performs a Pre Order traversal of an AST and applies the given function,
`f`, on each node.  This construct exists to allow a user to easily traverse
through an AST using `for` loops and print out diagnostic or debugging
information. Using this iterator will transform the AST into an ordered
list of nodes and does not preserve the AST topology.

For Compiler transformations, use the `MapPostOrder` construct.
*/
pub struct PreOrderIter<'a, A>
where
    A: Annotation,
{
    out: Vec<&'a dyn Node<A>>,
}

impl<'a, A> PreOrderIter<'a, A>
where
    A: Annotation,
{
    pub fn new(node: &'a dyn Node<A>) -> PreOrderIter<'a, A> {
        let mut stack = vec![node];
        let mut out = vec![];

        // Build a Post Ordered vector of references to Nodes
        while let Some(n) = stack.pop() {
            out.push(n);
            let children = n.children().into_iter().rev();
            for c in children {
                stack.push(c);
            }
        }

        out.reverse();

        PreOrderIter { out }
    }
}

impl<'a, A> Iterator for PreOrderIter<'a, A>
where
    A: std::fmt::Debug + Annotation,
{
    type Item = &'a dyn Node<A>;
    fn next(&mut self) -> Option<Self::Item> {
        self.out.pop()
    }
}
