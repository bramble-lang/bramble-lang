use crate::ast::node::Node;

use super::Annotation;

pub struct PostOrderIter<'a, A>
where
    A: Annotation,
{
    out: Vec<&'a dyn Node<A>>,
}

/**
Performs a Post Order DFS traversal over all the nodes in an AST.

TODO: This will do the full traversal when `new` is called, this can be
improved to amortize time while reducing storage costs by lazily populating
`out`
*/
impl<'a, A> PostOrderIter<'a, A> where A: Annotation {
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

        PostOrderIter {
            out,
        }
    }
}

impl<'a, A> Iterator for PostOrderIter<'a, A> where A: std::fmt::Debug + Annotation {
    type Item = &'a dyn Node<A>;
    fn next(&mut self) -> Option<Self::Item> {
        self.out.pop()
    }
}