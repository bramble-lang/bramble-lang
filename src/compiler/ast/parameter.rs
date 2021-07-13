use serde::{Deserialize, Serialize};

use super::{
    node::{
        Context, Node, NodeType, {PostOrderIter, PreOrderIter},
    },
    ty::Type,
};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Parameter<M> {
    pub context: M,
    pub name: String,
    pub ty: Type,
}

impl<M: Context> Node<M> for Parameter<M> {
    fn get_context(&self) -> &M {
        &self.context
    }

    fn annotation_mut(&mut self) -> &mut M {
        &mut self.context
    }

    fn node_type(&self) -> NodeType {
        NodeType::Parameter
    }

    fn children(&self) -> Vec<&dyn Node<M>> {
        vec![]
    }

    fn name(&self) -> Option<&str> {
        Some(&self.name)
    }

    fn iter_postorder(&self) -> PostOrderIter<M> {
        PostOrderIter::new(self)
    }

    fn iter_preorder(&self) -> PreOrderIter<M> {
        PreOrderIter::new(self)
    }
}

impl<M> Parameter<M> {
    pub fn new(a: M, name: &str, ty: &Type) -> Parameter<M> {
        Parameter {
            context: a,
            name: name.into(),
            ty: ty.clone(),
        }
    }

    pub fn root_str(&self) -> String {
        format!("{}:{}", self.name, self.ty)
    }

    pub fn map_annotation<F, N>(&self, mut f: F) -> Parameter<N>
    where
        F: FnMut(&M) -> N,
    {
        Parameter {
            context: f(&self.context),
            name: self.name.clone(),
            ty: self.ty.clone(),
        }
    }
}

impl<M> std::fmt::Display for Parameter<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.write_str(&self.root_str())
    }
}
