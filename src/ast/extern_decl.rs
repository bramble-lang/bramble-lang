use super::{
    node::{
        Annotation, Node, NodeType, {PostOrderIter, PreOrderIter},
    },
    parameter::Parameter,
    ty::Type,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Extern<M> {
    pub annotations: M,
    pub name: String,
    pub params: Vec<Parameter<M>>,
    pub has_varargs: bool,
    pub ty: Type,
}

impl<M: Annotation> Node<M> for Extern<M> {
    fn annotation(&self) -> &M {
        &self.annotations
    }

    fn annotation_mut(&mut self) -> &mut M {
        &mut self.annotations
    }

    fn node_type(&self) -> NodeType {
        NodeType::Extern
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

impl<M> std::fmt::Display for Extern<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.write_str(&self.name)
    }
}

impl<M> Extern<M> {
    pub fn new(
        name: &str,
        annotations: M,
        params: Vec<Parameter<M>>,
        has_varargs: bool,
        ty: Type,
    ) -> Extern<M> {
        Extern {
            annotations,
            name: name.into(),
            params,
            has_varargs,
            ty,
        }
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_params(&self) -> &Vec<Parameter<M>> {
        &self.params
    }

    pub fn get_return_type(&self) -> &Type {
        &self.ty
    }

    pub fn root_str(&self) -> String {
        format!("extern fn {}", self.name)
    }
}
