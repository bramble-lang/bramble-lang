use crate::StringId;

use super::{
    node::{
        Context, Node, NodeType, {PostOrderIter, PreOrderIter},
    },
    parameter::Parameter,
    ty::Type,
};

pub type HasVarArgs = bool;

#[derive(Clone, Debug, PartialEq)]
pub struct Extern<M> {
    pub context: M,
    pub name: StringId,
    pub params: Vec<Parameter<M>>,
    pub has_varargs: HasVarArgs,
    pub ty: Type,
}

impl<M: Context> Node<M> for Extern<M> {
    fn context(&self) -> &M {
        &self.context
    }

    fn get_context_mut(&mut self) -> &mut M {
        &mut self.context
    }

    fn node_type(&self) -> NodeType {
        NodeType::Extern
    }

    fn children(&self) -> Vec<&dyn Node<M>> {
        vec![]
    }

    fn name(&self) -> Option<StringId> {
        Some(self.name)
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
        f.write_fmt(format_args!("{}", self.name))
    }
}

impl<M> Extern<M> {
    pub fn new(
        name: StringId,
        context: M,
        params: Vec<Parameter<M>>,
        has_varargs: bool,
        ty: Type,
    ) -> Extern<M> {
        Extern {
            context,
            name,
            params,
            has_varargs,
            ty,
        }
    }

    pub fn get_name(&self) -> StringId {
        self.name
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
