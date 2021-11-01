use crate::{compiler::source::SourceIr, StringId};

use super::{
    node::{
        Context, Node, NodeType, {PostOrderIter, PreOrderIter},
    },
    parameter::Parameter,
    statement::Statement,
    ty::Type,
};

#[derive(Clone, Debug, PartialEq)]
pub struct RoutineDef<M> {
    pub context: M,
    pub def: RoutineDefType,
    pub name: StringId,
    pub params: Vec<Parameter<M>>,
    pub ret_ty: Type,
    pub body: Vec<Statement<M>>,
}

impl<M: Context> SourceIr for RoutineDef<M> {
    fn span(&self) -> crate::compiler::Span {
        self.context.span()
    }
}

impl<M: Context> Node<M> for RoutineDef<M> {
    fn context(&self) -> &M {
        &self.context
    }

    fn get_context_mut(&mut self) -> &mut M {
        &mut self.context
    }

    fn node_type(&self) -> NodeType {
        NodeType::RoutineDef(self.def)
    }

    fn children(&self) -> Vec<&dyn Node<M>> {
        let mut v: Vec<&dyn Node<M>> = vec![];
        for f in self.params.iter() {
            v.push(f);
        }
        for b in self.body.iter() {
            v.push(b);
        }

        v
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

impl<M> std::fmt::Display for RoutineDef<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.write_fmt(format_args!("{}", self.get_name()))
    }
}

impl<M> RoutineDef<M> {
    pub fn new_function(
        name: StringId,
        context: M,
        params: Vec<Parameter<M>>,
        ty: Type,
        body: Vec<Statement<M>>,
    ) -> RoutineDef<M> {
        RoutineDef {
            context,
            def: RoutineDefType::Function,
            name,
            params,
            ret_ty: ty,
            body,
        }
    }

    pub fn new_coroutine(
        name: StringId,
        context: M,
        params: Vec<Parameter<M>>,
        ty: Type,
        body: Vec<Statement<M>>,
    ) -> RoutineDef<M> {
        RoutineDef {
            context,
            def: RoutineDefType::Coroutine,
            name,
            params,
            ret_ty: ty,
            body,
        }
    }

    pub fn get_name(&self) -> StringId {
        self.name
    }

    pub fn get_params(&self) -> &Vec<Parameter<M>> {
        &self.params
    }

    pub fn get_params_mut(&mut self) -> &mut Vec<Parameter<M>> {
        &mut self.params
    }

    pub fn set_params(&mut self, params: Vec<Parameter<M>>) {
        self.params = params;
    }

    pub fn get_return_type(&self) -> &Type {
        &self.ret_ty
    }

    pub fn get_body(&self) -> &Vec<Statement<M>> {
        &self.body
    }

    pub fn get_body_mut(&mut self) -> &mut Vec<Statement<M>> {
        &mut self.body
    }

    pub fn get_def(&self) -> &RoutineDefType {
        &self.def
    }

    pub fn root_str(&self) -> String {
        format!("{} {}", self.def, self.name)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RoutineDefType {
    Function,
    Coroutine,
}

impl std::fmt::Display for RoutineDefType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        use RoutineDefType::*;
        match self {
            Coroutine => f.write_str("coroutine def"),
            Function => f.write_str("function def"),
        }
    }
}
