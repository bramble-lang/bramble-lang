use super::{node::Node, parameter::Parameter, statement::Statement, ty::Type};

#[derive(Clone, Debug, PartialEq)]
pub struct RoutineDef<M> {
    pub annotations: M,
    pub def: RoutineDefType,
    pub name: String,
    pub params: Vec<Parameter<M>>,
    pub ty: Type,
    pub body: Vec<Statement<M>>,
}

impl<M> Node<M> for RoutineDef<M> {
    fn annotation(&self) -> &M {
        &self.annotations
    }

    fn annotation_mut(&mut self) -> &mut M {
        &mut self.annotations
    }
}

impl<M> std::fmt::Display for RoutineDef<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.write_str(self.get_name())
    }
}

impl<M> RoutineDef<M> {
    pub fn new_function(
        name: &str,
        annotations: M,
        params: Vec<Parameter<M>>,
        ty: Type,
        body: Vec<Statement<M>>,
    ) -> RoutineDef<M> {
        RoutineDef {
            annotations,
            def: RoutineDefType::Function,
            name: name.into(),
            params,
            ty,
            body,
        }
    }

    pub fn new_coroutine(
        name: &str,
        annotations: M,
        params: Vec<Parameter<M>>,
        ty: Type,
        body: Vec<Statement<M>>,
    ) -> RoutineDef<M> {
        RoutineDef {
            annotations,
            def: RoutineDefType::Coroutine,
            name: name.into(),
            params,
            ty,
            body,
        }
    }

    pub fn get_annotations(&self) -> &M {
        &self.annotations
    }

    pub fn get_annotations_mut(&mut self) -> &mut M {
        &mut self.annotations
    }

    pub fn get_name(&self) -> &str {
        &self.name
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
        &self.ty
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
