use super::{ast::Ast, ty::Type};

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

#[derive(Clone, Debug, PartialEq)]
pub struct RoutineDef<M> {
    pub meta: M,
    pub def: RoutineDefType,
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub ty: Type,
    pub body: Vec<Ast<M>>,
}

impl<M> RoutineDef<M> {
    pub fn new_function(
        name: &str,
        metadata: M,
        params: Vec<(String, Type)>,
        ty: Type,
        body: Vec<Ast<M>>,
    ) -> RoutineDef<M> {
        RoutineDef {
            meta: metadata,
            def: RoutineDefType::Function,
            name: name.into(),
            params,
            ty,
            body,
        }
    }

    pub fn new_coroutine(
        name: &str,
        metadata: M,
        params: Vec<(String, Type)>,
        ty: Type,
        body: Vec<Ast<M>>,
    ) -> RoutineDef<M> {
        RoutineDef {
            meta: metadata,
            def: RoutineDefType::Coroutine,
            name: name.into(),
            params,
            ty,
            body,
        }
    }

    pub fn get_metadata(&self) -> &M {
        &self.meta
    }

    pub fn get_metadata_mut(&mut self) -> &mut M {
        &mut self.meta
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_params(&self) -> &Vec<(String, Type)> {
        &self.params
    }

    pub fn set_params(&mut self, params: Vec<(String, Type)>) {
        self.params = params;
    }

    pub fn get_return_type(&self) -> &Type {
        &self.ty
    }

    pub fn get_body(&self) -> &Vec<Ast<M>> {
        &self.body
    }

    pub fn get_def(&self) -> &RoutineDefType {
        &self.def
    }

    pub fn root_str(&self) -> String {
        format!("{} {}", self.def, self.name)
    }
}
