use super::{ast::Ast, ty::Type};

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<M> {
    Bind(Box<Ast<M>>),
    Mutate(Box<Ast<M>>),
    Return(Box<Ast<M>>),
    Yield(Box<Ast<M>>),
    YieldReturn(Box<Ast<M>>),
    Printi(Box<Ast<M>>),
    Printiln(Box<Ast<M>>),
    Printbln(Box<Ast<M>>),
    Prints(Box<Ast<M>>),
}

impl<M> Statement<M> {
    pub fn get_metadata(&self) -> &M {
        use Statement::*;

        match self {
            Mutate(x) | Return(x) | Yield(x) | YieldReturn(x) | Printi(x) | Printiln(x)
            | Printbln(x) | Prints(x) => x.get_metadata(),
            Bind(b) => b.get_metadata(),
        }
    }

    pub fn get_metadata_mut(&mut self) -> &mut M {
        use Statement::*;

        match self {
            Mutate(x) | Return(x) | Yield(x) | YieldReturn(x) | Printi(x) | Printiln(x)
            | Printbln(x) | Prints(x) => x.get_metadata_mut(),
            Bind(b) => b.get_metadata_mut(),
        }
    }

    pub fn root_str(&self) -> String {
        use Statement::*;

        match self {
            Mutate(x) | Return(x) | Yield(x) | YieldReturn(x) | Printi(x) | Printiln(x)
            | Printbln(x) | Prints(x) => x.root_str(),
            Bind(b) => b.root_str(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Bind<M> {
    metadata: M,
    id: String,
    ty: Type,
    mutable: bool,
    rhs: Ast<M>,
}

impl<M> Bind<M> {
    pub fn new(metadata: M, id: &str, ty: Type, mutable: bool, rhs: Ast<M>) -> Bind<M> {
        Bind {
            metadata,
            id: id.into(),
            ty,
            mutable,
            rhs,
        }
    }

    pub fn get_id(&self) -> &str {
        &self.id
    }

    pub fn get_metadata(&self) -> &M {
        &self.metadata
    }

    pub fn get_metadata_mut(&mut self) -> &mut M {
        &mut self.metadata
    }

    pub fn is_mutable(&self) -> bool {
        self.mutable
    }

    pub fn get_rhs(&self) -> &Ast<M> {
        &self.rhs
    }

    pub fn get_type(&self) -> &Type {
        &self.ty
    }

    pub fn root_str(&self) -> String {
        format!("bind {}", self.id)
    }
}
