use super::{ast::Ast, ty::Type};

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<M> {
    Bind(Bind<M>),
    Mutate(Ast<M>),
    Return(Ast<M>),
    Yield(Ast<M>),
    YieldReturn(Ast<M>),
    Printi(Ast<M>),
    Printiln(Ast<M>),
    Printbln(Ast<M>),
    Prints(Ast<M>),
}

impl<M> Statement<M> {
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
