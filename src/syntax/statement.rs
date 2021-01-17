use super::{ast::Ast, ty::Type};

pub struct Bind<M> {
    metadata: M,
    id: String,
    ty: Type,
    mutable: bool,
    rhs: Ast<M>,
}

impl<M> Bind<M> {
    pub fn new(metadata: M, id: &str, ty: Type, mutable: bool, rhs: Ast<M>) -> Bind<M> {
        Bind{
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
}