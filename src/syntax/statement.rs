use super::{ast::Ast, ty::Type};

pub struct Bind<M> {
    metadata: M,
    id: String,
    ty: Type,
    mutable: bool,
    lhs: Ast<M>,
}

impl<M> Bind<M> {
    pub fn new(metadata: M, id: &str, ty: Type, mutable: bool, lhs: Ast<M>) -> Bind<M> {
        Bind{
            metadata,
            id: id.into(),
            ty,
            mutable,
            lhs,
        }
    }

    pub fn get_id(&self) -> &str {
        &self.id
    }

    pub fn is_mutable(&self) -> bool {
        self.mutable
    }

    pub fn get_lhs(&self) -> &Ast<M> {
        &self.lhs
    }

    pub fn get_type(&self) -> &Type {
        &self.ty
    }
}