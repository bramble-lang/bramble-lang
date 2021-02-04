use super::{expression::Expression, ty::Type};

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<M> {
    Bind(Box<Bind<M>>),
    Mutate(Box<Mutate<M>>),

    YieldReturn(Box<YieldReturn<M>>),
    Expression(Box<Expression<M>>),

    Return(Box<Return<M>>),
}

impl<M> Statement<M> {
    pub fn get_annotations(&self) -> &M {
        use Statement::*;

        match self {
            Return(x) => x.get_annotations(),
            YieldReturn(x) => x.get_annotations(),
            Expression(e) => e.get_annotations(),
            Bind(b) => b.get_annotations(),
            Mutate(m) => m.get_annotations(),
        }
    }

    pub fn from_ast(ast: Expression<M>) -> Option<Statement<M>> {
        match ast {
            _ => Some(Statement::Expression(Box::new(ast))),
        }
    }

    pub fn get_annotations_mut(&mut self) -> &mut M {
        use Statement::*;

        match self {
            Return(x) => x.get_annotations_mut(),
            YieldReturn(x) => x.get_annotations_mut(),
            Expression(e) => e.get_annotations_mut(),
            Bind(b) => b.get_annotations_mut(),
            Mutate(m) => m.get_annotations_mut(),
        }
    }

    pub fn root_str(&self) -> String {
        use Statement::*;

        match self {
            Return(x) => x.root_str(),
            YieldReturn(x) => x.root_str(),
            Expression(e) => e.root_str(),
            Bind(b) => b.root_str(),
            Mutate(m) => m.root_str(),
        }
    }

    pub fn get_name(&self) -> String {
        self.root_str()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Bind<M> {
    annotations: M,
    id: String,
    ty: Type,
    mutable: bool,
    rhs: Expression<M>,
}

impl<M> Bind<M> {
    pub fn new(annotations: M, id: &str, ty: Type, mutable: bool, rhs: Expression<M>) -> Bind<M> {
        Bind {
            annotations,
            id: id.into(),
            ty,
            mutable,
            rhs,
        }
    }

    pub fn get_id(&self) -> &str {
        &self.id
    }

    pub fn get_annotations(&self) -> &M {
        &self.annotations
    }

    pub fn get_annotations_mut(&mut self) -> &mut M {
        &mut self.annotations
    }

    pub fn is_mutable(&self) -> bool {
        self.mutable
    }

    pub fn get_rhs(&self) -> &Expression<M> {
        &self.rhs
    }

    pub fn get_rhs_mut(&mut self) -> &mut Expression<M> {
        &mut self.rhs
    }

    pub fn get_type(&self) -> &Type {
        &self.ty
    }

    pub fn root_str(&self) -> String {
        format!("bind {}", self.id)
    }

    pub fn get_name(&self) -> String {
        self.root_str()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Mutate<M> {
    annotations: M,
    id: String,
    rhs: Expression<M>,
}

impl<M> Mutate<M> {
    pub fn new(annotations: M, id: &str, rhs: Expression<M>) -> Self {
        Mutate {
            annotations,
            id: id.into(),
            rhs,
        }
    }

    pub fn get_id(&self) -> &str {
        &self.id
    }

    pub fn get_annotations(&self) -> &M {
        &self.annotations
    }

    pub fn get_annotations_mut(&mut self) -> &mut M {
        &mut self.annotations
    }

    pub fn get_rhs(&self) -> &Expression<M> {
        &self.rhs
    }

    pub fn get_rhs_mut(&mut self) -> &mut Expression<M> {
        &mut self.rhs
    }

    pub fn root_str(&self) -> String {
        format!("mut {}", self.id)
    }

    pub fn get_name(&self) -> String {
        self.root_str()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Yield<M> {
    annotations: M,
    value: Expression<M>,
}

impl<M> Yield<M> {
    pub fn new(annotations: M, value: Expression<M>) -> Self {
        Self { annotations, value }
    }

    pub fn get_annotations(&self) -> &M {
        &self.annotations
    }

    pub fn get_annotations_mut(&mut self) -> &mut M {
        &mut self.annotations
    }

    pub fn get_value(&self) -> &Expression<M> {
        &self.value
    }

    pub fn root_str(&self) -> String {
        format!("yield")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct YieldReturn<M> {
    annotations: M,
    value: Option<Expression<M>>,
}

impl<M> YieldReturn<M> {
    pub fn new(annotations: M, value: Option<Expression<M>>) -> Self {
        Self { annotations, value }
    }

    pub fn get_annotations(&self) -> &M {
        &self.annotations
    }

    pub fn get_annotations_mut(&mut self) -> &mut M {
        &mut self.annotations
    }

    pub fn get_value(&self) -> &Option<Expression<M>> {
        &self.value
    }

    pub fn get_value_mut(&mut self) -> &mut Option<Expression<M>> {
        &mut self.value
    }

    pub fn root_str(&self) -> String {
        format!("yret")
    }

    pub fn get_name(&self) -> String {
        self.root_str()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Return<M> {
    annotations: M,
    value: Option<Expression<M>>,
}

impl<M> Return<M> {
    pub fn new(annotations: M, value: Option<Expression<M>>) -> Self {
        Self { annotations, value }
    }

    pub fn get_annotations(&self) -> &M {
        &self.annotations
    }

    pub fn get_annotations_mut(&mut self) -> &mut M {
        &mut self.annotations
    }

    pub fn get_value(&self) -> &Option<Expression<M>> {
        &self.value
    }

    pub fn get_value_mut(&mut self) -> &mut Option<Expression<M>> {
        &mut self.value
    }

    pub fn root_str(&self) -> String {
        format!("return")
    }

    pub fn get_name(&self) -> String {
        self.root_str()
    }
}
