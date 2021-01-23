use super::{expression::Expression, ty::Type};

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<M> {
    Bind(Box<Bind<M>>),
    Mutate(Box<Mutate<M>>),

    Printi(Box<Printi<M>>),
    Printiln(Box<Printiln<M>>),
    Printbln(Box<Printbln<M>>),
    Prints(Box<Prints<M>>),

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
            Printi(m) => m.get_annotations(),
            Printiln(m) => m.get_annotations(),
            Printbln(m) => m.get_annotations(),
            Prints(m) => m.get_annotations(),
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
            Printi(m) => m.get_annotations_mut(),
            Printiln(m) => m.get_annotations_mut(),
            Printbln(m) => m.get_annotations_mut(),
            Prints(m) => m.get_annotations_mut(),
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
            Printi(m) => m.root_str(),
            Printiln(m) => m.root_str(),
            Printbln(m) => m.root_str(),
            Prints(m) => m.root_str(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Bind<M> {
    metadata: M,
    id: String,
    ty: Type,
    mutable: bool,
    rhs: Expression<M>,
}

impl<M> Bind<M> {
    pub fn new(metadata: M, id: &str, ty: Type, mutable: bool, rhs: Expression<M>) -> Bind<M> {
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

    pub fn get_annotations(&self) -> &M {
        &self.metadata
    }

    pub fn get_annotations_mut(&mut self) -> &mut M {
        &mut self.metadata
    }

    pub fn is_mutable(&self) -> bool {
        self.mutable
    }

    pub fn get_rhs(&self) -> &Expression<M> {
        &self.rhs
    }

    pub fn get_type(&self) -> &Type {
        &self.ty
    }

    pub fn root_str(&self) -> String {
        format!("bind {}", self.id)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Mutate<M> {
    metadata: M,
    id: String,
    rhs: Expression<M>,
}

impl<M> Mutate<M> {
    pub fn new(metadata: M, id: &str, rhs: Expression<M>) -> Self {
        Mutate {
            metadata,
            id: id.into(),
            rhs,
        }
    }

    pub fn get_id(&self) -> &str {
        &self.id
    }

    pub fn get_annotations(&self) -> &M {
        &self.metadata
    }

    pub fn get_annotations_mut(&mut self) -> &mut M {
        &mut self.metadata
    }

    pub fn get_rhs(&self) -> &Expression<M> {
        &self.rhs
    }

    pub fn root_str(&self) -> String {
        format!("mut {}", self.id)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Printi<M> {
    metadata: M,
    value: Expression<M>,
}

impl<M> Printi<M> {
    pub fn new(metadata: M, value: Expression<M>) -> Self {
        Self { metadata, value }
    }

    pub fn get_annotations(&self) -> &M {
        &self.metadata
    }

    pub fn get_annotations_mut(&mut self) -> &mut M {
        &mut self.metadata
    }

    pub fn get_value(&self) -> &Expression<M> {
        &self.value
    }

    pub fn root_str(&self) -> String {
        format!("printi")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Printiln<M> {
    metadata: M,
    value: Expression<M>,
}

impl<M> Printiln<M> {
    pub fn new(metadata: M, value: Expression<M>) -> Self {
        Self { metadata, value }
    }

    pub fn get_annotations(&self) -> &M {
        &self.metadata
    }

    pub fn get_annotations_mut(&mut self) -> &mut M {
        &mut self.metadata
    }

    pub fn get_value(&self) -> &Expression<M> {
        &self.value
    }

    pub fn root_str(&self) -> String {
        format!("printiln")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Printbln<M> {
    metadata: M,
    value: Expression<M>,
}

impl<M> Printbln<M> {
    pub fn new(metadata: M, value: Expression<M>) -> Self {
        Self { metadata, value }
    }

    pub fn get_annotations(&self) -> &M {
        &self.metadata
    }

    pub fn get_annotations_mut(&mut self) -> &mut M {
        &mut self.metadata
    }

    pub fn get_value(&self) -> &Expression<M> {
        &self.value
    }

    pub fn root_str(&self) -> String {
        format!("printbln")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Prints<M> {
    metadata: M,
    value: Expression<M>,
}

impl<M> Prints<M> {
    pub fn new(metadata: M, value: Expression<M>) -> Self {
        Self { metadata, value }
    }

    pub fn get_annotations(&self) -> &M {
        &self.metadata
    }

    pub fn get_annotations_mut(&mut self) -> &mut M {
        &mut self.metadata
    }

    pub fn get_value(&self) -> &Expression<M> {
        &self.value
    }

    pub fn root_str(&self) -> String {
        format!("prints")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Yield<M> {
    metadata: M,
    value: Expression<M>,
}

impl<M> Yield<M> {
    pub fn new(metadata: M, value: Expression<M>) -> Self {
        Self { metadata, value }
    }

    pub fn get_annotations(&self) -> &M {
        &self.metadata
    }

    pub fn get_annotations_mut(&mut self) -> &mut M {
        &mut self.metadata
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
    metadata: M,
    value: Option<Expression<M>>,
}

impl<M> YieldReturn<M> {
    pub fn new(metadata: M, value: Option<Expression<M>>) -> Self {
        Self { metadata, value }
    }

    pub fn get_annotations(&self) -> &M {
        &self.metadata
    }

    pub fn get_annotations_mut(&mut self) -> &mut M {
        &mut self.metadata
    }

    pub fn get_value(&self) -> &Option<Expression<M>> {
        &self.value
    }

    pub fn root_str(&self) -> String {
        format!("yret")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Return<M> {
    metadata: M,
    value: Option<Expression<M>>,
}

impl<M> Return<M> {
    pub fn new(metadata: M, value: Option<Expression<M>>) -> Self {
        Self { metadata, value }
    }

    pub fn get_annotations(&self) -> &M {
        &self.metadata
    }

    pub fn get_annotations_mut(&mut self) -> &mut M {
        &mut self.metadata
    }

    pub fn get_value(&self) -> &Option<Expression<M>> {
        &self.value
    }

    pub fn root_str(&self) -> String {
        format!("return")
    }
}
