use super::{expression::Expression, node::Node, ty::Type};

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<M> {
    Bind(Box<Bind<M>>),
    Mutate(Box<Mutate<M>>),

    YieldReturn(Box<YieldReturn<M>>),
    Expression(Box<Expression<M>>),

    Return(Box<Return<M>>),
}

impl<M> Node<M> for Statement<M> {
    fn annotation(&self) -> &M {
        use Statement::*;

        match self {
            Return(x) => x.annotation(),
            YieldReturn(x) => x.annotation(),
            Expression(e) => e.annotation(),
            Bind(b) => b.annotation(),
            Mutate(m) => m.annotation(),
        }
    }

    fn annotation_mut(&mut self) -> &mut M {
        use Statement::*;

        match self {
            Return(x) => x.annotation_mut(),
            YieldReturn(x) => x.annotation_mut(),
            Expression(e) => e.annotation_mut(),
            Bind(b) => b.annotation_mut(),
            Mutate(m) => m.annotation_mut(),
        }
    }
}

impl<M> std::fmt::Display for Statement<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.write_str(&self.root_str())
    }
}

impl<M> Statement<M> {
    pub fn from_ast(ast: Expression<M>) -> Option<Statement<M>> {
        match ast {
            _ => Some(Statement::Expression(Box::new(ast))),
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
}

#[derive(Clone, Debug, PartialEq)]
pub struct Bind<M> {
    annotations: M,
    id: String,
    ty: Type,
    mutable: bool,
    rhs: Expression<M>,
}

impl<M> Node<M> for Bind<M> {
    fn annotation(&self) -> &M {
        &self.annotations
    }

    fn annotation_mut(&mut self) -> &mut M {
        &mut self.annotations
    }
}

impl<M> std::fmt::Display for Bind<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.write_str(&self.root_str())
    }
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
}

#[derive(Clone, Debug, PartialEq)]
pub struct Mutate<M> {
    annotations: M,
    id: String,
    rhs: Expression<M>,
}

impl<M> Node<M> for Mutate<M> {
    fn annotation(&self) -> &M {
        &self.annotations
    }

    fn annotation_mut(&mut self) -> &mut M {
        &mut self.annotations
    }
}

impl<M> std::fmt::Display for Mutate<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.write_str(&self.root_str())
    }
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

    pub fn get_rhs(&self) -> &Expression<M> {
        &self.rhs
    }

    pub fn get_rhs_mut(&mut self) -> &mut Expression<M> {
        &mut self.rhs
    }

    pub fn root_str(&self) -> String {
        format!("mut {}", self.id)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Yield<M> {
    annotations: M,
    value: Expression<M>,
}

impl<M> Node<M> for Yield<M> {
    fn annotation(&self) -> &M {
        &self.annotations
    }

    fn annotation_mut(&mut self) -> &mut M {
        &mut self.annotations
    }
}

impl<M> Yield<M> {
    pub fn new(annotations: M, value: Expression<M>) -> Self {
        Self { annotations, value }
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

impl<M> Node<M> for YieldReturn<M> {
    fn annotation(&self) -> &M {
        &self.annotations
    }

    fn annotation_mut(&mut self) -> &mut M {
        &mut self.annotations
    }
}

impl<M> std::fmt::Display for YieldReturn<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.write_str(&self.root_str())
    }
}

impl<M> YieldReturn<M> {
    pub fn new(annotations: M, value: Option<Expression<M>>) -> Self {
        Self { annotations, value }
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
}

#[derive(Clone, Debug, PartialEq)]
pub struct Return<M> {
    annotations: M,
    value: Option<Expression<M>>,
}

impl<M> Node<M> for Return<M> {
    fn annotation(&self) -> &M {
        &self.annotations
    }

    fn annotation_mut(&mut self) -> &mut M {
        &mut self.annotations
    }
}

impl<M> std::fmt::Display for Return<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.write_str(&self.root_str())
    }
}

impl<M> Return<M> {
    pub fn new(annotations: M, value: Option<Expression<M>>) -> Self {
        Self { annotations, value }
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
}
