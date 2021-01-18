use super::{ast::Ast, ty::Type};

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<M> {
    Bind(Box<Bind<M>>),
    Mutate(Box<Mutate<M>>),

    Printi(Box<Printi<M>>),
    Printiln(Box<Printiln<M>>),
    Printbln(Box<Printbln<M>>),
    Prints(Box<Prints<M>>),

    Yield(Box<Ast<M>>),
    YieldReturn(Box<Ast<M>>),
    Expression(Box<Ast<M>>),

    Return(Box<Ast<M>>),
}

impl<M> Statement<M> {
    pub fn get_metadata(&self) -> &M {
        use Statement::*;

        match self {
            Return(x) 
            | Yield(x) 
            | YieldReturn(x) => x.get_metadata(),
            Expression(e) => e.get_metadata(),
            Bind(b) => b.get_metadata(),
            Mutate(m) => m.get_metadata(),
            Printi(m) => m.get_metadata(),
            Printiln(m) => m.get_metadata(),
            Printbln(m) => m.get_metadata(),
            Prints(m) => m.get_metadata(),
        }
    }

    pub fn from_ast(ast: Ast<M>) -> Option<Statement<M>> {
        match ast {
            Ast::Statement(s) => Some(s),
            Ast::Yield(_, _) => Some(Statement::Yield(Box::new(ast))),
            Ast::YieldReturn(_, _) => Some(Statement::YieldReturn(Box::new(ast))),
            Ast::Return(_, _) => Some(Statement::Return(Box::new(ast))),
            _ => Some(Statement::Expression(Box::new(ast))),
        }
    }

    pub fn get_metadata_mut(&mut self) -> &mut M {
        use Statement::*;

        match self {
            Return(x) 
            | Yield(x) 
            | YieldReturn(x) => x.get_metadata_mut(),
            Expression(e) => e.get_metadata_mut(),
            Bind(b) => b.get_metadata_mut(),
            Mutate(m) => m.get_metadata_mut(),
            Printi(m) => m.get_metadata_mut(),
            Printiln(m) => m.get_metadata_mut(),
            Printbln(m) => m.get_metadata_mut(),
            Prints(m) => m.get_metadata_mut(),
        }
    }

    pub fn root_str(&self) -> String {
        use Statement::*;

        match self {
            Return(x) 
            | Yield(x) 
            | YieldReturn(x) => x.root_str(),
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


#[derive(Clone, Debug, PartialEq)]
pub struct Mutate<M> {
    metadata: M,
    id: String,
    rhs: Ast<M>,
}

impl<M> Mutate<M> {
    pub fn new(metadata: M, id: &str, rhs: Ast<M>) -> Self {
        Mutate {
            metadata,
            id: id.into(),
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

    pub fn get_rhs(&self) -> &Ast<M> {
        &self.rhs
    }

    pub fn root_str(&self) -> String {
        format!("mut {}", self.id)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Printi<M> {
    metadata: M,
    value: Ast<M>,
}

impl<M> Printi<M> {
    pub fn new(metadata: M, value: Ast<M>) -> Self {
        Self {
            metadata,
            value,
        }
    }

    pub fn get_metadata(&self) -> &M {
        &self.metadata
    }

    pub fn get_metadata_mut(&mut self) -> &mut M {
        &mut self.metadata
    }

    pub fn get_value(&self) -> &Ast<M> {
        &self.value
    }

    pub fn root_str(&self) -> String {
        format!("printiln")
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct Printiln<M> {
    metadata: M,
    value: Ast<M>,
}

impl<M> Printiln<M> {
    pub fn new(metadata: M, value: Ast<M>) -> Self {
        Self {
            metadata,
            value,
        }
    }

    pub fn get_metadata(&self) -> &M {
        &self.metadata
    }

    pub fn get_metadata_mut(&mut self) -> &mut M {
        &mut self.metadata
    }

    pub fn get_value(&self) -> &Ast<M> {
        &self.value
    }

    pub fn root_str(&self) -> String {
        format!("printiln")
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct Printbln<M> {
    metadata: M,
    value: Ast<M>,
}

impl<M> Printbln<M> {
    pub fn new(metadata: M, value: Ast<M>) -> Self {
        Self {
            metadata,
            value,
        }
    }

    pub fn get_metadata(&self) -> &M {
        &self.metadata
    }

    pub fn get_metadata_mut(&mut self) -> &mut M {
        &mut self.metadata
    }

    pub fn get_value(&self) -> &Ast<M> {
        &self.value
    }

    pub fn root_str(&self) -> String {
        format!("printiln")
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct Prints<M> {
    metadata: M,
    value: Ast<M>,
}

impl<M> Prints<M> {
    pub fn new(metadata: M, value: Ast<M>) -> Self {
        Self {
            metadata,
            value,
        }
    }

    pub fn get_metadata(&self) -> &M {
        &self.metadata
    }

    pub fn get_metadata_mut(&mut self) -> &mut M {
        &mut self.metadata
    }

    pub fn get_value(&self) -> &Ast<M> {
        &self.value
    }

    pub fn root_str(&self) -> String {
        format!("printiln")
    }
}
