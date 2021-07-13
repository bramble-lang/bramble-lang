use super::{
    expression::Expression,
    node::{
        Context, Node, NodeType, {PostOrderIter, PreOrderIter},
    },
    ty::Type,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<M> {
    Bind(Box<Bind<M>>),
    Mutate(Box<Mutate<M>>),

    YieldReturn(Box<YieldReturn<M>>),
    Expression(Box<Expression<M>>),

    Return(Box<Return<M>>),
}

impl<M: Context> Node<M> for Statement<M> {
    fn get_context(&self) -> &M {
        use Statement::*;

        match self {
            Return(x) => x.get_context(),
            YieldReturn(x) => x.get_context(),
            Expression(e) => e.get_context(),
            Bind(b) => b.get_context(),
            Mutate(m) => m.get_context(),
        }
    }

    fn get_context_mut(&mut self) -> &mut M {
        use Statement::*;

        match self {
            Return(x) => x.get_context_mut(),
            YieldReturn(x) => x.get_context_mut(),
            Expression(e) => e.get_context_mut(),
            Bind(b) => b.get_context_mut(),
            Mutate(m) => m.get_context_mut(),
        }
    }

    fn node_type(&self) -> NodeType {
        NodeType::Statement
    }

    fn children(&self) -> Vec<&dyn Node<M>> {
        use Statement::*;

        match self {
            Return(x) => x.children(),
            YieldReturn(x) => x.children(),
            Expression(e) => e.children(),
            Bind(b) => b.children(),
            Mutate(m) => m.children(),
        }
    }

    fn name(&self) -> Option<&str> {
        use Statement::*;

        match self {
            Return(x) => x.name(),
            YieldReturn(x) => x.name(),
            Expression(e) => e.name(),
            Bind(b) => b.name(),
            Mutate(m) => m.name(),
        }
    }

    fn iter_postorder(&self) -> PostOrderIter<M> {
        PostOrderIter::new(self)
    }

    fn iter_preorder(&self) -> PreOrderIter<M> {
        PreOrderIter::new(self)
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
    context: M,
    id: String,
    ty: Type,
    mutable: bool,
    rhs: Expression<M>,
}

impl<M: Context> Node<M> for Bind<M> {
    fn get_context(&self) -> &M {
        &self.context
    }

    fn get_context_mut(&mut self) -> &mut M {
        &mut self.context
    }

    fn node_type(&self) -> NodeType {
        NodeType::Statement
    }

    fn children(&self) -> Vec<&dyn Node<M>> {
        vec![&self.rhs]
    }

    fn name(&self) -> Option<&str> {
        None
    }

    fn iter_postorder(&self) -> PostOrderIter<M> {
        PostOrderIter::new(self)
    }

    fn iter_preorder(&self) -> PreOrderIter<M> {
        PreOrderIter::new(self)
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
            context: annotations,
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

    pub fn set_type(&mut self, ty: Type) {
        self.ty = ty;
    }

    pub fn root_str(&self) -> String {
        format!("bind {}", self.id)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Mutate<M> {
    context: M,
    id: String,
    rhs: Expression<M>,
}

impl<M: Context> Node<M> for Mutate<M> {
    fn get_context(&self) -> &M {
        &self.context
    }

    fn get_context_mut(&mut self) -> &mut M {
        &mut self.context
    }

    fn node_type(&self) -> NodeType {
        NodeType::Statement
    }

    fn children(&self) -> Vec<&dyn Node<M>> {
        vec![&self.rhs]
    }

    fn name(&self) -> Option<&str> {
        None
    }

    fn iter_postorder(&self) -> PostOrderIter<M> {
        PostOrderIter::new(self)
    }

    fn iter_preorder(&self) -> PreOrderIter<M> {
        PreOrderIter::new(self)
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
            context: annotations,
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
pub struct YieldReturn<M> {
    context: M,
    value: Option<Expression<M>>,
}

impl<M: Context> Node<M> for YieldReturn<M> {
    fn get_context(&self) -> &M {
        &self.context
    }

    fn get_context_mut(&mut self) -> &mut M {
        &mut self.context
    }

    fn node_type(&self) -> NodeType {
        NodeType::Statement
    }

    fn children(&self) -> Vec<&dyn Node<M>> {
        match &self.value {
            Some(v) => vec![v],
            None => vec![],
        }
    }

    fn name(&self) -> Option<&str> {
        None
    }

    fn iter_postorder(&self) -> PostOrderIter<M> {
        PostOrderIter::new(self)
    }

    fn iter_preorder(&self) -> PreOrderIter<M> {
        PreOrderIter::new(self)
    }
}

impl<M> std::fmt::Display for YieldReturn<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.write_str(&self.root_str())
    }
}

impl<M> YieldReturn<M> {
    pub fn new(annotations: M, value: Option<Expression<M>>) -> Self {
        Self {
            context: annotations,
            value,
        }
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
    context: M,
    value: Option<Expression<M>>,
}

impl<M: Context> Node<M> for Return<M> {
    fn get_context(&self) -> &M {
        &self.context
    }

    fn get_context_mut(&mut self) -> &mut M {
        &mut self.context
    }

    fn node_type(&self) -> NodeType {
        NodeType::Statement
    }

    fn children(&self) -> Vec<&dyn Node<M>> {
        match &self.value {
            Some(v) => vec![v],
            None => vec![],
        }
    }

    fn name(&self) -> Option<&str> {
        None
    }

    fn iter_postorder(&self) -> PostOrderIter<M> {
        PostOrderIter::new(self)
    }

    fn iter_preorder(&self) -> PreOrderIter<M> {
        PreOrderIter::new(self)
    }
}

impl<M> std::fmt::Display for Return<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.write_str(&self.root_str())
    }
}

impl<M> Return<M> {
    pub fn new(annotations: M, value: Option<Expression<M>>) -> Self {
        Self {
            context: annotations,
            value,
        }
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
