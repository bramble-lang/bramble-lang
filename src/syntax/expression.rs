use super::{path::Path, statement::Statement, ty::Type};

#[derive(Clone, Debug, PartialEq)]
pub enum Expression<I> {
    Integer(I, i32),
    Boolean(I, bool),
    StringLiteral(I, String),
    CustomType(I, Path),
    Identifier(I, String),
    Path(I, Path),
    MemberAccess(I, Box<Expression<I>>, String),
    IdentifierDeclare(I, String, Type),
    RoutineCall(I, RoutineCall, Path, Vec<Expression<I>>),
    StructExpression(I, Path, Vec<(String, Expression<I>)>),
    If(
        I,
        Box<Expression<I>>,
        Box<Expression<I>>,
        Box<Expression<I>>,
    ),
    ExpressionBlock(I, Vec<Statement<I>>, Option<Box<Expression<I>>>),

    BinaryOp(I, BinaryOperator, Box<Expression<I>>, Box<Expression<I>>),
    UnaryOp(I, UnaryOperator, Box<Expression<I>>),

    Yield(I, Box<Expression<I>>),
}

impl<I> Expression<I> {
    pub fn root_str(&self) -> String {
        use Expression::*;
        match self {
            Integer(_, v) => format!("{}", v),
            Boolean(_, v) => format!("{}", v),
            StringLiteral(_, v) => format!("\"{}\"", v),
            CustomType(_, v) => format!("{}", v),
            Identifier(_, v) => v.clone(),
            IdentifierDeclare(_, v, p) => format!("{}:{}", v, p),
            MemberAccess(_, s, m) => format!("{}.{}", s.root_str(), m),
            Path(_, path) => format!("{}", path),
            BinaryOp(_, op, _, _) => format!("{}", op),
            UnaryOp(_, op, _) => format!("{}", op),
            StructExpression(_, name, ..) => format!("intialization for struct {}", name),
            RoutineCall(_, call, name, ..) => format!("{} of {:?}", call, name),
            If(_, _, _, _) => "if".into(),
            ExpressionBlock(..) => "expression block".into(),
            Yield(_, _) => "yield".into(),
        }
    }

    pub fn get_annotations(&self) -> &I {
        use Expression::*;
        match self {
            Integer(m, ..)
            | Boolean(m, ..)
            | StringLiteral(m, ..)
            | CustomType(m, ..)
            | Identifier(m, ..)
            | IdentifierDeclare(m, ..)
            | Path(m, ..)
            | MemberAccess(m, ..)
            | BinaryOp(m, ..)
            | UnaryOp(m, ..)
            | If(m, ..)
            | ExpressionBlock(m, ..)
            | Yield(m, ..)
            | RoutineCall(m, ..) => m,
            StructExpression(m, ..) => m,
        }
    }

    pub fn get_annotations_mut(&mut self) -> &mut I {
        use Expression::*;
        match self {
            Integer(m, ..)
            | Boolean(m, ..)
            | StringLiteral(m, ..)
            | CustomType(m, ..)
            | Identifier(m, ..)
            | IdentifierDeclare(m, ..)
            | Path(m, ..)
            | MemberAccess(m, ..)
            | BinaryOp(m, ..)
            | UnaryOp(m, ..)
            | If(m, ..)
            | ExpressionBlock(m, ..)
            | Yield(m, ..)
            | RoutineCall(m, ..) => m,
            StructExpression(m, ..) => m,
        }
    }

    /// If a node is a function or a coroutine this will return its parameter vector.  If
    /// the node is not a function or coroutine, this will return None.
    pub fn get_params(&self) -> Option<&Vec<(String, Type)>> {
        match self {
            _ => None,
        }
    }

    /// if a node is a routine type then return the return type of the routine
    /// otherwise return None.
    pub fn get_return_type(&self) -> Option<&Type> {
        match self {
            _ => None,
        }
    }

    /// If a node is an identifier, function or coroutine, then this will return the name; otherwise it will return `None`.
    pub fn get_name(&self) -> Option<&str> {
        match self {
            Expression::Identifier(_, name) => Some(name),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    BAnd,
    BOr,
    Eq,
    NEq,
    Ls,
    LsEq,
    Gr,
    GrEq,
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        use BinaryOperator::*;
        match self {
            Add => f.write_str("+"),
            Sub => f.write_str("-"),
            Mul => f.write_str("*"),
            Div => f.write_str("/"),
            BAnd => f.write_str("&&"),
            BOr => f.write_str("||"),
            Eq => f.write_str("=="),
            NEq => f.write_str("!="),
            Ls => f.write_str("<"),
            LsEq => f.write_str("<="),
            Gr => f.write_str(">"),
            GrEq => f.write_str(">="),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOperator {
    Minus,
    Not,
}

impl std::fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        use UnaryOperator::*;
        match self {
            Minus => f.write_str("-"),
            Not => f.write_str("!"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RoutineCall {
    Function,
    CoroutineInit,
}

impl std::fmt::Display for RoutineCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        use RoutineCall::*;
        match self {
            CoroutineInit => f.write_str("coroutine init"),
            Function => f.write_str("function call"),
        }
    }
}
