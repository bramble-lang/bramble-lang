use super::{
    module::{Item, Module},
    path::Path,
    ty::Type,
};

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

#[derive(Clone, Debug, PartialEq)]
pub enum Ast<I> {
    Integer(I, i32),
    Boolean(I, bool),
    StringLiteral(I, String),
    CustomType(I, Path),
    Identifier(I, String),
    Path(I, Path),
    MemberAccess(I, Box<Ast<I>>, String),
    IdentifierDeclare(I, String, Type),

    BinaryOp(I, BinaryOperator, Box<Ast<I>>, Box<Ast<I>>),
    UnaryOp(I, UnaryOperator, Box<Ast<I>>),
    Printi(I, Box<Ast<I>>),
    Prints(I, Box<Ast<I>>),
    Printiln(I, Box<Ast<I>>),
    Printbln(I, Box<Ast<I>>),

    If(I, Box<Ast<I>>, Box<Ast<I>>, Box<Ast<I>>),
    ExpressionBlock(I, Vec<Ast<I>>),

    Statement(I, Box<Ast<I>>),
    Bind(I, String, bool, Type, Box<Ast<I>>),
    Mutate(I, String, Box<Ast<I>>),
    Return(I, Option<Box<Ast<I>>>),
    Yield(I, Box<Ast<I>>),
    YieldReturn(I, Option<Box<Ast<I>>>),

    RoutineCall(I, RoutineCall, Path, Vec<Ast<I>>),
    Module(Module<I>),
    StructDef(I, String, Vec<(String, Type)>),
    StructExpression(I, Path, Vec<(String, Ast<I>)>),
}

impl<I> Ast<I> {
    pub fn root_str(&self) -> String {
        use Ast::*;
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

            Printi(_, _) => "printi".into(),
            Prints(_, _) => "prints".into(),
            Printiln(_, _) => "printiln".into(),
            Printbln(_, _) => "printbln".into(),

            If(_, _, _, _) => "if".into(),
            ExpressionBlock(_, _) => "expression block".into(),

            Statement(_, _) => "statement".into(),
            Bind(..) => "bind".into(),
            Mutate(..) => "assign".into(),
            Return(_, _) => "return".into(),
            Yield(_, _) => "yield".into(),
            YieldReturn(_, _) => "yret".into(),

            RoutineCall(_, call, name, ..) => format!("{} of {:?}", call, name),

            Module(m) => format!("module {}", m.get_name()),
            StructDef(_, name, ..) => format!("definition of struct {}", name),
            StructExpression(_, name, ..) => format!("intialization for struct {}", name),
        }
    }

    pub fn get_metadata(&self) -> &I {
        use Ast::*;
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
            | Printi(m, ..)
            | Printiln(m, ..)
            | Prints(m, ..)
            | Printbln(m, ..)
            | If(m, ..)
            | ExpressionBlock(m, ..)
            | Statement(m, ..)
            | Bind(m, ..)
            | Mutate(m, ..)
            | Return(m, ..)
            | Yield(m, ..)
            | YieldReturn(m, ..)
            | RoutineCall(m, ..)
            | StructDef(m, ..) => m,
            StructExpression(m, ..) => m,
            Module(m) => m.get_metadata(),
        }
    }

    pub fn get_metadata_mut(&mut self) -> &mut I {
        use Ast::*;
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
            | Printi(m, ..)
            | Printiln(m, ..)
            | Prints(m, ..)
            | Printbln(m, ..)
            | If(m, ..)
            | ExpressionBlock(m, ..)
            | Statement(m, ..)
            | Bind(m, ..)
            | Mutate(m, ..)
            | Return(m, ..)
            | Yield(m, ..)
            | YieldReturn(m, ..)
            | RoutineCall(m, ..)
            | StructDef(m, ..) => m,
            StructExpression(m, ..) => m,
            Module(m) => m.get_metadata_mut(),
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
            Ast::Identifier(_, name) | Ast::StructDef(_, name, ..) => Some(name),
            _ => None,
        }
    }

    pub fn go_to(&self, path: &Path) -> Option<&Item<I>> {
        if path.len() == 0 {
            return None;
        }

        if let Ast::Module(m) = self {
            m.go_to(path)
        } else {
            return None;
        }
    }

    pub fn go_to_module(&self, path: &Path) -> Option<&Module<I>> {
        if path.len() == 0 {
            return None;
        }

        if let Ast::Module(m) = self {
            m.go_to_module(path)
        } else {
            return None;
        }
    }
}
