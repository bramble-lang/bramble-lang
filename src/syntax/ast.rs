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
pub enum RoutineDef {
    Function,
    Coroutine,
}

impl std::fmt::Display for RoutineDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        use RoutineDef::*;
        match self {
            Coroutine => f.write_str("coroutine def"),
            Function => f.write_str("function def"),
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
    Identifier(I, String),
    IdentifierDeclare(I, String, Primitive),

    BinaryOp(I, BinaryOperator, Box<Ast<I>>, Box<Ast<I>>),
    UnaryOp(I, UnaryOperator, Box<Ast<I>>),
    Printi(I, Box<Ast<I>>),
    Printiln(I, Box<Ast<I>>),
    Printbln(I, Box<Ast<I>>),

    If(I, Box<Ast<I>>, Box<Ast<I>>, Box<Ast<I>>),
    ExpressionBlock(I, Vec<Ast<I>>),

    Statement(I, Box<Ast<I>>),
    Bind(I, String, bool, Primitive, Box<Ast<I>>),
    Mutate(I, String, Box<Ast<I>>),
    Return(I, Option<Box<Ast<I>>>),
    Yield(I, Box<Ast<I>>),
    YieldReturn(I, Option<Box<Ast<I>>>),

    RoutineDef(
        I,
        RoutineDef,
        String,
        Vec<(String, Primitive)>,
        Primitive,
        Vec<Ast<I>>,
    ),
    RoutineCall(I, RoutineCall, String, Vec<Ast<I>>),
    Module(I, Vec<Ast<I>>, Vec<Ast<I>>),
    Struct(I, String, Vec<(String,Primitive)>),
}

impl<I> Ast<I> {
    pub fn root_str(&self) -> String {
        use Ast::*;
        match self {
            Integer(_, v) => format!("{}", v),
            Boolean(_, v) => format!("{}", v),
            Identifier(_, v) => v.clone(),
            IdentifierDeclare(_, v, p) => format!("{}:{}", v, p),

            BinaryOp(_, op, _, _) => format!("{}", op),
            UnaryOp(_, op, _) => format!("{}", op),

            Printi(_, _) => "printi".into(),
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

            RoutineDef(_, def, name, ..) => format!("{} for {}", def, name),
            RoutineCall(_, call, name, ..) => format!("{} of {}", call, name),

            Module(_, _, _) => "module".into(),
            Struct(..) => "struct".into(),
        }
    }

    pub fn get_metadata(&self) -> &I {
        use Ast::*;
        match self {
            Integer(m, ..)
            | Boolean(m, ..)
            | Identifier(m, ..)
            | IdentifierDeclare(m, ..)
            | BinaryOp(m, ..)
            | UnaryOp(m, ..)
            | Printi(m, ..)
            | Printiln(m, ..)
            | Printbln(m, ..)
            | If(m, ..)
            | ExpressionBlock(m, ..)
            | Statement(m, ..)
            | Bind(m, ..)
            | Mutate(m, ..)
            | Return(m, ..)
            | Yield(m, ..)
            | YieldReturn(m, ..)
            | RoutineDef(m, ..)
            | RoutineCall(m, ..)
            | Module(m, ..) => m,
            | Struct(m, ..) => m,
        }
    }

    /// If a node is a function or a coroutine this will return its parameter vector.  If
    /// the node is not a function or coroutine, this will return None.
    pub fn get_params(&self) -> Option<&Vec<(String, Primitive)>> {
        match self {
            Ast::RoutineDef(_, _, _, params, ..) => Some(params),
            _ => None,
        }
    }

    /// If a node is an identifier, function or coroutine, then this will return the name; otherwise it will return `None`.
    pub fn get_name(&self) -> Option<&str> {
        match self {
            Ast::RoutineDef(_, _, name, _, ..) | Ast::Identifier(_, name) => Some(name),
            _ => None,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Primitive {
    I32,
    Bool,
    Unit,
    Unknown,
}

impl std::fmt::Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        use Primitive::*;
        match self {
            I32 => f.write_str("i32"),
            Bool => f.write_str("bool"),
            Unit => f.write_str("unit"),
            Unknown => f.write_str("unknown"),
        }
    }
}
