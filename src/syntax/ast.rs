#[derive(Clone, Debug, PartialEq)]
pub enum Ast<I> {
    Integer(I, i32),
    Boolean(I, bool),
    Identifier(I, String),
    IdentifierDeclare(I, String, Primitive),

    Mul(I, Box<Ast<I>>, Box<Ast<I>>),
    Add(I, Box<Ast<I>>, Box<Ast<I>>),
    BAnd(I, Box<Ast<I>>, Box<Ast<I>>),
    BOr(I, Box<Ast<I>>, Box<Ast<I>>),
    Gr(I, Box<Ast<I>>, Box<Ast<I>>),
    GrEq(I, Box<Ast<I>>, Box<Ast<I>>),
    Ls(I, Box<Ast<I>>, Box<Ast<I>>),
    LsEq(I, Box<Ast<I>>, Box<Ast<I>>),
    Eq(I, Box<Ast<I>>, Box<Ast<I>>),
    NEq(I, Box<Ast<I>>, Box<Ast<I>>),
    Printi(I, Box<Ast<I>>),
    Printiln(I, Box<Ast<I>>),
    Printbln(I, Box<Ast<I>>),

    If(I, Box<Ast<I>>, Box<Ast<I>>, Box<Ast<I>>),
    ExpressionBlock(I, Vec<Ast<I>>),

    Statement(I, Box<Ast<I>>),
    Bind(I, String, Primitive, Box<Ast<I>>),
    Return(I, Option<Box<Ast<I>>>),
    Yield(I, Box<Ast<I>>),
    YieldReturn(I, Option<Box<Ast<I>>>),

    FunctionDef(I, String, Vec<(String, Primitive)>, Primitive, Vec<Ast<I>>),
    FunctionCall(I, String, Vec<Ast<I>>),
    CoroutineDef(I, String, Vec<(String, Primitive)>, Primitive, Vec<Ast<I>>),
    CoroutineInit(I, String, Vec<Ast<I>>),
    Module(I, Vec<Ast<I>>, Vec<Ast<I>>),
}

impl<I> Ast<I> {
    pub fn root_str(&self) -> String {
        use Ast::*;
        match self {
            Integer(_, v) => format!("{}", v),
            Boolean(_, v) => format!("{}", v),
            Identifier(_, v) => v.clone(),
            IdentifierDeclare(_, v, p) => format!("{}:{}", v, p),

            Mul(_, _, _) => "*".into(),
            Add(_, _, _) => "+".into(),
            BAnd(_, _, _) => "&&".into(),
            BOr(_, _, _) => "||".into(),

            Eq(_, _, _) => "==".into(),
            NEq(_, _, _) => "!=".into(),
            Ls(_, _, _) => "<".into(),
            LsEq(_, _, _) => "<=".into(),
            Gr(_, _, _) => ">".into(),
            GrEq(_, _, _) => ">=".into(),

            Printi(_, _) => "printi".into(),
            Printiln(_, _) => "printiln".into(),
            Printbln(_, _) => "printbln".into(),

            If(_, _, _, _) => "if".into(),
            ExpressionBlock(_, _) => "expression block".into(),

            Statement(_, _) => "statement".into(),
            Bind(_, _, _, _) => "bind".into(),
            Return(_, _) => "return".into(),
            Yield(_, _) => "yield".into(),
            YieldReturn(_, _) => "yret".into(),

            FunctionDef(_, _, _, _, _) => "function definition".into(),
            FunctionCall(_, _, _) => "function call".into(),
            CoroutineDef(_, _, _, _, _) => "coroutine definition".into(),
            CoroutineInit(_, _, _) => "coroutine init".into(),
            Module(_, _, _) => "module".into(),
        }
    }

    pub fn get_metadata(&self) -> &I {
        use Ast::*;
        match self {
            Integer(m,..) | Boolean(m,..) | Identifier(m,..) | IdentifierDeclare(m,..)
            | Mul(m,..) | Add(m,..) | BAnd(m,..) | BOr(m,..)
            | Eq(m,..) | NEq(m,..) | Ls(m,..) | LsEq(m,..) | Gr(m,..) | GrEq(m,..)
            | Printi(m,..) | Printiln(m,..) | Printbln(m,..)
            | If(m,..) | ExpressionBlock(m,..) | Statement(m,..)
            | Bind(m,..) | Return(m,..) | Yield(m,..) | YieldReturn(m,..)
            | FunctionDef(m,..) | FunctionCall(m,..) | CoroutineDef(m,..) | CoroutineInit(m,..)
            | Module(m,..)
            => m
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
