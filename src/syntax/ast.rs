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
    StringLiteral(I, String),
    CustomType(I, String),
    Identifier(I, String),
    Path(I, Vec<String>),
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

    RoutineDef(
        I,
        RoutineDef,
        String,
        Vec<(String, Type)>,
        Type,
        Vec<Ast<I>>,
    ),
    RoutineCall(I, RoutineCall, Vec<String>, Vec<Ast<I>>),
    Module {
        meta: I,
        name: String,
        functions: Vec<Ast<I>>,
        coroutines: Vec<Ast<I>>,
        structs: Vec<Ast<I>>,
    },
    StructDef(I, String, Vec<(String, Type)>),
    StructExpression(I, String, Vec<(String, Ast<I>)>),
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
            Path(_, path) => path.join("::"),
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

            RoutineDef(_, def, name, ..) => format!("{} for {}", def, name),
            RoutineCall(_, call, name, ..) => format!("{} of {}", call, name),

            Module { .. } => "module".into(),
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
            | RoutineDef(m, ..)
            | RoutineCall(m, ..)
            | Module { meta: m, .. }
            | StructDef(m, ..) => m,
            StructExpression(m, ..) => m,
        }
    }

    /// If a node is a function or a coroutine this will return its parameter vector.  If
    /// the node is not a function or coroutine, this will return None.
    pub fn get_params(&self) -> Option<&Vec<(String, Type)>> {
        match self {
            Ast::RoutineDef(_, _, _, params, ..) => Some(params),
            _ => None,
        }
    }

    /// if a node is a routine type then return the return type of the routine
    /// otherwise return None.
    pub fn get_return_type(&self) -> Option<&Type> {
        match self {
            Ast::RoutineDef(_, _, _, _, ret, _) => Some(ret),
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

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    I32,
    Bool,
    StringLiteral,
    Unit,
    Custom(String),
    StructDef(Vec<(String, Type)>),
    FunctionDef(Vec<Type>, Box<Type>),
    CoroutineDef(Vec<Type>, Box<Type>),
    Coroutine(Box<Type>),
    Unknown,
}

impl Type {
    pub fn get_members(&self) -> Option<&Vec<(String, Type)>> {
        match self {
            Type::StructDef(members) => Some(members),
            _ => None,
        }
    }

    pub fn get_member(&self, member: &str) -> Option<&Type> {
        self.get_members()
            .map(|ms| ms.iter().find(|(n, _)| n == member).map(|m| &m.1))
            .flatten()
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        use Type::*;
        match self {
            I32 => f.write_str("i32"),
            Bool => f.write_str("bool"),
            StringLiteral => f.write_str("string"),
            Unit => f.write_str("unit"),
            Custom(name) => f.write_str(name),
            StructDef(members) => {
                let members = members
                    .iter()
                    .map(|m| format!("{}: {}", m.0, m.1))
                    .collect::<Vec<String>>()
                    .join(",");
                f.write_str(&members)
            }
            Type::CoroutineDef(params, ret_ty) => {
                let params = params
                    .iter()
                    .map(|p| format!("{}", p))
                    .collect::<Vec<String>>()
                    .join(",");
                f.write_fmt(format_args!("co ({}) -> {}", params, ret_ty))
            }
            Type::Coroutine(ret_ty) => f.write_fmt(format_args!("co<{}>", ret_ty)),
            Type::FunctionDef(params, ret_ty) => {
                let params = params
                    .iter()
                    .map(|p| format!("{}", p))
                    .collect::<Vec<String>>()
                    .join(",");
                f.write_fmt(format_args!("fn ({}) -> {}", params, ret_ty))
            }
            Unknown => f.write_str("unknown"),
        }
    }
}
