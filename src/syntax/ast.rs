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
pub struct Path {
    path: Vec<String>,
}

impl Path {
    pub fn len(&self) -> usize {
        self.path.len()
    }

    pub fn last(&self) -> Option<&String> {
        self.path.last()
    }

    pub fn iter(&self) -> std::slice::Iter<String> {
        self.path.iter()
    }

    /// Remove the last step in the path
    pub fn truncate(&mut self) -> Option<String> {
        self.path.pop()
    }

    /**
    Converts this path into a canonical path by merging it
    with the given current path.

    - If this is already a canonical path (i.e. begins with `root`) then nothing will change.
    - If this path begins with `self` then `self` will be replaced with `current_path`
    - occurances of `super` will move up the current path
    */
    pub fn to_canonical(&self, current_path: &Path) -> Result<Path, String> {
        if self.path[0] == "root" {
            Ok(self.clone())
        } else {
            let path = if self.path[0] == "self" {&self.path[1..]} else {&self.path};
            let mut merged = current_path.path.clone();
            for step in path.iter() {
                if step == "super" {
                    merged.pop().ok_or("Use of super in path exceeded the depth of the current path")?;
                    if merged.len() == 0 {
                        return Err("Use of super in path exceeded the depth of the current path".into());
                    }
                } else {
                    merged.push(step.clone());
                }
            }
            Ok(Path {path: merged})
        }
    }
}

impl<I: std::slice::SliceIndex<[String]>> std::ops::Index<I> for Path {
    type Output = I::Output;

    #[inline]
    fn index(&self, index: I) -> &Self::Output {
        std::ops::Index::index(&*self.path, index)
    }
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.path.join("::"))
    }
}

impl From<Vec<String>> for Path {
    fn from(v: Vec<String>) -> Self {
        Path {
            path: v.clone(),
        }
    }
}

impl From<Vec<&str>> for Path {
    fn from(v: Vec<&str>) -> Self {
        Path {
            path: v.into_iter().map(|e| e.into()).collect(),
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

    RoutineDef(
        I,
        RoutineDef,
        String,
        Vec<(String, Type)>,
        Type,
        Vec<Ast<I>>,
    ),
    RoutineCall(I, RoutineCall, Path, Vec<Ast<I>>),
    Module {
        meta: I,
        name: String,
        modules: Vec<Ast<I>>,
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

            RoutineDef(_, def, name, ..) => format!("{} for {}", def, name),
            RoutineCall(_, call, name, ..) => format!("{} of {:?}", call, name),

            Module { name, .. } => format!("module {}", name),
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
            Ast::RoutineDef(_, _, name, _, ..) | Ast::Identifier(_, name) | Ast::Module{name, ..} => Some(name),
            _ => None,
        }
    }

    pub fn go_to(&self, path: &Path) -> Option<&Self> {
        if path.len() == 0 {
            return None;
        }

        let mut iter = path.iter();
        if let Ast::Module{name, ..} = self {
            match iter.next() {
                Some(step) if step == name => (),
                _ => return None
            }
        } else {
            return None
        }

        let mut current = self;
        for step in iter {
            match current {
                Ast::Module{..} => {
                    if let Some(node) = current.get_item(step) {
                        current = node;
                    } else {
                        return None
                    }
                }
                _ => return None,
            }
        }
        Some(current)
    }

    /// If a Node contains an Item (function, coroutine, module, or struct)
    /// Then return it
    pub fn get_item(&self, name: &str) -> Option<&Self> {
        match self {
            Ast::Module{modules, functions, coroutines, structs, ..} => {
                modules.iter().find(|f| f.get_name().map_or(false, |n| n == name))
                .or(functions.iter().find(|c| c.get_name().map_or(false, |n| n == name)))
                .or(coroutines.iter().find(|c| c.get_name().map_or(false, |n| n == name)))
                .or(structs.iter().find(|s| s.get_name().map_or(false, |n| n == name)))
            },
            _ => None
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

impl PartialEq<Type> for &Type {
    fn eq(&self, other: &Type) -> bool {
        *self == other
    }
}

impl PartialEq<&Type> for Type {
    fn eq(&self, other: &&Type) -> bool {
        self == *other
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_go_to_shallow() {
        let ast = Ast::Module{meta: 0, name: "root".into(), modules: vec![], functions: vec![], coroutines: vec![], structs: vec![]};
        let path = vec!["root"].into();
        let node = ast.go_to(&path).unwrap();
        assert_eq!(node, &ast);
    }

    #[test]
    fn test_go_to_nested() {
        let inner = Ast::Module{meta: 0, name: "inner".into(), modules: vec![], functions: vec![], coroutines: vec![], structs: vec![]};
        let middle = Ast::Module{meta: 0, name: "middle".into(), modules: vec![inner.clone()], functions: vec![], coroutines: vec![], structs: vec![]};
        let ast = Ast::Module{meta: 0, name: "root".into(), modules: vec![middle.clone()], functions: vec![], coroutines: vec![], structs: vec![]};

        let path = vec!["root"].into();
        let node = ast.go_to(&path).unwrap();
        assert_eq!(*node, ast);

        let path = vec!["root", "middle"].into();
        let node = ast.go_to(&path).unwrap();
        assert_eq!(*node, middle);

        let path = vec!["root", "middle", "inner"].into();
        let node = ast.go_to(&path).unwrap();
        assert_eq!(*node, inner);

        let path = vec!["root", "middle", "nothing"].into();
        let node = ast.go_to(&path);
        assert_eq!(node, None);

        let path = vec!["root", "middle", "inner", "nothing"].into();
        let node = ast.go_to(&path);
        assert_eq!(node, None);
    }

    #[test]
    fn test_go_to_not_found_root() {
        let ast = Ast::Module{meta: 0, name: "root".into(), modules: vec![], functions: vec![], coroutines: vec![], structs: vec![]};
        let path = vec!["wrong"].into();
        let node = ast.go_to(&path);
        assert_eq!(node, None);
    }

    #[test]
    fn test_go_to_not_found() {
        let ast = Ast::Module{meta: 0, name: "root".into(), modules: vec![], functions: vec![], coroutines: vec![], structs: vec![]};
        let path = vec!["root::test::blah"].into();
        let node = ast.go_to(&path);
        assert_eq!(node, None);
    }

    #[test]
    fn test_go_to_empty_path() {
        let ast = Ast::Module{meta: 0, name: "root".into(), modules: vec![], functions: vec![], coroutines: vec![], structs: vec![]};
        let path = Vec::<String>::new().into();
        let node = ast.go_to(&path);
        assert_eq!(node, None);
    }

    #[test]
    fn test_go_to_function() {
        let func = Ast::RoutineDef(0, RoutineDef::Function, "func".into(), vec![], Type::I32, vec![]);
        let ast = Ast::Module{meta: 0, name: "root".into(), modules: vec![], functions: vec![func.clone()], coroutines: vec![], structs: vec![]};
        let path = vec!["root", "func"].into();
        let node = ast.go_to(&path).unwrap();
        assert_eq!(*node, func);
    }
}

#[cfg(test)]
mod test_path {
    use super::*;

    #[test]
    fn test_canonical_to_canonical() {
        let path: Path = vec!["root", "first"].into();
        let current = vec!["root", "current"].into();
        let canonized_path = path.to_canonical(&current);
        let expected = vec!["root", "first"].into();
        assert_eq!(canonized_path, Ok(expected));
    }

    #[test]
    fn test_relative_to_canonical() {
        let path: Path = vec!["relative"].into();
        let current = vec!["root", "current"].into();
        let canonized_path = path.to_canonical(&current);
        let expected = vec!["root", "current", "relative"].into();
        assert_eq!(canonized_path, Ok(expected));
    }

    #[test]
    fn test_self_to_canonical() {
        let path: Path = vec!["self", "relative"].into();
        let current = vec!["root", "current"].into();
        let canonized_path = path.to_canonical(&current);
        let expected = vec!["root", "current", "relative"].into();
        assert_eq!(canonized_path, Ok(expected));
    }

    #[test]
    fn test_relative_with_super_to_canonical() {
        let path: Path = vec!["super", "relative"].into();
        let current = vec!["root", "current"].into();
        let canonized_path = path.to_canonical(&current);
        let expected = vec!["root", "relative"].into();
        assert_eq!(canonized_path, Ok(expected));
    }

    #[test]
    fn test_relative_with_post_super_to_canonical() {
        let path: Path = vec!["relative", "super"].into();
        let current = vec!["root", "current"].into();
        let canonized_path = path.to_canonical(&current);
        let expected = vec!["root", "current"].into();
        assert_eq!(canonized_path, Ok(expected));
    }

    #[test]
    fn test_too_many_supers() {
        let path: Path = vec!["super", "super", "relative"].into();
        let current = vec!["root", "current"].into();
        let canonized_path = path.to_canonical(&current);
        let expected = "Use of super in path exceeded the depth of the current path".into();
        assert_eq!(canonized_path, Err(expected));
    }

    #[test]
    fn test_relative_with_scattered_super_to_canonical() {
        let path: Path = vec!["super", "relative", "super"].into();
        let current = vec!["root", "current"].into();
        let canonized_path = path.to_canonical(&current);
        let expected = vec!["root"].into();
        assert_eq!(canonized_path, Ok(expected));
    }

    #[test]
    fn test_split_off_item() {
        let path: Path = vec!["self", "item"].into();
        let current = vec!["root", "current"].into();
        let mut canonized_path = path.to_canonical(&current).unwrap();
        let expected = vec!["root", "current"].into();
        let item = canonized_path.truncate();
        assert_eq!(canonized_path, expected);
        assert_eq!(item, Some("item".into()));
    }
}