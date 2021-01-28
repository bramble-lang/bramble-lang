#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    I64,
    Bool,
    StringLiteral,
}

impl std::fmt::Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Primitive::I64 => f.write_str("i64"),
            Primitive::Bool => f.write_str("bool"),
            Primitive::StringLiteral => f.write_str("string"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lex {
    Integer64(i64),
    Bool(bool),
    Identifier(String),
    StringLiteral(String),
    Mul,
    Div,
    Add,
    Minus,
    Not,
    BAnd,
    BOr,
    GrEq,
    LsEq,
    Gr,
    Ls,
    Eq,
    NEq,
    Assign,
    Semicolon,
    Comma,
    Let,
    Mut,
    Return,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Init,
    Yield,
    YieldReturn,
    CoroutineDef,
    FunctionDef,
    ModuleDef,
    Struct,
    If,
    Else,
    Colon,
    MemberAccess,
    PathSeparator,
    LArrow,
    Primitive(Primitive),
}

impl Lex {
    pub fn get_str(&self) -> Option<String> {
        match self {
            Lex::StringLiteral(s) | Lex::Identifier(s) => Some(s.clone()),
            _ => None,
        }
    }
}

impl std::fmt::Display for Lex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Lex::*;
        match self {
            Integer64(i) => f.write_str(&format!("literal {}", i)),
            Bool(b) => f.write_str(&format!("literal {}", b)),
            Identifier(id) => f.write_str(&format!("identifier {}", id)),
            StringLiteral(str) => f.write_str(&format!("literal \"{}\"", str)),
            Ls => f.write_str("<"),
            LsEq => f.write_str("<="),
            Gr => f.write_str(">"),
            GrEq => f.write_str(">="),
            Eq => f.write_str("="),
            NEq => f.write_str("!="),
            Mul => f.write_str("*"),
            Div => f.write_str("/"),
            Add => f.write_str("+"),
            Minus => f.write_str("-"),
            Not => f.write_str("!"),
            BAnd => f.write_str("&&"),
            BOr => f.write_str("||"),
            Assign => f.write_str(":="),
            Semicolon => f.write_str(";"),
            Comma => f.write_str(","),
            Let => f.write_str("let"),
            Mut => f.write_str("mut"),
            Return => f.write_str("return"),
            LParen => f.write_str("("),
            RParen => f.write_str(")"),
            LBrace => f.write_str("{"),
            RBrace => f.write_str("}"),
            Init => f.write_str("init"),
            Yield => f.write_str("yield"),
            YieldReturn => f.write_str("yret"),
            CoroutineDef => f.write_str("co"),
            FunctionDef => f.write_str("fn"),
            ModuleDef => f.write_str("mod"),
            Struct => f.write_str("struct"),
            If => f.write_str("if"),
            Else => f.write_str("else"),
            Colon => f.write_str(":"),
            MemberAccess => f.write_str("."),
            PathSeparator => f.write_str("::"),
            LArrow => f.write_str("->"),
            Primitive(p) => f.write_str(&format!("{}", p)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub l: u32,
    pub s: Lex,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("L{}: {}", self.l, self.s))
    }
}

impl Token {
    pub fn new(l: u32, s: Lex) -> Token {
        Token { l, s }
    }

    pub fn token_eq(&self, a: &Lex) -> bool {
        match self.s {
            Lex::Integer64(_) => match a {
                Lex::Integer64(_) => true,
                _ => false,
            },
            Lex::Bool(_) => match a {
                Lex::Bool(_) => true,
                _ => false,
            },
            Lex::Identifier(_) => match a {
                Lex::Identifier(_) => true,
                _ => false,
            },
            Lex::StringLiteral(_) => match a {
                Lex::StringLiteral(_) => true,
                _ => false,
            },
            Lex::Primitive(_) => match a {
                Lex::Primitive(_) => true,
                _ => false,
            },
            _ => *a == self.s,
        }
    }
}
