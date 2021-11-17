use crate::{
    compiler::{source::SourceIr, CompilerDisplay, CompilerDisplayError, SourceMap, Span},
    StringId,
};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Primitive {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    Bool,
    StringLiteral,
}

impl std::fmt::Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Primitive::U8 => f.write_str("u8"),
            Primitive::U16 => f.write_str("u16"),
            Primitive::U32 => f.write_str("u32"),
            Primitive::U64 => f.write_str("u64"),
            Primitive::I8 => f.write_str("i8"),
            Primitive::I16 => f.write_str("i16"),
            Primitive::I32 => f.write_str("i32"),
            Primitive::I64 => f.write_str("i64"),
            Primitive::Bool => f.write_str("bool"),
            Primitive::StringLiteral => f.write_str("string"),
        }
    }
}

impl CompilerDisplay for Primitive {
    fn fmt(&self, _: &SourceMap, _: &crate::StringTable) -> Result<String, CompilerDisplayError> {
        Ok(format!("{}", self))
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Lex {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    Bool(bool),
    Identifier(StringId),
    StringLiteral(StringId),
    VarArgs,
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
    LBracket,
    RBracket,
    Init,
    Yield,
    YieldReturn,
    CoroutineDef,
    FunctionDef,
    ModuleDef,
    Struct,
    Extern,
    If,
    Else,
    While,
    Colon,
    MemberAccess,
    PathSeparator,
    LArrow,
    Primitive(Primitive),
    PathSelf,
    PathSuper,
    PathProjectRoot,
    PathFileRoot,
}

impl Lex {
    pub fn get_str(&self) -> Option<StringId> {
        match self {
            Lex::StringLiteral(s) | Lex::Identifier(s) => Some(*s),
            _ => None,
        }
    }
}

impl std::fmt::Display for Lex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Lex::*;
        match self {
            U8(i) => f.write_str(&format!("u8 literal {}", i)),
            U16(i) => f.write_str(&format!("u16 literal {}", i)),
            U32(i) => f.write_str(&format!("u32 literal {}", i)),
            U64(i) => f.write_str(&format!("u64 literal {}", i)),
            I8(i) => f.write_str(&format!("i8 literal {}", i)),
            I16(i) => f.write_str(&format!("i16 literal {}", i)),
            I32(i) => f.write_str(&format!("i32 literal {}", i)),
            I64(i) => f.write_str(&format!("i64 literal {}", i)),
            Bool(b) => f.write_str(&format!("bool literal {}", b)),
            Identifier(id) => f.write_str(&format!("identifier {}", id)),
            StringLiteral(str) => f.write_str(&format!("literal \"{}\"", str)),
            VarArgs => f.write_str("..."),
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
            LBracket => f.write_str("["),
            RBracket => f.write_str("]"),
            Init => f.write_str("init"),
            Yield => f.write_str("yield"),
            YieldReturn => f.write_str("yret"),
            CoroutineDef => f.write_str("co"),
            FunctionDef => f.write_str("fn"),
            ModuleDef => f.write_str("mod"),
            Struct => f.write_str("struct"),
            Extern => f.write_str("extern"),
            If => f.write_str("if"),
            While => f.write_str("while"),
            Else => f.write_str("else"),
            Colon => f.write_str(":"),
            MemberAccess => f.write_str("."),
            PathSeparator => f.write_str("::"),
            LArrow => f.write_str("->"),
            Primitive(p) => f.write_str(&format!("{}", p)),
            PathSelf => f.write_str("self"),
            PathSuper => f.write_str("super"),
            PathFileRoot => f.write_str("root"),
            PathProjectRoot => f.write_str("project"),
        }
    }
}

impl CompilerDisplay for Lex {
    fn fmt(&self, _: &SourceMap, st: &crate::StringTable) -> Result<String, CompilerDisplayError> {
        match self {
            Lex::Identifier(sid) => Ok(format!("identifier {}", st.get(*sid)?)),
            Lex::StringLiteral(sid) => Ok(format!("string literal {}", st.get(*sid)?)),
            _ => Ok(format!("{}", self)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// The value of the token
    pub sym: Lex,

    pub span: Span,
}

impl SourceIr for Token {
    fn span(&self) -> Span {
        self.span
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("S{}: {}", self.span, self.sym))
    }
}

impl CompilerDisplay for Token {
    fn fmt(&self, sm: &SourceMap, st: &crate::StringTable) -> Result<String, CompilerDisplayError> {
        Ok(format!("{}", self.sym.fmt(sm, st)?))
    }
}

impl Token {
    pub fn new(s: Lex, span: Span) -> Token {
        Token { sym: s, span }
    }

    pub fn token_eq(&self, a: &Lex) -> bool {
        // TODO: is there a way to make this easier to code
        match self.sym {
            Lex::U8(_) => match a {
                Lex::U8(_) => true,
                _ => false,
            },
            Lex::U16(_) => match a {
                Lex::U16(_) => true,
                _ => false,
            },
            Lex::U32(_) => match a {
                Lex::U32(_) => true,
                _ => false,
            },
            Lex::U64(_) => match a {
                Lex::U64(_) => true,
                _ => false,
            },
            Lex::I8(_) => match a {
                Lex::I8(_) => true,
                _ => false,
            },
            Lex::I16(_) => match a {
                Lex::I16(_) => true,
                _ => false,
            },
            Lex::I32(_) => match a {
                Lex::I32(_) => true,
                _ => false,
            },
            Lex::I64(_) => match a {
                Lex::I64(_) => true,
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
            Lex::VarArgs
            | Lex::Mul
            | Lex::Div
            | Lex::Add
            | Lex::Minus
            | Lex::Not
            | Lex::BAnd
            | Lex::BOr
            | Lex::GrEq
            | Lex::LsEq
            | Lex::Gr
            | Lex::Ls
            | Lex::Eq
            | Lex::NEq
            | Lex::Assign
            | Lex::Semicolon
            | Lex::Comma
            | Lex::Let
            | Lex::Mut
            | Lex::Return
            | Lex::LParen
            | Lex::RParen
            | Lex::LBrace
            | Lex::RBrace
            | Lex::LBracket
            | Lex::RBracket
            | Lex::Init
            | Lex::Yield
            | Lex::YieldReturn
            | Lex::CoroutineDef
            | Lex::FunctionDef
            | Lex::ModuleDef
            | Lex::Struct
            | Lex::Extern
            | Lex::If
            | Lex::Else
            | Lex::While
            | Lex::Colon
            | Lex::MemberAccess
            | Lex::PathSeparator
            | Lex::PathSelf
            | Lex::PathSuper
            | Lex::PathFileRoot
            | Lex::PathProjectRoot
            | Lex::LArrow => *a == self.sym,
        }
    }
}
