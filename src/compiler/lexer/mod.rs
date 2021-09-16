use tokens::Primitive;

use super::{CompilerDisplay, CompilerDisplayError};

mod tests;

pub(crate) mod lexer;
pub(crate) mod stringtable;
pub(crate) mod tokens;

/// Errors which can be encountered while tokenizing a compilation unit
#[derive(Clone, Debug, PartialEq)]
pub enum LexerError {
    Locked(Option<char>),
    InvalidEscapeSequence(char),
    ExpectedEscapeCharacter,
    InvalidInteger,
    UnexpectedSuffixType(Primitive),
}

impl CompilerDisplay for LexerError {
    fn fmt(&self, st: &crate::StringTable) -> Result<String, CompilerDisplayError> {
        use LexerError::*;
        let msg = match self {
            Locked(None) => format!("Lexer Locked on EOF"),
            Locked(Some(c)) => format!("Lexer locked on {}", c),
            InvalidEscapeSequence(c) => format!("Invalid escape sequence \\{}", c),
            ExpectedEscapeCharacter => format!("Expected an escape character after \\"),
            InvalidInteger => format!("Invalid integer"),
            UnexpectedSuffixType(ref prim) => format!("Invalid type suffix: {}", prim.fmt(st)?),
        };

        Ok(format!("{}", msg))
    }
}
