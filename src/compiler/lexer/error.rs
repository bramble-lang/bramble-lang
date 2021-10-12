use crate::compiler::{CompilerDisplay, CompilerDisplayError, SourceError};

use super::tokens::Primitive;

/// Errors which can be encountered while tokenizing a compilation unit
#[derive(Clone, PartialEq, Debug)]
pub enum LexerError {
    Locked(Option<char>),
    InvalidEscapeSequence(char),
    ExpectedEscapeCharacter,
    InvalidInteger,
    UnexpectedSuffixType(Primitive),
    SourceError, // TODO: make this more descriptive
}

impl CompilerDisplay for LexerError {
    fn fmt(&self, st: &crate::StringTable) -> Result<String, CompilerDisplayError> {
        use LexerError::*;
        let msg = match self {
            Locked(None) => format!("Lexer locked on EOF"),
            Locked(Some(c)) => format!("Lexer locked on {}", c),
            InvalidEscapeSequence(c) => format!("Invalid escape sequence \\{}", c),
            ExpectedEscapeCharacter => format!("Expected an escape character after \\"),
            InvalidInteger => format!("Invalid integer"),
            UnexpectedSuffixType(ref prim) => format!("Invalid type suffix: {}", prim.fmt(st)?),
            SourceError => format!("Error reading characters from source code"),
        };

        Ok(format!("{}", msg))
    }
}

impl From<SourceError> for LexerError {
    fn from(_: SourceError) -> Self {
        Self::SourceError
    }
}
