use std::num::{ParseFloatError, ParseIntError};

use crate::compiler::{
    source::SourceChar, CompilerDisplay, CompilerDisplayError, SourceError, SourceMap,
};

use super::tokens::Primitive;

/// Errors which can be encountered while tokenizing a compilation unit
#[derive(Clone, PartialEq, Debug)]
pub enum LexerError {
    UnexpectedEof,
    Locked(Option<SourceChar>),
    InvalidEscapeSequence(SourceChar),
    ExpectedEscapeCharacter,
    InvalidNumber,
    InvalidSuffixOnFloat,
    ParseIntError(Primitive, ParseIntError),
    ParseFloatError(Primitive, ParseFloatError),
    UnexpectedSuffixType(Primitive),
    SourceError, // TODO: make this more descriptive
}

impl CompilerDisplay for LexerError {
    fn fmt(&self, sm: &SourceMap, st: &crate::StringTable) -> Result<String, CompilerDisplayError> {
        use LexerError::*;
        let msg = match self {
            Locked(None) => format!("Lexer locked on EOF"),
            Locked(Some(c)) => format!("Lexer locked on {}", c),
            InvalidEscapeSequence(c) => format!("Invalid escape sequence \\{}", c),
            ExpectedEscapeCharacter => format!("Expected an escape character after \\"),
            InvalidNumber => format!("Invalid number"),
            UnexpectedSuffixType(ref prim) => format!("Invalid type suffix: {}", prim.fmt(sm, st)?),
            SourceError => format!("Error reading characters from source code"),
            UnexpectedEof => format!("Unexpected EOF"),
            InvalidSuffixOnFloat => format!("Invalid suffix after float literal."),
            ParseIntError(p, e) => format!("{} of {}", e, p),
            ParseFloatError(p, e) => format!("{} of {}", e, p),
        };

        Ok(format!("{}", msg))
    }
}

impl From<SourceError> for LexerError {
    fn from(_: SourceError) -> Self {
        Self::SourceError
    }
}

impl From<ParseIntError> for LexerError {
    fn from(e: ParseIntError) -> Self {
        format!("{:?}", e);
        Self::InvalidNumber
    }
}
