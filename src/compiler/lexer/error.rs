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
            Locked(None) => "Lexer locked on EOF".into(),
            Locked(Some(c)) => format!("Lexer locked on {}", c),
            InvalidEscapeSequence(c) => format!("Invalid escape sequence \\{}", c),
            ExpectedEscapeCharacter => "Expected an escape character after \\".into(),
            InvalidNumber => "Invalid number".into(),
            UnexpectedSuffixType(ref prim) => format!("Invalid type suffix: {}", prim.fmt(sm, st)?),
            SourceError => "Error reading characters from source code".into(),
            UnexpectedEof => "Unexpected EOF".into(),
            InvalidSuffixOnFloat => "Invalid suffix after float literal.".into(),
            ParseIntError(p, e) => format!("{} of {}", e, p),
            ParseFloatError(p, e) => format!("{} of {}", e, p),
        };

        Ok(msg)
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
