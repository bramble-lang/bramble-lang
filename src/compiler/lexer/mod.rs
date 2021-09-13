use tokens::Primitive;

use super::CompilerErrorDisplay;

mod tests;

pub(crate) mod lexer;
pub(crate) mod stringtable;
pub(crate) mod tokens;

#[derive(Clone, Debug, PartialEq)]
pub struct LexerError {
    kind: LexerErrorKind,
}

impl CompilerErrorDisplay for LexerError {
    fn format(&self, _: &crate::StringTable) -> Result<String, String> {
        use LexerErrorKind::*;
        let msg = match self.kind {
            Locked(None) => format!("Lexer Locked on EOF"),
            Locked(Some(c)) => format!("Lexer locked on {}", c),
            InvalidEscapeSequence(c) => format!("Invalid escape sequence \\{}", c),
            ExpectedEscapeCharacter => format!("Expected an escape character after \\"),
            InvalidInteger => format!("Invalid integer"),
            UnexpectedSuffixType(ref prim) => format!("Invalid type suffix: {}", prim),
        };

        Ok(format!("{}", msg))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum LexerErrorKind {
    Locked(Option<char>),
    InvalidEscapeSequence(char),
    ExpectedEscapeCharacter,
    InvalidInteger,
    UnexpectedSuffixType(Primitive),
}
