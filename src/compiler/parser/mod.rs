use super::{CompilerError, CompilerErrorDisplay};

mod statement;
mod tests;
mod tokenstream;

pub mod expression;
pub mod parser;

type ParserError = CompilerError<ParserErrorKind>;

#[derive(Clone, Debug, PartialEq)]
pub enum ParserErrorKind {
    Locked(Option<super::lexer::tokens::Token>),
    ModExpectedName,
    ExternInvalidVarArgs,
    ExternExpectedFnDecl,
    StructExpectedIdentifier,
    FnVarArgsNotAllowed,
    FnExpectedReturn(Option<super::lexer::tokens::Token>),
    FnExpectedTypeAfterArrow,
    FnCallExpectedParams,
    PathExpectedColons,
    ArrayExpectedIntLiteral,
    IdDeclExpectedType,
    ExpectedButFound(super::lexer::tokens::Lex, super::lexer::tokens::Lex),
}

impl CompilerErrorDisplay for ParserErrorKind {
    fn format(&self, _st: &crate::StringTable) -> Result<String, String> {
        Ok(format!("Parser error"))
    }
}
