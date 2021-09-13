use super::CompilerErrorDisplay;

mod statement;
mod tests;
mod tokenstream;

pub mod expression;
pub mod parser;

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
}

impl CompilerErrorDisplay for ParserErrorKind {
    fn format(&self, _st: &crate::StringTable) -> Result<String, String> {
        Ok(format!("Parser error"))
    }
}
