use super::{
    ast::AstError,
    lexer::{stringtable::StringId, tokens::Lex},
    CompilerError, CompilerErrorDisplay,
};

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
    ModAlreadyContains(StringId),
    ExternInvalidVarArgs,
    ExternExpectedFnDecl,
    StructExpectedIdentifier,
    FnExpectedIdentifierAfterFn,
    FnVarArgsNotAllowed,
    FnExpectedTypeAfterArrow,
    FnExpectedReturn(Option<super::lexer::tokens::Token>),
    FnCallExpectedParams,
    CoExpectedIdentifierAfterCo,
    PathExpectedColons,
    ArrayExpectedIntLiteral,
    ArrayDeclExpectedType,
    ArrayDeclExpectedSize,
    IdDeclExpectedType,
    ExpectedButFound(Lex, Lex),
    EOF,
    ExpectedSemicolon(Option<Lex>),
    ExpectedIdDeclAfterLet,
    ExpectedTypeAfter,
    ExpectedExpressionOnRhs,
    ExpectedParams,
    ExpectedIdAfterInit,
}

impl CompilerErrorDisplay for ParserErrorKind {
    fn format(&self, _st: &crate::StringTable) -> Result<String, String> {
        Ok(format!("Parser error"))
    }
}

impl From<CompilerError<AstError>> for CompilerError<ParserErrorKind> {
    fn from(ce: CompilerError<AstError>) -> Self {
        let ae = ce.inner();
        match ae {
            AstError::ModuleAlreadyContains(sid) => {
                CompilerError::new(ce.line(), ParserErrorKind::ModAlreadyContains(sid))
            }
        }
    }
}
