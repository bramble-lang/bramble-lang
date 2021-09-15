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

/// Compiler errors that happen within the Parser stage of compilation.
/// These errors may also cover [AstError]s thrown by the AST during construction;
/// such errors will be transformed into [ParserError]s.
#[derive(Clone, Debug, PartialEq)]
pub enum ParserError {
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
    ArrayExpectedIntLiteral,
    ArrayDeclExpectedType,
    ArrayDeclExpectedSize,
    IdDeclExpectedType,
    ExpectedButFound(Lex, Option<Lex>),
    ExpectedIdDeclAfterLet,
    ExpectedTypeInIdDecl,
    ExpectedExpressionOnRhs,
    ExpectedParams,
    ExpectedIdAfterInit,
    NotAUnaryOp(Lex),
    NotABinaryOp(Lex),
    IfExpectedConditional,
    IfTrueArmMissingExpr,
    IfElseExpectedIfExpr,
    IfFalseArmMissingExpr,
    WhileExpectedConditional,
    WhileMissingBody,
    PathExpectedIdentifier,
    YieldExpectedIdentifier,
    StructExpectedFieldExpr(StringId),
    ExpectedExprAfter(Lex),
    ExpectedTermAfter(Lex),
    MemberAccessExpectedField,
    IndexOpInvalidExpr,
}

impl CompilerErrorDisplay for ParserError {
    /// Format a ParserError into a human readable message and replace any [`StringId`]s
    /// with their respective string values.
    fn format(&self, st: &crate::StringTable) -> Result<String, String> {
        let msg = match self {
            ParserError::Locked(_) => todo!(),
            ParserError::ModExpectedName => format!("Identifier expected after mod keyword"),
            ParserError::ModAlreadyContains(sid) => {
                format!("Module already contains {}", st.get(*sid)?)
            }
            ParserError::ExternInvalidVarArgs => format!(""),
            ParserError::ExternExpectedFnDecl => {
                format!("Function declaration expected after extern keyword")
            }
            ParserError::StructExpectedIdentifier => {
                format!("Identifier expected after struct keyword")
            }
            ParserError::FnExpectedIdentifierAfterFn => {
                format!("Identifier expected after fn keyword")
            }
            ParserError::FnVarArgsNotAllowed => {
                format!("Varargs are not allowed in Braid functions (only in externs)")
            }
            ParserError::FnExpectedTypeAfterArrow => format!("Type expected after ->"),
            ParserError::FnExpectedReturn(_) => format!("Expected return, but found {}", todo!()),
            ParserError::FnCallExpectedParams => {
                format!("Expected parameter list after function call point")
            }
            ParserError::CoExpectedIdentifierAfterCo => {
                format!("Expected identifier after co keyword")
            }
            ParserError::ArrayExpectedIntLiteral => todo!(),
            ParserError::ArrayDeclExpectedType => todo!(),
            ParserError::ArrayDeclExpectedSize => todo!(),
            ParserError::IdDeclExpectedType => todo!(),
            ParserError::ExpectedButFound(expected, actual) => {
                format!(
                    "Expected {}, but found {}",
                    expected,
                    actual
                        .as_ref()
                        .map(|l| l.to_string())
                        .unwrap_or("EOF".into())
                )
            }
            ParserError::ExpectedIdDeclAfterLet => {
                format!("Expected identifier declaration (`<id> : <type>`) after let")
            }
            ParserError::ExpectedTypeInIdDecl => todo!(),
            ParserError::ExpectedExpressionOnRhs => todo!(),
            ParserError::ExpectedParams => todo!(),
            ParserError::ExpectedIdAfterInit => todo!(),
            ParserError::NotAUnaryOp(_) => todo!(),
            ParserError::NotABinaryOp(_) => todo!(),
            ParserError::IfExpectedConditional => todo!(),
            ParserError::IfTrueArmMissingExpr => todo!(),
            ParserError::IfElseExpectedIfExpr => todo!(),
            ParserError::IfFalseArmMissingExpr => todo!(),
            ParserError::WhileExpectedConditional => todo!(),
            ParserError::WhileMissingBody => todo!(),
            ParserError::PathExpectedIdentifier => todo!(),
            ParserError::YieldExpectedIdentifier => todo!(),
            ParserError::StructExpectedFieldExpr(_) => todo!(),
            ParserError::ExpectedExprAfter(_) => todo!(),
            ParserError::ExpectedTermAfter(_) => todo!(),
            ParserError::MemberAccessExpectedField => todo!(),
            ParserError::IndexOpInvalidExpr => todo!(),
        };
        Ok(msg)
    }
}

impl From<CompilerError<AstError>> for CompilerError<ParserError> {
    fn from(ce: CompilerError<AstError>) -> Self {
        let (line, ae) = ce.take();
        match ae {
            AstError::ModuleAlreadyContains(sid) => {
                CompilerError::new(line, ParserError::ModAlreadyContains(sid))
            }
            AstError::PathTooSuper => todo!(), // TODO: Investigate why this error is in the AST?
        }
    }
}
