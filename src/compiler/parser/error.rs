use crate::{
    compiler::{
        ast::AstError,
        lexer::tokens::{Lex, Token},
        CompilerDisplay, CompilerDisplayError, CompilerError, SourceMap,
    },
    StringId, StringTable,
};

/// Compiler errors that happen within the Parser stage of compilation.
/// These errors may also cover [AstError]s thrown by the AST during construction;
/// such errors will be transformed into [ParserError]s.
#[derive(Clone, Debug, PartialEq)]
pub enum ParserError {
    EmptyProject,
    Locked(Option<Token>),
    ModExpectedName,
    ModAlreadyContains(StringId),
    ExternInvalidVarArgs,
    ExternExpectedFnDecl,
    StructExpectedIdentifier,
    FnExpectedIdentifierAfterFn,
    FnVarArgsNotAllowed,
    FnExpectedTypeAfterArrow,
    FnExpectedReturn(Option<Token>),
    FnCallExpectedParams,
    CoExpectedIdentifierAfterCo,
    ArrayExpectedIntLiteral,
    ArrayDeclExpectedType,
    ArrayDeclExpectedSize,
    IdDeclExpectedType,
    ExpectedButFound(Vec<Lex>, Option<Lex>),
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

impl CompilerDisplay for ParserError {
    /// Format a ParserError into a human readable message and replace any [`StringId`]s
    /// with their respective string values.
    fn fmt(&self, sm: &SourceMap, st: &crate::StringTable) -> Result<String, CompilerDisplayError> {
        let msg = match self {
            ParserError::Locked(token) => {
                let ts = token_to_string(sm, st, token)?;
                format!("Parser cannot advance past {}", ts)
            }
            ParserError::ModExpectedName => format!("Identifier expected after mod keyword"),
            ParserError::ModAlreadyContains(sid) => {
                format!("Module already contains {}", sid.fmt(sm, st)?)
            }
            ParserError::ExternInvalidVarArgs => format!(
                "An extern declaration must have at least one \
                    parameter before a VarArgs (...) parameter"
            ),
            ParserError::ExternExpectedFnDecl => {
                format!("Expected function declaration after extern keyword")
            }
            ParserError::StructExpectedIdentifier => {
                format!("Expected identifier after struct keyword")
            }
            ParserError::FnExpectedIdentifierAfterFn => {
                format!("Expected identifier after fn keyword")
            }
            ParserError::FnVarArgsNotAllowed => {
                format!("Varargs are not allowed in Braid functions (only in externs)")
            }
            ParserError::FnExpectedTypeAfterArrow => format!("Type expected after ->"),
            ParserError::FnExpectedReturn(token) => {
                format!(
                    "Routines must end with a return statement, but found {}",
                    token_to_string(sm, st, token)?
                )
            }
            ParserError::FnCallExpectedParams => {
                format!("Expected parameters after function call point")
            }
            ParserError::CoExpectedIdentifierAfterCo => {
                format!("Expected identifier after co keyword")
            }
            ParserError::ArrayExpectedIntLiteral => {
                format!("Expected integer literal for array size")
            }
            ParserError::ArrayDeclExpectedType => {
                format!("Expected type in array type declaration")
            }
            ParserError::ArrayDeclExpectedSize => {
                format!("Expected size to be specified in array type declaration")
            }
            ParserError::IdDeclExpectedType => {
                format!("Expected type after : in variable declaration")
            }
            ParserError::ExpectedButFound(expected, actual) => {
                format!(
                    "Expected {}, but found {}",
                    lex_set_to_string(sm, st, expected)?,
                    lex_to_string(sm, st, actual)?
                )
            }
            ParserError::ExpectedIdDeclAfterLet => {
                format!("Expected identifier declaration (`<id> : <type>`) after let")
            }
            ParserError::ExpectedTypeInIdDecl => {
                format!("Expected type specification in let binding")
            }
            ParserError::ExpectedExpressionOnRhs => format!("Expected expression after :="),
            ParserError::ExpectedParams => format!("Expected parameter list after identifier"),
            ParserError::ExpectedIdAfterInit => format!("Expected identifer after init"),
            ParserError::NotAUnaryOp(op) => format!("{} is not a unary operator", op),
            ParserError::NotABinaryOp(op) => format!("{} is not a binary operator", op),
            ParserError::IfExpectedConditional => {
                format!("Expected conditional expression after if")
            }
            ParserError::IfTrueArmMissingExpr => {
                format!("Expected expression block in true arm of if expression")
            }
            ParserError::IfElseExpectedIfExpr => format!("Expected expression block after else if"),
            ParserError::IfFalseArmMissingExpr => format!("Expected expression block after else"),
            ParserError::WhileExpectedConditional => {
                format!("Expected conditional after while keyword")
            }
            ParserError::WhileMissingBody => {
                format!("Expected expression block for while loop body")
            }
            ParserError::PathExpectedIdentifier => format!("Expected identifier after ::"),
            ParserError::YieldExpectedIdentifier => format!("Expected identifier after yield"),
            ParserError::StructExpectedFieldExpr(sid) => format!(
                "Expected an expression to be assigned to field {}",
                sid.fmt(sm, st)?
            ),
            ParserError::ExpectedExprAfter(lex) => {
                format!(
                    "Expected expression after {}",
                    lex_to_string(sm, st, &Some(*lex))?
                )
            }
            ParserError::ExpectedTermAfter(lex) => {
                format!(
                    "Expected term after {}",
                    lex_to_string(sm, st, &Some(*lex))?
                )
            }
            ParserError::MemberAccessExpectedField => {
                format!("Expected member name after . operator.")
            }
            ParserError::IndexOpInvalidExpr => {
                format!("Index operator must contain valid expression")
            }
            ParserError::EmptyProject => format!("No source code."),
        };
        Ok(msg)
    }
}

fn token_to_string(
    sm: &SourceMap,
    st: &StringTable,
    token: &Option<Token>,
) -> Result<String, CompilerDisplayError> {
    token
        .as_ref()
        .map(|t| t.fmt(sm, st))
        .unwrap_or(Ok("EOF".into()))
}

fn lex_to_string(
    sm: &SourceMap,
    st: &StringTable,
    lex: &Option<Lex>,
) -> Result<String, CompilerDisplayError> {
    lex.as_ref()
        .map(|t| t.fmt(sm, st))
        .unwrap_or(Ok("EOF".into()))
}

fn lex_set_to_string(
    sm: &SourceMap,
    st: &StringTable,
    set: &[Lex],
) -> Result<String, CompilerDisplayError> {
    Ok(set
        .iter()
        .map(|l| l.fmt(sm, st))
        .collect::<Result<Vec<_>, _>>()?
        .join(" or "))
}

impl From<CompilerError<AstError>> for CompilerError<ParserError> {
    fn from(ce: CompilerError<AstError>) -> Self {
        let (span, ae) = ce.take();
        match ae {
            AstError::ModuleAlreadyContains(sid) => {
                CompilerError::new(span, ParserError::ModAlreadyContains(sid))
            }
        }
    }
}
