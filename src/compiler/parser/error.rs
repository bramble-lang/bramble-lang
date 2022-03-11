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
    RawPointerExpectedType,
    RawPointerExpectedConstOrMut,
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
    ExpectedIdentifierAfter(Lex),
    AddressOfExpectedConstOrMut,
    MemberAccessExpectedField,
    IndexOpInvalidExpr,
    InvalidCastTarget,
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
            ParserError::ModExpectedName => "Identifier expected after mod keyword".into(),
            ParserError::ModAlreadyContains(sid) => {
                format!("Module already contains {}", sid.fmt(sm, st)?)
            }
            ParserError::ExternInvalidVarArgs => "An extern declaration must have at least one \
                    parameter before a VarArgs (...) parameter"
                .into(),
            ParserError::ExternExpectedFnDecl => {
                "Expected function declaration after extern keyword".into()
            }
            ParserError::StructExpectedIdentifier => {
                "Expected identifier after struct keyword".into()
            }
            ParserError::FnExpectedIdentifierAfterFn => {
                "Expected identifier after fn keyword".into()
            }
            ParserError::FnVarArgsNotAllowed => {
                "Varargs are not allowed in Bramble functions (only in externs)".into()
            }
            ParserError::FnExpectedTypeAfterArrow => "Type expected after ->".into(),
            ParserError::FnExpectedReturn(token) => {
                format!(
                    "Routines must end with a return statement, but found {}",
                    token_to_string(sm, st, token)?
                )
            }
            ParserError::FnCallExpectedParams => {
                "Expected parameters after function call point".into()
            }
            ParserError::CoExpectedIdentifierAfterCo => {
                "Expected identifier after co keyword".into()
            }
            ParserError::ArrayExpectedIntLiteral => {
                "Expected integer literal for array size".into()
            }
            ParserError::ArrayDeclExpectedType => "Expected type in array type declaration".into(),
            ParserError::ArrayDeclExpectedSize => {
                "Expected size to be specified in array type declaration".into()
            }
            ParserError::IdDeclExpectedType => {
                "Expected type after : in variable declaration".into()
            }
            ParserError::ExpectedButFound(expected, actual) => {
                format!(
                    "Expected {}, but found {}",
                    lex_set_to_string(sm, st, expected)?,
                    lex_to_string(sm, st, actual)?
                )
            }
            ParserError::ExpectedIdDeclAfterLet => {
                "Expected identifier declaration (`<id> : <type>`) after let".into()
            }
            ParserError::ExpectedTypeInIdDecl => {
                "Expected type specification in let binding".into()
            }
            ParserError::ExpectedExpressionOnRhs => "Expected expression after :=".into(),
            ParserError::ExpectedParams => "Expected parameter list after identifier".into(),
            ParserError::ExpectedIdAfterInit => "Expected identifer after init".into(),
            ParserError::NotAUnaryOp(op) => format!("{} is not a unary operator", op),
            ParserError::NotABinaryOp(op) => format!("{} is not a binary operator", op),
            ParserError::IfExpectedConditional => "Expected conditional expression after if".into(),
            ParserError::IfTrueArmMissingExpr => {
                "Expected expression block in true arm of if expression".into()
            }
            ParserError::IfElseExpectedIfExpr => "Expected expression block after else if".into(),
            ParserError::IfFalseArmMissingExpr => "Expected expression block after else".into(),
            ParserError::WhileExpectedConditional => {
                "Expected conditional after while keyword".into()
            }
            ParserError::WhileMissingBody => "Expected expression block for while loop body".into(),
            ParserError::PathExpectedIdentifier => "Expected identifier after ::".into(),
            ParserError::YieldExpectedIdentifier => "Expected identifier after yield".into(),
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
                "Expected member name after . operator.".into()
            }
            ParserError::IndexOpInvalidExpr => {
                "Index operator must contain valid expression".into()
            }
            ParserError::EmptyProject => "No source code.".into(),
            ParserError::RawPointerExpectedType => "Raw Pointer expected underlying type".into(),
            ParserError::RawPointerExpectedConstOrMut => "Expected const or mut after *".into(),
            ParserError::ExpectedIdentifierAfter(lex) => format!(
                "Expected identifier after {}",
                lex_to_string(sm, st, &Some(*lex))?
            ),
            ParserError::AddressOfExpectedConstOrMut => "Expected const or mut after @".into(),
            ParserError::InvalidCastTarget => "Can only cast to and from primitive types.".into(),
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
        .unwrap_or_else(|| Ok("EOF".into()))
}

fn lex_to_string(
    sm: &SourceMap,
    st: &StringTable,
    lex: &Option<Lex>,
) -> Result<String, CompilerDisplayError> {
    lex.as_ref()
        .map(|t| t.fmt(sm, st))
        .unwrap_or_else(|| Ok("EOF".into()))
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
