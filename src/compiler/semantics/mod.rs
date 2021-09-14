use super::{
    ast::{AstError, BinaryOperator, Path, Type},
    lexer::stringtable::StringId,
    CompilerError, CompilerErrorDisplay,
};

/*
 * Handles semantic analysis of a syntax tree.  This includes:
 * 1. Type checking: determing the type of every expression and making sure that the types match
 *    any type restrictions.
 * 2. Checking functions, variables, coroutines, etc. to make sure that they exist
 * 3. Constructing the symbol table for the code.
 *
 * These functions will take an AST that has ParserContext context and will output the AST updated
 * with SemanticAnnotations (including the type of each node and the symbol tables).
 */
mod canonize;
mod stack;
mod tests;

pub mod semanticnode;
pub mod symbol_table;
pub mod type_resolver;

/// Errors generated during semantic analysis of a compilation unit.
#[derive(Debug, PartialEq)]
pub enum SemanticError {
    NotVariable(StringId),
    NotRoutine(StringId),
    NotCoroutine(StringId),
    MultipleDefs(Path),
    PathNotFound(Path, Path),
    PathNotValid,
    NotDefined(StringId),
    EmptyPath,
    ArrayInvalidSize(usize),
    ArrayInconsistentElementTypes,
    ArrayIndexingInvalidType(Type),
    ArrayIndexingInvalidIndexType(Type),
    InvalidPath,
    AlreadyDeclared(StringId),
    PathTooSuper,
    BindExpected(Type, Type),
    VariableNotMutable(StringId),
    BindMismatch(StringId, Type, Type),
    YieldExpected(Type, Type),
    YieldInvalidLocation,
    ReturnExpected(Type, Type),
    ReturnInvalidLocation,
    MemberAccessInvalidRootType(Type),
    MemberAccessMemberNotFound(Path, StringId),
    IfExprMismatchArms(Type, Type),
    CondExpectedBool(Type),
    WhileInvalidType(Type),
    YieldInvalidType(Type),
    RoutineCallWrongNumParams(Path, usize, usize),
    FunctionParamsNotEnough(Path, usize, usize),
    StructExprWrongNumParams(usize, usize),
    StructExprMemberNotFound(Path, StringId),
    StructExprFieldTypeMismatch(Path, StringId, Type, Type),
    ExpectedSignedInteger(Type),
    ExpectedBool(Type),
    OpExpected(BinaryOperator, Type, Type, Type),
    RoutineParamTypeMismatch(Path, String),
    MainFnInvalidType,
    MainFnInvalidParams,
    InvalidStructure,
    RoutineCallInvalidTarget(super::ast::RoutineCall, Path, Type),
}

impl CompilerErrorDisplay for SemanticError {
    fn format(&self, _st: &crate::StringTable) -> Result<String, String> {
        todo!()
    }
}

impl From<AstError> for SemanticError {
    fn from(ae: AstError) -> Self {
        match ae {
            AstError::ModuleAlreadyContains(_) => todo!(),
            AstError::PathTooSuper => Self::PathTooSuper,
        }
    }
}

type SemanticResult<T> = Result<T, CompilerError<SemanticError>>;
