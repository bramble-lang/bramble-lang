use crate::{
    compiler::{
        ast::{BinaryOperator, Path, PathCanonizationError, RoutineCall, Type, UnaryOperator},
        CompilerDisplay, CompilerDisplayError, SourceMap,
    },
    StringId,
};

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
    WhileCondInvalidType(Type),
    YieldInvalidType(Type),
    RoutineCallWrongNumParams(Path, usize, usize),
    FunctionParamsNotEnough(Path, usize, usize),
    StructExprWrongNumParams(usize, usize),
    StructExprMemberNotFound(Path, StringId),
    StructExprFieldTypeMismatch(Path, StringId, Type, Type),
    ExpectedSignedInteger(UnaryOperator, Type),
    ExpectedBool(UnaryOperator, Type),
    OpExpected(BinaryOperator, Type, Type, Type),
    RoutineParamTypeMismatch(Path, Vec<(u32, Type, Type)>),
    MainFnInvalidType,
    MainFnInvalidParams,
    InvalidStructure,
    RoutineCallInvalidTarget(RoutineCall, Path, Type),
    InvalidIdentifierType(Type),
}

impl CompilerDisplay for SemanticError {
    /// Turn a SemanticError into a human readable message.  This will convert all StringIds
    /// to their associated string value.
    fn fmt(&self, sm: &SourceMap, st: &crate::StringTable) -> Result<String, CompilerDisplayError> {
        match self {
            SemanticError::NotVariable(sid) => {
                Ok(format!("{} is not a variable", sid.fmt(sm, st)?))
            }
            SemanticError::NotRoutine(sid) => Ok(format!("{} is not a routine", sid.fmt(sm, st)?)),
            SemanticError::NotCoroutine(sid) => {
                Ok(format!("{} is not a coroutine", sid.fmt(sm, st)?))
            }
            SemanticError::MultipleDefs(path) => {
                Ok(format!("{} is defined multiple times", path.fmt(sm, st)?))
            }
            SemanticError::PathNotFound(path, canonical_form) => Ok(format!(
                "Could not find item with the given path: {} ({})",
                path.fmt(sm, st)?,
                canonical_form.fmt(sm, st)?
            )),
            SemanticError::PathNotValid => Ok(format!("Path is not valid")),
            SemanticError::NotDefined(sid) => Ok(format!(
                "Could not find definition for {} in this scope",
                sid.fmt(sm, st)?
            )),
            SemanticError::EmptyPath => Ok(format!("Empty path")),
            SemanticError::ArrayInvalidSize(sz) => {
                Ok(format!("Expected length > 0 for array, but found {}", sz))
            }
            SemanticError::ArrayInconsistentElementTypes => {
                Ok(format!("Inconsistent types in array value"))
            }
            SemanticError::ArrayIndexingInvalidType(ty) => Ok(format!(
                "Expected array type on LHS of [] but found {}",
                ty.fmt(sm, st)?
            )),
            SemanticError::ArrayIndexingInvalidIndexType(ty) => Ok(format!(
                "Expected integral type for index but found {}",
                ty.fmt(sm, st)?
            )),
            SemanticError::AlreadyDeclared(sid) => {
                Ok(format!("{} already declared", sid.fmt(sm, st)?))
            }
            SemanticError::PathTooSuper => Ok(format!("Path too super")),
            SemanticError::BindExpected(expected, actual) => Ok(format!(
                "Bind expected {} but got {}",
                expected.fmt(sm, st)?,
                actual.fmt(sm, st)?
            )),
            SemanticError::VariableNotMutable(sid) => {
                Ok(format!("Variable {} is not mutable", sid.fmt(sm, st)?))
            }
            SemanticError::BindMismatch(sid, expected, actual) => Ok(format!(
                "{} is of type {} but is assigned {}",
                sid.fmt(sm, st)?,
                expected.fmt(sm, st)?,
                actual.fmt(sm, st)?
            )),
            SemanticError::YieldExpected(expected, actual) => Ok(format!(
                "Yield return expected {} but got {}",
                expected.fmt(sm, st)?,
                actual.fmt(sm, st)?
            )),
            SemanticError::YieldInvalidLocation => Ok(format!("yield must be at end of function")),
            SemanticError::ReturnExpected(expected, actual) => Ok(format!(
                "Return expected {} but got {}",
                expected.fmt(sm, st)?,
                actual.fmt(sm, st)?
            )),
            SemanticError::ReturnInvalidLocation => Ok(format!("return invalid loc")),
            SemanticError::MemberAccessInvalidRootType(_) => {
                Ok(format!("Member access invalid root type"))
            }
            SemanticError::MemberAccessMemberNotFound(path, member) => Ok(format!(
                "{} does not have member {}",
                path.fmt(sm, st)?,
                member.fmt(sm, st)?
            )),
            SemanticError::IfExprMismatchArms(t, f) => Ok(format!(
                "If expression has mismatching arms: expected {} got {}",
                t.fmt(sm, st)?,
                f.fmt(sm, st)?
            )),
            SemanticError::CondExpectedBool(actual) => Ok(format!(
                "Expected boolean expression in if conditional, got: {}",
                actual.fmt(sm, st)?
            )),
            SemanticError::WhileInvalidType(actual) => Ok(format!(
                "The body of a while expression must resolve to the unit type, but got: {}",
                actual.fmt(sm, st)?
            )),
            SemanticError::WhileCondInvalidType(actual) => Ok(format!(
                "The condition of a while expression must resolve to the bool type, but got: {}",
                actual.fmt(sm, st)?
            )),
            SemanticError::YieldInvalidType(ty) => {
                Ok(format!("Yield expects co<_> but got {}", ty.fmt(sm, st)?))
            }
            SemanticError::RoutineCallWrongNumParams(path, expected, actual) => Ok(format!(
                "Incorrect number of parameters passed to routine: {}. Expected {} but got {}",
                path.fmt(sm, st)?,
                expected,
                actual,
            )),
            SemanticError::FunctionParamsNotEnough(path, expected, actual) => Ok(format!(
                "Function {} expects at least {} parameters, but got {}",
                path.fmt(sm, st)?,
                expected,
                actual,
            )),
            SemanticError::StructExprWrongNumParams(expected, actual) => Ok(format!(
                "Struct expected {} parameters but found {}",
                expected, actual
            )),
            SemanticError::StructExprMemberNotFound(path, sid) => Ok(format!(
                "member {} not found on {}",
                sid.fmt(sm, st)?,
                path.fmt(sm, st)?
            )),
            SemanticError::StructExprFieldTypeMismatch(path, fname, expected, actual) => {
                Ok(format!(
                    "{}.{} expects {} but got {}",
                    path.fmt(sm, st)?,
                    fname.fmt(sm, st)?,
                    expected.fmt(sm, st)?,
                    actual.fmt(sm, st)?
                ))
            }
            SemanticError::ExpectedSignedInteger(op, ty) => Ok(format!(
                "{} expected i32 or i64 but found {}",
                op,
                ty.fmt(sm, st)?
            )),
            SemanticError::ExpectedBool(op, ty) => Ok(format!(
                "{} expected bool but found {}",
                op,
                ty.fmt(sm, st)?
            )),
            SemanticError::OpExpected(op, expected, l, r) => Ok(format!(
                "{} expected {} but found {} and {}",
                op,
                expected.fmt(sm, st)?,
                l.fmt(sm, st)?,
                r.fmt(sm, st)?
            )),
            SemanticError::RoutineParamTypeMismatch(path, mismatches) => Ok(format!(
                "One or more parameters have mismatching types for function {}: {}",
                path.fmt(sm, st)?,
                mismatches
                    .iter()
                    .map(|(param_id, expected, actual)| {
                        Ok(format!(
                            "parameter {} expected {} but got {}",
                            param_id,
                            expected.fmt(sm, st)?,
                            actual.fmt(sm, st)?
                        ))
                    })
                    .collect::<Result<Vec<_>, CompilerDisplayError>>()?
                    .join(", ")
            )),
            SemanticError::MainFnInvalidType => {
                Ok(format!("my_main must be a function of type () -> i64"))
            }
            SemanticError::MainFnInvalidParams => Ok(format!(
                "my_main must take no parameters. It must be of type () -> i64"
            )),
            SemanticError::InvalidStructure => Ok(format!("Not a valid structure definition")),
            SemanticError::RoutineCallInvalidTarget(call, path, ty) => {
                let call = match call {
                    crate::compiler::ast::RoutineCall::Function => "function",
                    crate::compiler::ast::RoutineCall::CoroutineInit => "coroutine",
                    crate::compiler::ast::RoutineCall::Extern => "extern",
                };
                Ok(format!(
                    "Expected {} but {} is a {}",
                    call,
                    path.fmt(sm, st)?,
                    ty.fmt(sm, st)?
                ))
            }
            SemanticError::InvalidIdentifierType(ty) => Ok(format!(
                "Invalid type used in identifier declaration: {}",
                ty.fmt(sm, st)?
            )),
        }
    }
}

impl From<PathCanonizationError> for SemanticError {
    fn from(pe: PathCanonizationError) -> Self {
        match pe {
            PathCanonizationError::SubceedingRoot => Self::PathTooSuper, // TODO: maybe reevaluate why I get this from the AST in Semantic?
        }
    }
}
