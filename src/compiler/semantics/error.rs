use crate::{
    compiler::{
        ast::{BinaryOperator, Path, PathCanonizationError, RoutineCall, Type, UnaryOperator},
        CompilerDisplay, CompilerDisplayError, SourceError, SourceMap, Span,
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
    ExpressionNotMutable(Span),
    BindMismatch(Span, Type, Type),
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
    ExpectedRawPointer(UnaryOperator, Type),
    OpExpected(BinaryOperator, Type, Type, Type),
    ExpectedIdentifier(UnaryOperator),
    ExpectedAddressable(UnaryOperator),
    RoutineParamTypeMismatch(Path, Vec<(u32, Type, Type)>),
    MainFnInvalidType,
    MainFnInvalidParams,
    InvalidStructure,
    MutablePointerToImmutable,
    RoutineCallInvalidTarget(RoutineCall, Path, Type),
    InvalidIdentifierType(Type),
    OffsetOperatorRequiresPointer(Type),
    OffsetOperatorRequiresInteger(Type),
    InvalidTypeCast,
}

impl CompilerDisplay for SemanticError {
    /// Turn a SemanticError into a human readable message.  This will convert all StringIds
    /// to their associated string value.
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        sm: &SourceMap,
        st: &crate::StringTable,
    ) -> Result<String, CompilerDisplayError> {
        match self {
            SemanticError::NotVariable(sid) => {
                Ok(format!("{} is not a variable", sid.fmt(f, sm, st)?))
            }
            SemanticError::NotRoutine(sid) => {
                Ok(format!("{} is not a routine", sid.fmt(f, sm, st)?))
            }
            SemanticError::NotCoroutine(sid) => {
                Ok(format!("{} is not a coroutine", sid.fmt(f, sm, st)?))
            }
            SemanticError::MultipleDefs(path) => Ok(format!(
                "{} is defined multiple times",
                path.fmt(f, sm, st)?
            )),
            SemanticError::PathNotFound(path, canonical_form) => Ok(format!(
                "Could not find item with the given path: {} ({})",
                path.fmt(f, sm, st)?,
                canonical_form.fmt(f, sm, st)?
            )),
            SemanticError::PathNotValid => Ok("Path is not valid".into()),
            SemanticError::NotDefined(sid) => Ok(format!(
                "Could not find definition for {} in this scope",
                sid.fmt(f, sm, st)?
            )),
            SemanticError::EmptyPath => Ok("Empty path".into()),
            SemanticError::ArrayInvalidSize(sz) => {
                Ok(format!("Expected length > 0 for array, but found {}", sz))
            }
            SemanticError::ArrayInconsistentElementTypes => {
                Ok("Inconsistent types in array value".into())
            }
            SemanticError::ArrayIndexingInvalidType(ty) => Ok(format!(
                "Expected array type on LHS of [] but found {}",
                ty.fmt(f, sm, st)?
            )),
            SemanticError::ArrayIndexingInvalidIndexType(ty) => Ok(format!(
                "Expected integral type for index but found {}",
                ty.fmt(f, sm, st)?
            )),
            SemanticError::AlreadyDeclared(sid) => {
                Ok(format!("{} already declared", sid.fmt(f, sm, st)?))
            }
            SemanticError::PathTooSuper => Ok("Path too super".into()),
            SemanticError::BindExpected(expected, actual) => Ok(format!(
                "Bind expected {} but got {}",
                expected.fmt(f, sm, st)?,
                actual.fmt(f, sm, st)?
            )),
            SemanticError::ExpressionNotMutable(span) => {
                Ok(format!("{} is not mutable", sm.text_in_span(*span)?))
            }
            SemanticError::BindMismatch(span, expected, actual) => Ok(format!(
                "{} is of type {} but is assigned {}",
                sm.text_in_span(*span)?,
                expected.fmt(f, sm, st)?,
                actual.fmt(f, sm, st)?
            )),
            SemanticError::YieldExpected(expected, actual) => Ok(format!(
                "Yield return expected {} but got {}",
                expected.fmt(f, sm, st)?,
                actual.fmt(f, sm, st)?
            )),
            SemanticError::YieldInvalidLocation => Ok("yield must be at end of function".into()),
            SemanticError::ReturnExpected(expected, actual) => Ok(format!(
                "Return expected {} but got {}",
                expected.fmt(f, sm, st)?,
                actual.fmt(f, sm, st)?
            )),
            SemanticError::ReturnInvalidLocation => Ok("return invalid loc".into()),
            SemanticError::MemberAccessInvalidRootType(_) => {
                Ok("Member access invalid root type".into())
            }
            SemanticError::MemberAccessMemberNotFound(path, member) => Ok(format!(
                "{} does not have member {}",
                path.fmt(f, sm, st)?,
                member.fmt(f, sm, st)?
            )),
            SemanticError::IfExprMismatchArms(t, f) => Ok(format!(
                "If expression has mismatching arms: expected {} got {}",
                t.fmt(f, sm, st)?,
                f.fmt(f, sm, st)?
            )),
            SemanticError::CondExpectedBool(actual) => Ok(format!(
                "Expected boolean expression in if conditional, got: {}",
                actual.fmt(f, sm, st)?
            )),
            SemanticError::WhileInvalidType(actual) => Ok(format!(
                "The body of a while expression must resolve to the unit type, but got: {}",
                actual.fmt(f, sm, st)?
            )),
            SemanticError::WhileCondInvalidType(actual) => Ok(format!(
                "The condition of a while expression must resolve to the bool type, but got: {}",
                actual.fmt(f, sm, st)?
            )),
            SemanticError::YieldInvalidType(ty) => Ok(format!(
                "Yield expects co<_> but got {}",
                ty.fmt(f, sm, st)?
            )),
            SemanticError::RoutineCallWrongNumParams(path, expected, actual) => Ok(format!(
                "Incorrect number of parameters passed to routine: {}. Expected {} but got {}",
                path.fmt(f, sm, st)?,
                expected,
                actual,
            )),
            SemanticError::FunctionParamsNotEnough(path, expected, actual) => Ok(format!(
                "Function {} expects at least {} parameters, but got {}",
                path.fmt(f, sm, st)?,
                expected,
                actual,
            )),
            SemanticError::StructExprWrongNumParams(expected, actual) => Ok(format!(
                "Struct expected {} parameters but found {}",
                expected, actual
            )),
            SemanticError::StructExprMemberNotFound(path, sid) => Ok(format!(
                "member {} not found on {}",
                sid.fmt(f, sm, st)?,
                path.fmt(f, sm, st)?
            )),
            SemanticError::StructExprFieldTypeMismatch(path, fname, expected, actual) => {
                Ok(format!(
                    "{}.{} expects {} but got {}",
                    path.fmt(f, sm, st)?,
                    fname.fmt(f, sm, st)?,
                    expected.fmt(f, sm, st)?,
                    actual.fmt(f, sm, st)?
                ))
            }
            SemanticError::ExpectedSignedInteger(op, ty) => Ok(format!(
                "{} expected i32 or i64 but found {}",
                op,
                ty.fmt(f, sm, st)?
            )),
            SemanticError::ExpectedBool(op, ty) => Ok(format!(
                "{} expected bool but found {}",
                op,
                ty.fmt(f, sm, st)?
            )),
            SemanticError::ExpectedRawPointer(op, ty) => Ok(format!(
                "{} expected a *mut or *const but found {}",
                op,
                ty.fmt(f, sm, st)?
            )),
            SemanticError::OpExpected(op, expected, l, r) => Ok(format!(
                "{} expected {} but found {} and {}",
                op,
                expected.fmt(f, sm, st)?,
                l.fmt(f, sm, st)?,
                r.fmt(f, sm, st)?
            )),
            SemanticError::RoutineParamTypeMismatch(path, mismatches) => Ok(format!(
                "One or more parameters have mismatching types for function {}: {}",
                path.fmt(f, sm, st)?,
                mismatches
                    .iter()
                    .map(|(param_id, expected, actual)| {
                        Ok(format!(
                            "parameter {} expected {} but got {}",
                            param_id,
                            expected.fmt(f, sm, st)?,
                            actual.fmt(f, sm, st)?
                        ))
                    })
                    .collect::<Result<Vec<_>, CompilerDisplayError>>()?
                    .join(", ")
            )),
            SemanticError::MainFnInvalidType => {
                Ok("my_main must be a function of type () -> i64".into())
            }
            SemanticError::MainFnInvalidParams => {
                Ok("my_main must take no parameters. It must be of type () -> i64".into())
            }
            SemanticError::InvalidStructure => Ok("Not a valid structure definition".into()),
            SemanticError::RoutineCallInvalidTarget(call, path, ty) => {
                let call = match call {
                    crate::compiler::ast::RoutineCall::Function => "function",
                    crate::compiler::ast::RoutineCall::CoroutineInit => "coroutine",
                    crate::compiler::ast::RoutineCall::Extern => "extern",
                };
                Ok(format!(
                    "Expected {} but {} is a {}",
                    call,
                    path.fmt(f, sm, st)?,
                    ty.fmt(f, sm, st)?
                ))
            }
            SemanticError::InvalidIdentifierType(ty) => Ok(format!(
                "Invalid type used in identifier declaration: {}",
                ty.fmt(f, sm, st)?
            )),
            SemanticError::MutablePointerToImmutable => {
                Ok("Cannot make mutable pointer to immutable variable".into())
            }
            SemanticError::ExpectedIdentifier(op) => Ok(format!("{} expected identifier", op)),
            SemanticError::ExpectedAddressable(op) => {
                Ok(format!("{} expected an addressable operand", op))
            }
            SemanticError::OffsetOperatorRequiresPointer(ty) => Ok(format!(
                "@ operator expects raw pointer on left side, but got {}",
                ty.fmt(f, sm, st)?
            )),
            SemanticError::OffsetOperatorRequiresInteger(ty) => Ok(format!(
                "@ operator expects integer on right side, but got {}",
                ty.fmt(f, sm, st)?
            )),
            SemanticError::InvalidTypeCast => Ok("Invalid type cast".into()),
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

impl From<SourceError> for CompilerDisplayError {
    fn from(se: SourceError) -> Self {
        Self::SourceError(se)
    }
}
