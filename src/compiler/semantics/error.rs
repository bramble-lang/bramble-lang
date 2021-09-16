use crate::compiler::{
    ast::{AstError, BinaryOperator, Path, RoutineCall, Type, UnaryOperator},
    lexer::stringtable::StringId,
    CompilerDisplay,
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
}

impl CompilerDisplay for SemanticError {
    /// Turn a SemanticError into a human readable message.  This will convert all StringIds
    /// to their associated string value.
    fn fmt(&self, st: &crate::StringTable) -> Result<String, String> {
        match self {
            SemanticError::NotVariable(_) => Ok(format!("Not a variable")),
            SemanticError::NotRoutine(_) => Ok(format!("Not a routine")),
            SemanticError::NotCoroutine(_) => Ok(format!("Not a coroutine")),
            SemanticError::MultipleDefs(_) => Ok(format!("Multiple defs")),
            SemanticError::PathNotFound(path, canonical_form) => Ok(format!(
                "Could not find item with the given path: {} ({})",
                path.to_human_string(st)?,
                canonical_form.to_human_string(st)?
            )),
            SemanticError::PathNotValid => Ok(format!("Path is not valid")),
            SemanticError::NotDefined(sid) => Ok(format!("{} is not defined", st.get(*sid)?)),
            SemanticError::EmptyPath => Ok(format!("Empty path")),
            SemanticError::ArrayInvalidSize(sz) => {
                Ok(format!("Expected length > 0 for array, but found {}", sz))
            }
            SemanticError::ArrayInconsistentElementTypes => {
                Ok(format!("Inconsistent types in array value"))
            }
            SemanticError::ArrayIndexingInvalidType(ty) => Ok(format!(
                "Expected array type on LHS of [] but found {}",
                ty.to_human_string(st)?
            )),
            SemanticError::ArrayIndexingInvalidIndexType(ty) => Ok(format!(
                "Expected integral type for index but found {}",
                ty.to_human_string(st)?
            )),
            SemanticError::AlreadyDeclared(sid) => {
                Ok(format!("{} already declared", st.get(*sid)?))
            }
            SemanticError::PathTooSuper => Ok(format!("Path too super")),
            SemanticError::BindExpected(expected, actual) => Ok(format!(
                "Bind expected {} but got {}",
                expected.to_human_string(st)?,
                actual.to_human_string(st)?
            )),
            SemanticError::VariableNotMutable(sid) => {
                Ok(format!("Variable {} is not mutable", st.get(*sid)?))
            }
            SemanticError::BindMismatch(sid, expected, actual) => Ok(format!(
                "{} is of type {} but is assigned {}",
                st.get(*sid)?,
                expected.to_human_string(st)?,
                actual.to_human_string(st)?
            )),
            SemanticError::YieldExpected(expected, actual) => Ok(format!(
                "Yield return expected {} but got {}",
                expected.to_human_string(st)?,
                actual.to_human_string(st)?
            )),
            SemanticError::YieldInvalidLocation => Ok(format!("yield must be at end of function")),
            SemanticError::ReturnExpected(expected, actual) => Ok(format!(
                "Return expected {} but got {}",
                expected.to_human_string(st)?,
                actual.to_human_string(st)?
            )),
            SemanticError::ReturnInvalidLocation => Ok(format!("return invalid loc")),
            SemanticError::MemberAccessInvalidRootType(_) => {
                Ok(format!("Member access invalid root type"))
            }
            SemanticError::MemberAccessMemberNotFound(path, member) => Ok(format!(
                "{} does not have member {}",
                path.to_human_string(st)?,
                st.get(*member)?
            )),
            SemanticError::IfExprMismatchArms(t, f) => Ok(format!(
                "If expression has mismatching arms: expected {} got {}",
                t.to_human_string(st)?,
                f.to_human_string(st)?
            )),
            SemanticError::CondExpectedBool(actual) => Ok(format!(
                "Expected boolean expression in if conditional, got: {}",
                actual.to_human_string(st)?
            )),
            SemanticError::WhileInvalidType(actual) => Ok(format!(
                "The body of a while expression must resolve to the unit type, but got: {}",
                actual.to_human_string(st)?
            )),
            SemanticError::WhileCondInvalidType(actual) => Ok(format!(
                "The condition of a while expression must resolve to the bool type, but got: {}",
                actual.to_human_string(st)?
            )),
            SemanticError::YieldInvalidType(ty) => Ok(format!(
                "Yield expects co<_> but got {}",
                ty.to_human_string(st)?
            )),
            SemanticError::RoutineCallWrongNumParams(path, expected, actual) => Ok(format!(
                "Incorrect number of parameters passed to routine: {}. Expected {} but got {}",
                path.to_human_string(st)?,
                expected,
                actual,
            )),
            SemanticError::FunctionParamsNotEnough(path, expected, actual) => Ok(format!(
                "Function {} expects at least {} parameters, but got {}",
                path.to_human_string(st)?,
                expected,
                actual,
            )),
            SemanticError::StructExprWrongNumParams(expected, actual) => Ok(format!(
                "Struct expected {} parameters but found {}",
                expected, actual
            )),
            SemanticError::StructExprMemberNotFound(path, sid) => Ok(format!(
                "member {} not found on {}",
                st.get(*sid)?,
                path.to_human_string(st)?
            )),
            SemanticError::StructExprFieldTypeMismatch(path, fname, expected, actual) => {
                Ok(format!(
                    "{}.{} expects {} but got {}",
                    path.to_human_string(st)?,
                    st.get(*fname)?,
                    expected.to_human_string(st)?,
                    actual.to_human_string(st)?
                ))
            }
            SemanticError::ExpectedSignedInteger(op, ty) => {
                Ok(format!("{} expected i32 or i64 but found {}", op, ty))
            }
            SemanticError::ExpectedBool(op, ty) => {
                Ok(format!("{} expected bool but found {}", op, ty))
            }
            SemanticError::OpExpected(op, expected, l, r) => Ok(format!(
                "{} expected {} but found {} and {}",
                op,
                expected.to_human_string(st)?,
                l.to_human_string(st)?,
                r.to_human_string(st)?
            )),
            SemanticError::RoutineParamTypeMismatch(path, mismatches) => Ok(format!(
                "One or more parameters have mismatching types for function {}: {}",
                path.to_human_string(st)?,
                mismatches
                    .iter()
                    .map(|(param_id, expected, actual)| {
                        Ok(format!(
                            "parameter {} expected {} but got {}",
                            param_id,
                            expected.to_human_string(st)?,
                            actual.to_human_string(st)?
                        ))
                    })
                    .collect::<Result<Vec<_>, String>>()?
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
                    path.to_human_string(st)?,
                    ty.to_human_string(st)?
                ))
            }
        }
    }
}

impl From<AstError> for SemanticError {
    /// If the AST generates an error during SemanticAnalysis then this will convert that
    /// error to an appropriate [SemanticError].
    ///
    /// Only non-structural changes to the AST are considered valid (e.g. traversing the AST via
    /// a path).  If the semantic analyzer attempts to change the structure of the AST (adding nodes)
    // then that is a fault.
    fn from(ae: AstError) -> Self {
        match ae {
            AstError::PathTooSuper => Self::PathTooSuper, // TODO: maybe reevaluate why I get this from the AST in Semantic?
            AstError::ModuleAlreadyContains(_) => {
                panic!("Semantic Analysis should never modify the structure  of the AST")
            }
        }
    }
}
