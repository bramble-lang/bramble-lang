use super::{
    ast::{AstError, Path},
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
#[derive(Debug)]
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
    InvalidPath,
    AlreadyDeclared(StringId),
    PathTooSuper,
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
