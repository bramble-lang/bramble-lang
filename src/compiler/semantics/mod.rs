use super::CompilerError;

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
mod error;
mod stack;
mod tests;

pub mod semanticnode;
pub mod symbol_table;
pub mod type_resolver;

use error::SemanticError;

/// Captures the Failure state of any Semantic Analysis operation.
/// Which will, if it fails, result in a [`SemanticError`] wrapped
/// in a [`CompilerError`]
type SemanticResult<T> = Result<T, CompilerError<SemanticError>>;
