/*
 * Handles semantic analysis of a syntax tree.  This includes:
 * 1. Type checking: determing the type of every expression and making sure that the types match
 *    any type restrictions.
 * 2. Checking functions, variables, coroutines, etc. to make sure that they exist
 * 3. Constructing the symbol table for the code.
 *
 * These functions will take an AST that has ParserInfo metadata and will output the AST updated
 * with SemanticMetadata (including the type of each node and the symbol tables).
 */

pub mod semanticnode;
pub mod symbol_table;
pub mod type_checker;
mod stack;