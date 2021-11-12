use super::{diagnostics::Writable, CompilerError};

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

struct TypeOk<'a> {
    ty: &'a super::ast::Type,
    refs: Vec<super::Span>,
}

impl<'a> Writable for TypeOk<'a> {
    fn write(&self, w: &dyn super::diagnostics::Writer) {
        if self.refs.len() > 0 {
            w.write_str("{");
            for r in &self.refs {
                w.write_span(*r);
            }
            w.write_str("} ");
        }

        match self.ty {
            super::ast::Type::U8 => w.write_str("u8"),
            super::ast::Type::U16 => w.write_str("u16"),
            super::ast::Type::U32 => w.write_str("u32"),
            super::ast::Type::U64 => todo!(),
            super::ast::Type::I8 => todo!(),
            super::ast::Type::I16 => todo!(),
            super::ast::Type::I32 => todo!(),
            super::ast::Type::I64 => w.write_str("i64"),
            super::ast::Type::Bool => w.write_str("bool"),
            super::ast::Type::StringLiteral => w.write_str("string"),
            super::ast::Type::Array(_, _) => todo!(),
            super::ast::Type::Unit => todo!(),
            super::ast::Type::Custom(_) => todo!(),
            super::ast::Type::StructDef(_) => todo!(),
            super::ast::Type::FunctionDef(_, _) => todo!(),
            super::ast::Type::CoroutineDef(_, _) => todo!(),
            super::ast::Type::Coroutine(_) => todo!(),
            super::ast::Type::ExternDecl(_, _, _) => todo!(),
            super::ast::Type::Unknown => w.write_str("Unknown"),
        }
    }
}
