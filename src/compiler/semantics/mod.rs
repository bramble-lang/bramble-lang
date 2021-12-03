use self::semanticnode::SemanticContext;

use super::{
    ast::Type,
    diagnostics::{View, Writable},
    CompilerError,
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
        w.write_text("\"");
        w.write(self.ty);
        w.write_text("\"");

        if self.refs.len() > 0 {
            for r in &self.refs {
                w.write_span("ref", *r);
            }
        }
    }
}

impl Writable for Type {
    fn write(&self, w: &dyn super::diagnostics::Writer) {
        match self {
            Type::U8 => w.write_text("u8"),
            Type::U16 => w.write_text("u16"),
            Type::U32 => w.write_text("u32"),
            Type::U64 => w.write_text("u64"),
            Type::I8 => w.write_text("i8"),
            Type::I16 => w.write_text("i16"),
            Type::I32 => w.write_text("i32"),
            Type::I64 => w.write_text("i64"),
            Type::Bool => w.write_text("bool"),
            Type::StringLiteral => w.write_text("string"),
            Type::Array(ty, sz) => {
                w.write_text("[");
                w.write(ty.as_ref());
                w.write_text(&format!("; {}]", sz));
            }
            Type::Unit => w.write_text("Unit"),
            Type::Custom(p) => w.write_path(&p),
            Type::StructDef(_) => w.write_text("Struct Def"),
            Type::FunctionDef(_, _) => w.write_text("Function Def"),
            Type::CoroutineDef(_, _) => w.write_text("Coroutine Def"),
            Type::Coroutine(_) => w.write_text("Coroutine"),
            Type::ExternDecl(_, _, _) => w.write_text("Extern"),
            Type::Unknown => w.write_text("Unknown"),
        }
    }
}

impl<V: super::ast::Node<SemanticContext>> View<V> for Result<V, CompilerError<SemanticError>> {
    fn view<F: FnOnce(&V)>(self, f: F) -> Self {
        match &self {
            Ok(v) => f(v),
            Err(_) => (),
        }
        self
    }
}
