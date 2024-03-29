use self::semanticnode::{Addressability, SemanticContext};

use super::{
    ast::{Node, PointerMut, Type},
    diagnostics::{View, View2, Writable},
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

impl<T: Node<SemanticContext>> View2<T, SemanticError> for SemanticResult<T> {
    fn view<F: FnOnce(Result<&T, &CompilerError<SemanticError>>)>(self, f: F) -> Self {
        match &self {
            Ok(v) => f(Ok(v)),
            Err(err) => f(Err(err)),
        }

        self
    }
}

struct TypeOk<'a> {
    ty: &'a super::ast::Type,
    addressable: Addressability,
    refs: Vec<super::Span>,
}

impl<'a> Writable for TypeOk<'a> {
    fn write(&self, w: &dyn super::diagnostics::Writer) {
        w.write_text("\"");
        w.write(self.ty);
        w.write_text("\"");

        w.write_field("addressability", &self.addressable);

        if !self.refs.is_empty() {
            for r in &self.refs {
                w.write_span("ref", *r);
            }
        }
    }
}

impl Writable for Addressability {
    fn write(&self, w: &dyn super::diagnostics::Writer) {
        match self {
            Addressability::None => w.write_str("None"),
            Addressability::Value => w.write_str("Value"),
            Addressability::Addressable => w.write_str("Addressable"),
            Addressability::AddressableMutable => w.write_str("Mutable"),
        }
    }
}

impl Writable for Type {
    fn write(&self, w: &dyn super::diagnostics::Writer) {
        match self {
            Type::Null => w.write_text("null"),
            Type::U8 => w.write_text("u8"),
            Type::U16 => w.write_text("u16"),
            Type::U32 => w.write_text("u32"),
            Type::U64 => w.write_text("u64"),
            Type::I8 => w.write_text("i8"),
            Type::I16 => w.write_text("i16"),
            Type::I32 => w.write_text("i32"),
            Type::I64 => w.write_text("i64"),
            Type::F64 => w.write_text("f64"),
            Type::Bool => w.write_text("bool"),
            Type::StringLiteral => w.write_text("string"),
            Type::RawPointer(is_mut, ty) => {
                if *is_mut == PointerMut::Mut {
                    w.write_text("*mut ")
                } else {
                    w.write_text("*const ")
                }
                w.write(ty.as_ref());
            }
            Type::Array(ty, sz) => {
                w.write_text("[");
                w.write(ty.as_ref());
                w.write_text(&format!("; {}]", sz));
            }
            Type::Unit => w.write_text("Unit"),
            Type::Custom(p) => w.write_path(p),
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
