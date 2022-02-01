use crate::compiler::{ast::*, parser::ParserContext, Span};
use crate::StringId;

use super::{error::SemanticError, symbol_table::SymbolTable};

/// Indicates if an AST node resolves to having a value, to
/// a value in a location in memory, to a mutable location in memory, or
/// not a value at all. This property is essential for determing
/// if an expression resolves to a place which can be mutated or
/// referenced via addresses
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Addressability {
    None,
    Value,
    Addressable,
    AddressableMutable,
}

/// Contextual data that is derived during the semantic analysis process
/// This includes the resolved [`Type`] of a node or expression from the user
/// input and the scoped symbol tables
#[derive(Clone, Debug, PartialEq)]
pub struct SemanticContext {
    /// Unique id of a node in the Semantic AST
    id: u32,

    /// [`Span`] that this node represents
    span: Span,

    /// Resolved [`Type`] of this node
    ty: Type,

    /// How this node can be interpreted for address operations
    addressability: Addressability,

    /// Symbols which are within the scope of this node and its children
    sym: SymbolTable,

    /// Canonical path from the root of the project to this node
    canonical_path: Path,
}

impl Context for SemanticContext {
    fn id(&self) -> u32 {
        self.id
    }

    fn span(&self) -> Span {
        self.span
    }
}

pub type SemanticNode = Expression<SemanticContext>;

impl SemanticNode {
    pub fn get_type(&self) -> &Type {
        let meta = self.context();
        &meta.ty
    }
}

impl Statement<SemanticContext> {
    pub fn get_type(&self) -> &Type {
        let m = self.context();
        &m.ty
    }
}

impl SemanticContext {
    pub fn new_local(id: u32, ctx: ParserContext, ty: Type) -> SemanticContext {
        SemanticContext {
            id,
            span: ctx.span(),
            ty,
            addressability: Addressability::None,
            sym: SymbolTable::new(),
            canonical_path: Path::new(),
        }
    }

    pub fn new_routine(id: u32, ctx: ParserContext, name: StringId, ty: Type) -> SemanticContext {
        SemanticContext {
            id,
            span: ctx.span(),
            ty,
            addressability: Addressability::None,
            sym: SymbolTable::new_routine(name),
            canonical_path: Path::new(),
        }
    }

    pub fn new_module(id: u32, ctx: ParserContext, name: StringId) -> SemanticContext {
        SemanticContext {
            id,
            span: ctx.span(),
            ty: Type::Unit,
            addressability: Addressability::None,
            sym: SymbolTable::new_module(name),
            canonical_path: Path::new(),
        }
    }

    /// Creates a copy of this instances of [`SemanticContext`] but the [`Type`]
    /// field is assigned the value in `ty`
    pub fn with_type(&self, ty: Type) -> SemanticContext {
        let mut sm = self.clone();
        sm.ty = ty;
        sm
    }

    /// Updates this context to mark the node as Addressable.  If `is_mut` is
    /// `true` then this node will also be marked as mutable (which is a subset
    /// of Addressable).
    pub fn with_addressable(mut self, is_mut: bool) -> SemanticContext {
        if is_mut {
            self.addressability = Addressability::AddressableMutable;
        } else {
            self.addressability = Addressability::Addressable;
        }
        self
    }

    /// Creates a copy of this instances of [`SemanticContext`] but the [`SymbolTable`]
    /// field is assigned the value in `ty`
    pub fn with_sym(&self, sym: SymbolTable) -> SemanticContext {
        let mut sm = self.clone();
        sm.sym = sym;
        sm
    }

    /// Get the [`SymbolTable`] for a node in the AST.
    pub fn sym(&self) -> &SymbolTable {
        &self.sym
    }

    /// Get the [`Type`] for a node in the AST
    pub fn ty(&self) -> &Type {
        &self.ty
    }

    pub fn addressability(&self) -> Addressability {
        self.addressability
    }

    /// Returns true if the resolution of this expression has a location in memory and
    /// therefore has an address.
    pub fn is_addressable(&self) -> bool {
        match self.addressability {
            Addressability::Addressable | Addressability::AddressableMutable  => true,
            Addressability::None | Addressability::Value => false,
        }
    }

    /// Returns true if the resolution of this expression has a location in memory which
    /// is mutable.
    pub fn is_mutable(&self) -> bool {
        match self.addressability {
            Addressability::AddressableMutable  => true,
            Addressability::Addressable | Addressability::None | Addressability::Value => false,
        }
    }

    pub fn canonical_path(&self) -> &Path {
        &self.canonical_path
    }

    pub fn set_canonical_path(&mut self, path: Path) {
        self.canonical_path = path;
    }

    pub fn add_symbol(
        &mut self,
        name: StringId,
        ty: Type,
        mutable: bool,
        is_extern: bool,
        span: Span,
    ) -> Result<(), SemanticError> {
        self.sym.add(name, ty, mutable, is_extern, span)
    }
}

pub struct SemanticAst {
    next_id: u32,
}

impl SemanticAst {
    pub fn new() -> SemanticAst {
        SemanticAst { next_id: 0 }
    }

    pub fn from_module(&mut self, m: &Module<ParserContext>) -> Module<SemanticContext> {
        let f = |n: &dyn Node<ParserContext>| match n.node_type() {
            NodeType::Module => {
                let name = n.name().expect("Modules must have a name");
                self.module_semantic_context_from(*n.context(), name)
            }
            NodeType::RoutineDef(_) => {
                let name = n.name().expect("RoutineDefs must have a name");
                self.routine_semantic_context_from(*n.context(), name)
            }
            _ => self.semantic_context_from(*n.context()),
        };

        let mut mapper = MapPreOrder::new("parser-to-semantic", f);
        mapper.apply(m)
    }

    fn semantic_context_from(&mut self, ctx: ParserContext) -> SemanticContext {
        let sm_data = SemanticContext::new_local(self.next_id, ctx, Type::Unknown);
        self.next_id += 1;
        sm_data
    }

    fn routine_semantic_context_from(
        &mut self,
        ctx: ParserContext,
        name: StringId,
    ) -> SemanticContext {
        let sm_data = SemanticContext::new_routine(self.next_id, ctx, name, Type::Unknown);
        self.next_id += 1;
        sm_data
    }

    fn module_semantic_context_from(
        &mut self,
        ctx: ParserContext,
        name: StringId,
    ) -> SemanticContext {
        let sm_data = SemanticContext::new_module(self.next_id, ctx, name);
        self.next_id += 1;
        sm_data
    }
}
