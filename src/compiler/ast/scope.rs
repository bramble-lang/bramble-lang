use std::collections::HashMap;

use crate::{
    semantics, semantics::semanticnode::SemanticMetadata, 
};

#[derive(Debug, PartialEq)]
pub struct Scope {
    pub(super) ty: Type,
    pub(super) symbols: SymbolTable,
}

impl Scope {
    pub fn new(ty: Type) -> Scope {
        Scope {
            ty,
            symbols: SymbolTable::new(),
        }
    }

    pub fn ty(&self) -> &Type {
        &self.ty
    }

    pub fn insert(&mut self, name: &str, size: i32, offset: i32) -> i32 {
        self.symbols
            .table
            .insert(name.into(), Symbol::new(name, size, offset + size));
        offset + size
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.symbols.table.get(name)
    }

    pub fn block_from(m: &SemanticMetadata, current_offset: i32) -> (Scope, i32) {
        let mut scope = Scope::new(Type::Block);
        let mut current_offset = current_offset;
        for s in m.sym.table().iter() {
            current_offset = scope.insert(&s.name, s.ty.size(), current_offset);
        }
        (scope, current_offset)
    }

    pub fn routine_from(m: &SemanticMetadata, current_offset: i32) -> (Scope, i32) {
        let mut scope = Scope::new(Type::Routine {
            next_label: 0,
            allocation: 0,
        });
        let mut current_offset = current_offset;
        for s in m.sym.table().iter() {
            current_offset = scope.insert(&s.name, s.ty.size(), current_offset);
        }
        match scope.ty {
            Type::Routine {
                ref mut allocation, ..
            } => *allocation = current_offset,
            _ => (),
        };
        (scope, current_offset)
    }
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Block,
    Routine { next_label: i32, allocation: i32 },
}

#[derive(Debug, PartialEq)]
pub struct SymbolTable {
    pub(super) table: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            table: HashMap::new(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub size: i32,
    pub offset: i32,
}

impl Symbol {
    pub fn new(name: &str, size: i32, offset: i32) -> Symbol {
        Symbol {
            name: name.into(),
            size,
            offset,
        }
    }
}

impl semantics::symbol_table::Type {
    pub fn size(&self) -> i32 {
        match self {
            semantics::symbol_table::Type::Primitive(crate::syntax::ast::Primitive::I32) => 4,
            semantics::symbol_table::Type::Primitive(crate::syntax::ast::Primitive::Bool) => 4,
            _ => 0,
        }
    }
}
