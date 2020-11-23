use std::collections::HashMap;

use crate::{
    semantics, semantics::semanticnode::SemanticMetadata, 
};

#[derive(Debug, PartialEq)]
pub struct LayoutData {
    pub(super) offset: i32,
    pub(super) next_label: i32,
}

impl LayoutData {
    pub fn new(offset:i32) -> LayoutData {
        LayoutData{
            offset,
            next_label: 0,
        }
    }
    
    pub fn new2(offset:i32, next_label: i32) -> LayoutData {
        LayoutData{
            offset,
            next_label,
        }
    }

    pub fn get_label(&mut self) -> i32 {
        let label = self.next_label;
        self.next_label += 1;
        label
    }
}

#[derive(Debug, PartialEq)]
pub struct Scope {
    pub(super) ty: Type,
    pub(super) symbols: SymbolTable,
    pub(super) label: i32,
}

impl Scope {
    pub fn new(ty: Type) -> Scope {
        Scope {
            ty,
            symbols: SymbolTable::new(),
            label: 0,
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

    pub fn block_from(m: &SemanticMetadata, current_layout: &mut LayoutData) -> (Scope, LayoutData) {
        let mut scope = Scope::new(Type::Block);
        scope.label = current_layout.get_label();
        let mut current_offset = current_layout.offset;
        for s in m.sym.table().iter() {
            current_offset = scope.insert(&s.name, s.ty.size(), current_offset);
        }
        (scope, LayoutData::new2(current_offset, current_layout.next_label))
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
