use crate::{semantics::semanticnode::SemanticMetadata, syntax::ast};

use super::{struct_table::{StructDefinition, StructTable}, symbol_table::{Symbol, SymbolTable}};

#[derive(Debug, PartialEq)]
pub struct LayoutData {
    pub(super) offset: i32,
    pub(super) next_label: i32,
}

impl LayoutData {
    pub fn new(offset: i32) -> LayoutData {
        LayoutData {
            offset,
            next_label: 0,
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
    pub(super) level: Level,
    pub(super) ty: ast::Type,
    pub(super) symbols: SymbolTable,
    pub(super) structs: StructTable,
    pub(super) label: i32,
}

impl Scope {
    pub fn new(level: Level, label: i32, ty: ast::Type) -> Scope {
        Scope {
            level,
            ty,
            symbols: SymbolTable::new(),
            structs: StructTable::new(),
            label,
        }
    }

    pub fn level(&self) -> &Level {
        &self.level
    }

    pub fn local_allocation(&self) -> Option<i32> {
        self.level().allocation()
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

    pub fn get_struct(&self, name: &str) -> Option<&StructDefinition> {
        self.structs.get(name)
    }

    pub fn label(&self) -> i32 {
        self.label
    }

    pub fn ty(&self) -> &ast::Type {
        &self.ty
    }

    pub fn size_of(&self, ty: &ast::Type) -> Option<i32> {
        self.structs.size_of(ty)
    }

    pub(super) fn local_from(
        m: &SemanticMetadata,
        struct_table: &StructTable,
        current_layout: LayoutData,
    ) -> (Scope, LayoutData) {
        let mut layout = current_layout;
        let mut scope = Scope::new(Level::Local, 0, m.ty.clone());
        scope.label = layout.get_label();
        for s in m.sym.table().iter() {
            layout.offset =
                scope.insert(&s.name, struct_table.size_of(&s.ty).unwrap(), layout.offset);
        }
        (scope, layout)
    }

    pub(super) fn routine_from(
        m: &SemanticMetadata,
        struct_table: &StructTable,
        current_offset: i32,
    ) -> (Scope, i32) {
        let mut scope = Scope::new(
            Level::Routine {
                next_label: 0,
                allocation: 0,
            },
            0,
            m.ty.clone(),
        );
        let mut current_offset = current_offset;
        for s in m.sym.table().iter() {
            current_offset = scope.insert(
                &s.name,
                struct_table.size_of(&s.ty).unwrap(),
                current_offset,
            );
        }
        match scope.level {
            Level::Routine {
                ref mut allocation, ..
            } => *allocation = current_offset,
            _ => (),
        };
        (scope, current_offset)
    }
}

#[derive(Debug, PartialEq)]
pub enum Level {
    Local,
    Routine { next_label: i32, allocation: i32 },
}

impl Level {
    pub fn allocation(&self) -> Option<i32> {
        match self {
            Level::Local => None,
            Level::Routine { allocation, .. } => Some(*allocation),
        }
    }
}