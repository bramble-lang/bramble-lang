use crate::{semantics::semanticnode::SemanticMetadata, syntax::ast};

use super::{
    struct_table::{StructDefinition, StructTable},
    symbol_table::{Symbol, SymbolTable},
};

#[derive(Debug, PartialEq)]
pub struct LayoutData {
    pub(super) offset: i32,
}

impl LayoutData {
    pub fn new(offset: i32) -> LayoutData {
        LayoutData { offset }
    }
}

#[derive(Debug, PartialEq)]
pub struct Scope {
    pub(super) id: u32,
    pub(super) line: u32,
    pub(super) level: Level,
    pub(super) ty: ast::Type,
    pub(super) symbols: SymbolTable,
    pub(super) structs: StructTable,
}

impl Scope {
    pub fn new(id: u32, level: Level, ty: ast::Type) -> Scope {
        Scope {
            id,
            line: 0,
            level,
            ty,
            symbols: SymbolTable::new(),
            structs: StructTable::new(),
        }
    }

    pub fn id(&self) -> u32 {
        self.id
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

    pub fn line(&self) -> u32 {
        self.line
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
        let mut scope = Scope::new(m.id, Level::Local, m.ty.clone());
        scope.line = m.ln;
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
            m.id,
            Level::Routine {
                next_label: 0,
                allocation: 0,
            },
            m.ty.clone(),
        );
        scope.line = m.ln;
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

    pub(super) fn module_from(
        m: &SemanticMetadata,
        name: &str,
        struct_table: &StructTable,
        current_layout: LayoutData,
    ) -> (Scope, LayoutData) {
        let mut layout = current_layout;
        let mut scope = Scope::new(m.id, Level::Module{name: name.into()}, m.ty.clone());
        scope.line = m.ln;
        for s in m.sym.table().iter() {
            layout.offset =
                scope.insert(&s.name, struct_table.size_of(&s.ty).unwrap(), layout.offset);
        }
        (scope, layout)
    }
}

impl std::fmt::Display for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Level: {} | ", self.level))?;
        f.write_fmt(format_args!("Type: {}\n", self.ty))?;
        f.write_fmt(format_args!(
            "Symbols (! prefix indicates anonymous symbol):\n{}",
            self.symbols
        ))?;
        f.write_fmt(format_args!("Structs:\n{}\n", self.structs))?;

        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum Level {
    Local,
    Routine { next_label: i32, allocation: i32 },
    Module{ name: String},
}

impl Level {
    pub fn allocation(&self) -> Option<i32> {
        match self {
            Level::Local | Level::Module{..} => None,
            Level::Routine { allocation, .. } => Some(*allocation),
        }
    }
}

impl std::fmt::Display for Level {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Level::Local => f.write_str("Local"),
            Level::Routine {
                next_label,
                allocation,
            } => f.write_fmt(format_args!(
                "Routine: [Next Label: {}, Allocation: {}]",
                next_label, allocation
            )),
            Level::Module{name} => {
                f.write_fmt(format_args!("Module: {}", name))
            }
        }
    }
}
