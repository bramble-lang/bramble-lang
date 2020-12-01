use std::collections::HashMap;

use crate::{semantics::semanticnode::SemanticMetadata, syntax::ast};

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

    pub fn get_label(&mut self) -> i32 {
        let label = self.next_label;
        self.next_label += 1;
        label
    }
}

#[derive(Debug, PartialEq)]
pub struct Scope {
    pub(super) ty: Level,
    pub(super) symbols: SymbolTable,
    pub(super) structs: StructTable,
    pub(super) label: i32,
}

impl Scope {
    pub fn new(ty: Level) -> Scope {
        Scope {
            ty,
            symbols: SymbolTable::new(),
            structs: StructTable::new(),
            label: 0,
        }
    }

    pub fn new2(ty: Level, label: i32) -> Scope {
        Scope {
            ty,
            symbols: SymbolTable::new(),
            structs: StructTable::new(),
            label,
        }
    }

    pub fn ty(&self) -> &Level {
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

    pub fn add_struct(&mut self, name: &str, fields: Vec<(String, ast::Type)>) -> Result<(), String> {
        self.structs.add(name, fields)
    }

    pub fn get_struct(&self, name: &str) -> Option<&StructDefinition> {
        self.structs.get(name)
    }

    pub fn label(&self) -> i32 {
        self.label
    }

    pub fn block_from(m: &SemanticMetadata, current_layout: LayoutData) -> (Scope, LayoutData) {
        let mut layout = current_layout;
        let mut scope = Scope::new(Level::Block);
        scope.label = layout.get_label();
        for s in m.sym.table().iter() {
            layout.offset = scope.insert(&s.name, s.ty.size(), layout.offset);
        }
        (scope, layout)
    }

    pub fn routine_from(m: &SemanticMetadata, current_offset: i32) -> (Scope, i32) {
        let mut scope = Scope::new(Level::Routine {
            next_label: 0,
            allocation: 0,
        });
        let mut current_offset = current_offset;
        for s in m.sym.table().iter() {
            current_offset = scope.insert(&s.name, s.ty.size(), current_offset);
        }
        match scope.ty {
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

impl ast::Type {
    pub fn size(&self) -> i32 {
        match self {
            ast::Type::I32 => 4,
            ast::Type::Bool => 4,
            _ => 0,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct StructTable {
    pub structs: HashMap<String, StructDefinition>,
}

impl StructTable {
    pub fn new() -> StructTable {
        StructTable {
            structs: HashMap::new(),
        }
    }

    pub fn add(&mut self, name: &str, fields: Vec<(String, ast::Type)>) -> Result<(), String> {
        if self.structs.contains_key(name) {
            Err(format!("Struct {} is already in the StructTable", name))
        } else {
            let struct_def = StructDefinition::new(name, fields);
            self.structs.insert(name.into(), struct_def);
            Ok(())
        }
    }

    pub fn get(&self, name: &str) -> Option<&StructDefinition> {
        self.structs.get(name)
    }
}

#[derive(Debug, PartialEq)]
pub struct StructDefinition {
    pub name: String,
    pub size: Option<i32>,
    pub fields: Vec<(String, ast::Type, Option<i32>)>,
}

impl StructDefinition {
    pub fn new(name: &str, fields: Vec<(String, ast::Type)>) -> StructDefinition {
        let mut nfields = vec![];
        let mut total_sz = 0;
        let mut size_known = true;
        for (fname, fty) in fields.iter() {
            let sz = fty.size();
            if sz > 0 {
                total_sz += sz;
                nfields.push((fname.clone(), fty.clone(), Some(sz)));
            } else {
                size_known = false;
                nfields.push((fname.clone(), fty.clone(), None));
            }
        }

        StructDefinition {
            name: name.into(),
            fields: nfields,
            size: if size_known {Some(total_sz)} else {None},
        }
    }
}