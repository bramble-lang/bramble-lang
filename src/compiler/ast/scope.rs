use std::collections::HashMap;

use crate::{semantics::semanticnode::SemanticMetadata, syntax::ast};

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

    pub fn insert(&mut self, name: &str, size: i32, offset: i32) -> i32 {
        self.symbols
            .table
            .insert(name.into(), Symbol::new(name, size, offset + size));
        offset + size
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.symbols.table.get(name)
    }

    pub fn add_struct(
        &mut self,
        name: &str,
        fields: Vec<(String, ast::Type)>,
    ) -> Result<(), String> {
        self.structs.add(name, fields)
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
        match ty {
            ast::Type::I32 => Some(4),
            ast::Type::Bool => Some(4),
            ast::Type::Custom(name) => {
                self.structs.get(name).map(|st| st.size).flatten()
            }
            ast::Type::CoroutineVal(..) => Some(4),
            _ => None,
        }
    }

    pub fn block_from(m: &SemanticMetadata, current_layout: LayoutData) -> (Scope, LayoutData) {
        let mut layout = current_layout;
        let mut scope = Scope::new(Level::Block, 0, m.ty.clone());
        scope.label = layout.get_label();
        for s in m.sym.table().iter() {
            layout.offset = scope.insert(&s.name, s.ty.size(None), layout.offset);
        }
        (scope, layout)
    }

    pub fn routine_from(
        m: &SemanticMetadata,
        struct_table: Option<&StructTable>,
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
            current_offset = scope.insert(&s.name, s.ty.size(struct_table), current_offset);
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
    Block,
    Routine { next_label: i32, allocation: i32 },
}

impl Level {
    pub fn allocation(&self) -> Option<i32> {
        match self {
            Level::Block => None,
            Level::Routine{allocation, ..} => Some(*allocation),
        }
    }
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
    pub fn size(&self, struct_table: Option<&StructTable>) -> i32 {
        match self {
            ast::Type::I32 => 4,
            ast::Type::Bool => 4,
            ast::Type::CoroutineVal(_) => 4,
            ast::Type::Custom(name) => {
                match struct_table {
                    Some(st) => st
                        .get(name)
                        .expect("Could not find struct")
                        .size
                        .unwrap_or(0),
                    _ => 0,
                    //_ => panic!("Attempting to look up a struct when there is not struct table"),
                }
            }
            ast::Type::Function(..) => 0,
            ast::Type::Coroutine(..) => 0,
            ast::Type::StructDef(..) => 0,
            ast::Type::Unit => 0,
            ast::Type::Unknown => panic!("Requesting size for a type of Unknown"),
        }
    }

    pub fn size2(&self, resolved_sz: &HashMap<String, Vec<i32>>) -> Option<i32> {
        match self {
            ast::Type::I32 => Some(4),
            ast::Type::Bool => Some(4),
            ast::Type::Custom(name) => {
                Some(resolved_sz.get(name)?.iter().sum())
            }
            ast::Type::CoroutineVal(_) => Some(4),
            _ => None,
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

    /// Attempt to resolve the size of every struct in this table
    /// On success, the StructTable will be updated such that every
    /// struct in the table has a Some value for its size.
    ///
    /// If a struct cannot have its size resolved (because of a circular
    /// dependency) then an error is returned. The StructTable will then
    /// be left with one or more Structs that do not have a resolved size.
    pub fn resolve_size(&mut self) -> Result<(), String> {
        // Create a counter that will count every time a struct is resolved
        let mut counter = 0;
        let mut resolved_sizes: HashMap<String, Vec<i32>> = HashMap::new();
        loop {
            for (_, st) in self.structs.iter() {
                // Check if we have already resolved this struct
                if resolved_sizes.contains_key(&st.name) {
                    continue;
                }

                match self.attempt_size_resolution(st, &resolved_sizes) {
                    Some(sz) => {
                        resolved_sizes.insert(st.name.clone(), sz);
                        counter += 1;
                    }
                    None => ()
                }
            }

            // If one or more structs were resolved then reset te counter and repeate
            // If no structs were resolved then equilibrium is reached and we stop the loop
            if counter > 0 {
                counter = 0;
            } else {
                break;
            }
        }

        // Run through all the structs and update their sizes
        for (_, st) in self.structs.iter_mut() {
            match resolved_sizes.get(&st.name) {
                Some(sz) => {
                    let mut total_offset = 0;
                    st.fields.iter_mut().zip(sz.iter()).for_each(|(f,sz)|{
                        total_offset += *sz;
                        f.2 = Some(total_offset);
                    });
                    st.size = Some(sz.iter().sum());
                },
                None => (),
            }
        }

        // If any struct has None for its size then return an error
        // Otherwise, return success
        match self.structs.iter().find(|(_, st)| st.size.is_none()) {
            Some((n, _)) => Err(format!("Struct {} cannot be resolved", n)),
            None => Ok(()),
        }
    }

    fn attempt_size_resolution(&self, st: &StructDefinition, resolved_sz: &HashMap<String, Vec<i32>>) -> Option<Vec<i32>> {
        // Loop through each struct in the table and attempt to resolve its size
        st
            .fields
            .iter()
            .map(|(_, ty, _)| ty.size2(resolved_sz))
            .collect::<Option<Vec<i32>>>()
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
            let sz = fty.size(None);
            if sz > 0 {
                total_sz += sz;
                let offset = if size_known { Some(total_sz) } else { None };
                nfields.push((fname.clone(), fty.clone(), offset));
            } else {
                size_known = false;
                nfields.push((fname.clone(), fty.clone(), None));
            }
        }

        StructDefinition {
            name: name.into(),
            fields: nfields,
            size: if size_known { Some(total_sz) } else { None },
        }
    }

    pub fn get_offset_of(&self, field: &str) -> Option<i32> {
        if self.size.is_none() {
            None
        } else {
            self.fields
                .iter()
                .find(|(fname, _, _)| fname == field)
                .map_or(None, |f| f.2)
        }
    }

    pub fn fields(&self) -> &Vec<(String, ast::Type, Option<i32>)> {
        &self.fields
    }
}
