use std::collections::HashMap;

use crate::parser::Primitive;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Primitive(Primitive),
    Function(Vec<Primitive>, Primitive),
    Coroutine(Vec<Primitive>, Primitive),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
    sym: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            sym: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.sym.get(name)
    }

    pub fn add(&mut self, name: &str, ty: Type) -> Result<(), String> {
        if self.sym.contains_key(name) {
            Err(format!("{} already defined", name))
        } else {
            self.sym.insert(
                name.into(),
                Symbol {
                    name: name.into(),
                    ty,
                },
            );
            Ok(())
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ScopeStack {
    stack: Vec<SymbolTable>,
}

impl ScopeStack {
    pub fn new() -> ScopeStack {
        ScopeStack { stack: vec![] }
    }

    pub fn push(&mut self, sym: SymbolTable) {
        self.stack.push(sym);
    }

    pub fn pop(&mut self) {
        self.stack.pop();
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        for scope in self.stack.iter().rev() {
            match scope.get(name) {
                Some(v) => return Some(v),
                None => {}
            };
        }
        None
    }
}
