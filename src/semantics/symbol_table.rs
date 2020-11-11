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
    name: String,
    ty: Type,
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
pub struct ScopeStack<'a> {
    stack: Vec<&'a SymbolTable>,
}

impl<'a> ScopeStack<'a> {
    pub fn new() -> ScopeStack<'a> {
        ScopeStack { stack: vec![] }
    }

    pub fn push(&mut self, sym: &'a SymbolTable) {
        self.stack.push(sym);
    }

    pub fn pop(&mut self) {
        self.stack.pop();
    }
}
