use std::collections::HashMap;

use crate::parser::Primitive;

pub enum Type {
    Primitive(Primitive),
    Function(Vec<Primitive>, Primitive),
    Coroutine(Vec<Primitive>, Primitive),
}

pub struct Symbol {
    name: String,
    ty: Type,
}

pub struct SymbolTable {
    sym: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable{
            sym: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.sym.get(name)
    }

    pub fn add(&mut self, name: &str, ty: Type) -> Result<(),String>{
        if self.sym.contains_key(name) {
            Err(format!("{} already defined", name))
        } else {
            self.sym.insert(name.into(), Symbol{
                name: name.into(),
                ty,
            });
            Ok(())
        }
    }
}
