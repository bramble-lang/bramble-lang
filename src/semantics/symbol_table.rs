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
    sym: Vec<Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            sym: vec![],
        }
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        //self.sym.get(name)
        self.sym.iter().find(|s| s.name == name)
    }

    pub fn add(&mut self, name: &str, ty: Type) -> Result<(), String> {
        if self.get(name).is_some() {
            Err(format!("{} already defined", name))
        } else {
            self.sym.push(
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
