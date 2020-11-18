use std::collections::HashMap;

pub struct ScopeStack<'a> {
    stack: Vec<&'a Scope>,
}

impl<'a> ScopeStack<'a> {
    pub fn new() -> ScopeStack<'a> {
        ScopeStack {
            stack: vec![],
        }
    }

    pub fn push(&mut self, scope: &'a Scope) {
        self.stack.push(scope);
    }

    pub fn pop(&mut self) -> Option<&'a Scope> {
        self.stack.pop()
    }

    pub fn find(&self, name: &str) -> Option<&'a Symbol> {
        self.stack.iter().rev().find_map(|scope| scope.get(name))
    }
}

pub struct Scope {
    ty: Type,
    symbols: SymbolTable,
}

impl Scope {
    pub fn new(ty: Type) -> Scope {
        Scope {
            ty,
            symbols: SymbolTable::new(),
        }
    }

    pub fn insert(&mut self, name: &str, size: i32, offset: i32) -> i32 {
        self.symbols.table.insert(name.into(), Symbol::new(name, size, offset));
        offset + size
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.symbols.table.get(name)
    }
}

pub enum Type {
    Block,
    Function{
        next_label: i32,
        allocation: i32,
    }
}

pub struct SymbolTable {
    table: HashMap<String,Symbol>,
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_symbol_in_current_scope() {
        let mut scope = Scope::new(Type::Block);
        scope.insert("x", 4, 4);
        let mut stack = ScopeStack::new();
        stack.push(&scope);

        let sym = stack.find("x").unwrap();
        assert_eq!(sym.name, "x");
        assert_eq!(sym.size, 4);
        assert_eq!(sym.offset, 4);
    }

    #[test]
    fn test_find_symbol_in_outer_scope() {
        let mut stack = ScopeStack::new();

        let mut outer_scope = Scope::new(Type::Block);
        outer_scope.insert("x", 4, 4);
        stack.push(&outer_scope);
        
        let inner_scope = Scope::new(Type::Block);
        stack.push(&inner_scope);

        let sym = stack.find("x").unwrap();
        assert_eq!(sym.name, "x");
        assert_eq!(sym.size, 4);
        assert_eq!(sym.offset, 4);
    }

    #[test]
    fn test_find_symbol_defined_in_both_scopes() {
        let mut stack = ScopeStack::new();

        let mut outer_scope = Scope::new(Type::Block);
        outer_scope.insert("x", 4, 4);
        stack.push(&outer_scope);
        
        let mut inner_scope = Scope::new(Type::Block);
        inner_scope.insert("x", 4, 8);
        stack.push(&inner_scope);

        let sym = stack.find("x").unwrap();
        assert_eq!(sym.name, "x");
        assert_eq!(sym.size, 4);
        assert_eq!(sym.offset, 8);
    }

    #[test]
    fn test_find_symbol_does_not_exist() {
        let mut stack = ScopeStack::new();

        let mut outer_scope = Scope::new(Type::Block);
        outer_scope.insert("x", 4, 4);
        stack.push(&outer_scope);
        
        let inner_scope = Scope::new(Type::Block);
        stack.push(&inner_scope);

        assert_eq!(stack.find("y").is_none(), true);
    }
}