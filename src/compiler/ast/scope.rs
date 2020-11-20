use std::collections::HashMap;

use crate::{
    semantics, semantics::semanticnode::SemanticMetadata, 
};

#[derive(Debug)]
pub struct ScopeStack<'a> {
    stack: Vec<&'a Scope>,
}

impl<'a> ScopeStack<'a> {
    pub fn new() -> ScopeStack<'a> {
        ScopeStack { stack: vec![] }
    }

    /// Push a new scope onto the stack.
    pub fn push(&mut self, scope: &'a Scope) {
        self.stack.push(scope);
    }

    /// Pop the current scope off of the stack
    pub fn pop(&mut self) -> Option<&'a Scope> {
        self.stack.pop()
    }

    /// Searches through the stack, starting at the top and going to the bottom, for a
    /// variable with the given name.  This will not search past the root scope of the
    /// current function.
    pub fn find(&self, name: &str) -> Option<&'a Symbol> {
        for scope in self.stack.iter().rev() {
            let t = scope.get(name);
            if t.is_some() {
                return t;
            }
            match scope.ty {
                Type::Block => (),
                Type::Routine { .. } => return None,
            }
        }

        None
    }
}

#[derive(Debug, PartialEq)]
pub struct Scope {
    pub(super) ty: Type,
    pub(super) symbols: SymbolTable,
}

impl Scope {
    pub fn new(ty: Type) -> Scope {
        Scope {
            ty,
            symbols: SymbolTable::new(),
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

    pub fn block_from(m: &SemanticMetadata, current_offset: i32) -> (Scope, i32) {
        let mut scope = Scope::new(Type::Block);
        let mut current_offset = current_offset;
        for s in m.sym.table().iter() {
            current_offset = scope.insert(&s.name, s.ty.size(), current_offset);
        }
        (scope, current_offset)
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
        assert_eq!(sym.offset, 8);
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
        assert_eq!(sym.offset, 8);
    }

    #[test]
    fn test_find_symbol_defined_in_both_scopes() {
        let mut stack = ScopeStack::new();

        let mut outer_scope = Scope::new(Type::Block);
        outer_scope.insert("x", 4, 4);
        stack.push(&outer_scope);
        let mut inner_scope = Scope::new(Type::Block);
        inner_scope.insert("x", 4, 16);
        stack.push(&inner_scope);

        let sym = stack.find("x").unwrap();
        assert_eq!(sym.name, "x");
        assert_eq!(sym.size, 4);
        assert_eq!(sym.offset, 20);
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

    #[test]
    fn test_find_symbol_does_not_pass_function() {
        let mut stack = ScopeStack::new();

        let mut outer_scope = Scope::new(Type::Block);
        outer_scope.insert("nope", 4, 4);
        stack.push(&outer_scope);

        let mut fun_scope = Scope::new(Type::Routine {
            next_label: 0,
            allocation: 8,
        });
        fun_scope.insert("y", 4, 4);
        fun_scope.insert("z", 4, 8);
        stack.push(&fun_scope);

        let mut inner_scope = Scope::new(Type::Block);
        inner_scope.insert("x", 4, 4);
        stack.push(&inner_scope);

        assert_eq!(stack.find("x").is_some(), true);
        assert_eq!(stack.find("y").is_some(), true);
        assert_eq!(stack.find("nope").is_some(), false);
    }
}