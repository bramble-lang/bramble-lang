use braid_lang::result::Result;
use crate::ast::{Module, Path, Type};

use super::{semanticnode::SemanticAnnotations, symbol_table::{ScopeType, Symbol, SymbolTable}};

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTableScopeStack<'a> {
    stack: Vec<SymbolTable>,
    pub(super) root: &'a Module<SemanticAnnotations>,
}

impl<'a> std::fmt::Display for SymbolTableScopeStack<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut i = 0;
        for scope in self.stack.iter() {
            f.write_fmt(format_args!("{}: {}\n", i, scope))?;
            i += 1;
        }
        Ok(())
    }
}

impl<'a> SymbolTableScopeStack<'a> {
    pub fn new(root: &'a Module<SemanticAnnotations>) -> SymbolTableScopeStack {
        SymbolTableScopeStack { stack: vec![], root }
    }

    pub fn push(&mut self, sym: &SymbolTable) {
        self.stack.push(sym.clone());
    }

    pub fn pop(&mut self) {
        self.stack.pop();
    }

    pub fn root(&self) -> &Module<SemanticAnnotations> {
        self.root
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

    /**
    Given a type reference that appears in the current node, will convert that type reference
    to a canonical path from a relative path.  If the type reference is already an absolute
    path then no change is made.

    For example, the path `super::MyStruct` would be converted to `root::my_module::MyStruct`
    if the current node were in a module contained within `my_module`.
     */
    pub fn canonize_local_type_ref(&self, sym: &'a SymbolTable, ty: &Type) -> Result<Type> {
        match ty {
            Type::Custom(path) => Ok(Type::Custom(
                self.to_canonical(sym, path)?
            )),
            Type::Coroutine(ty) => Ok(Type::Coroutine(Box::new(self.canonize_local_type_ref(sym, &ty)?))),
            _ => Ok(ty.clone()),
        }
    }

    /// Converts a relative path, `path`, into a canonical path by merging it with
    /// the path to the current node, as represented by the stack.
    pub fn to_canonical(&self, sym: &'a SymbolTable, path: &Path) -> Result<Path> {
        let current_path = self.to_path(sym).ok_or("A valid path is expected")?;
        path.to_canonical(&current_path)
    }

    /// Starting from the bottom of the stack this builds a path
    /// of all the modules that we are current in, in effect
    /// the current path within the AST.
    pub fn to_path(&self, current: &SymbolTable) -> Option<Path> {
        let mut steps = vec![];

        for node in self.stack.iter() {
            match node.scope_type() {
                ScopeType::Module { name } => {
                    steps.push(name.clone());
                }
                _ => (),
            }
        }

        match current.scope_type() {
            ScopeType::Module { name } => steps.push(name.clone()),
            _ => (),
        }

        if steps.len() > 0 {
            Some(steps.into())
        } else {
            None
        }
    }
}

/*#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_stack_to_path() {
        let stack = SymbolTableScopeStack::new();
        let local = SymbolTable::new();
        let path = stack.to_path(&local);
        assert_eq!(path, None);
    }

    #[test]
    fn test_one_module_stack_to_path() {
        let mut stack = SymbolTableScopeStack::new();
        let sym = SymbolTable::new_module("root");
        stack.push(sym);
        let local = SymbolTable::new();
        let path = stack.to_path(&local).unwrap();
        let expected = vec!["root"].into();
        assert_eq!(path, expected);
    }

    #[test]
    fn test_one_module_stack_module_current_to_path() {
        let mut stack = SymbolTableScopeStack::new();
        let sym = SymbolTable::new_module("root");
        stack.push(sym);
        let current = SymbolTable::new_module("inner");
        let path = stack.to_path(&current).unwrap();
        let expected = vec!["root", "inner"].into();
        assert_eq!(path, expected);
    }

    #[test]
    fn test_local_then_one_module_stack_to_path() {
        let mut stack = SymbolTableScopeStack::new();
        let module = SymbolTable::new_module("root");
        stack.push(module);
        let local = SymbolTable::new();
        stack.push(local);
        let local2 = SymbolTable::new();
        let path = stack.to_path(&local2).unwrap();
        let expected = vec!["root"].into();
        assert_eq!(path, expected);
    }

    #[test]
    fn test_local_then_two_module_stack_to_path() {
        let mut stack = SymbolTableScopeStack::new();
        let module = SymbolTable::new_module("first");
        stack.push(module);
        let module2 = SymbolTable::new_module("second");
        stack.push(module2);
        let local = SymbolTable::new();
        stack.push(local);
        let local2 = SymbolTable::new();
        let path = stack.to_path(&local2).unwrap();
        let expected = vec!["first", "second"].into();
        assert_eq!(path, expected);
    }
}
*/