use std::collections::HashMap;

use crate::ast::{Module, Node, Path, Type};
use braid_lang::result::Result;

use super::{
    semanticnode::SemanticAnnotations,
    symbol_table::{ScopeType, Symbol, SymbolTable},
};

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTableScopeStack<'a> {
    stack: Vec<SymbolTable>,
    pub(super) root: &'a Module<SemanticAnnotations>,
    imported_symbols: HashMap<String, Symbol>,
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
        SymbolTableScopeStack {
            stack: vec![],
            root,
            imported_symbols: HashMap::new(),
        }
    }

    pub fn import_function(
        &mut self,
        canonical_name: Path,
        params: Vec<Type>,
        return_ty: Type,
    ) -> Option<Symbol> {
        match canonical_name.item() {
            Some(item) => self.imported_symbols.insert(
                canonical_name.to_string(),
                Symbol {
                    name: item.into(),
                    ty: Type::FunctionDef(params, Box::new(return_ty)),
                    mutable: false,
                },
            ),
            None => None,
        }
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

    pub fn lookup_var(&'a self, sym: &'a SymbolTable, id: &str) -> Result<&'a Symbol> {
        let (symbol, _) = &self.lookup_symbol_by_path(sym, &vec![id].into())?;
        match symbol.ty {
            Type::FunctionDef(..)
            | Type::CoroutineDef(..)
            | Type::StructDef { .. }
            | Type::Unknown => return Err(format!("{} is not a variable", id)),
            Type::Custom(..)
            | Type::Coroutine(_)
            | Type::I32
            | Type::I64
            | Type::Bool
            | Type::StringLiteral
            | Type::Unit => Ok(symbol),
        }
    }

    pub fn lookup_func_or_cor(&'a self, sym: &'a SymbolTable, id: &str) -> Result<(&Vec<Type>, &Type)> {
        match self.lookup_symbol_by_path(sym, &vec![id].into())?.0 {
            Symbol {
                ty: Type::CoroutineDef(params, p),
                ..
            }
            | Symbol {
                ty: Type::FunctionDef(params, p),
                ..
            } => Ok((params, p)),
            _ => return Err(format!("{} is not a coroutine or function", id)),
        }
    }

    pub fn lookup_coroutine(&'a self, sym: &'a SymbolTable, id: &str) -> Result<(&Vec<Type>, &Type)> {
        match self.lookup_symbol_by_path(sym, &vec![id].into())?.0 {
            Symbol {
                ty: Type::CoroutineDef(params, p),
                ..
            } => Ok((params, p)),
            _ => return Err(format!("{} is not a coroutine", id)),
        }
    }

    pub fn lookup_symbol_by_path(
        &'a self,
        sym: &'a SymbolTable,
        path: &Path,
    ) -> Result<(&'a Symbol, Path)> {
        let canon_path = self.to_canonical(sym, path)?;

        if path.len() > 1 {
            // If the path contains more than just the item's name then
            // traverse the parent path to find the specified item
            let item = canon_path
                .item()
                .expect("Expected a canonical path with at least one step in it");

            // Look in the project being compiled
            let project_symbol = self
                .root
                .go_to_module(&canon_path.parent())
                .map(|module| module.annotation().sym.get(&item))
                .flatten();

            // look in any imported symbols
            let imported_symbol = self.get_imported_symbol(&canon_path);

            // Make sure that there is no ambiguity about what is being referenced
            match (project_symbol, imported_symbol) {
                (Some(ps), None) => Ok((ps, canon_path)),
                (None, Some(is)) => Ok((is, canon_path)),
                (Some(_), Some(_)) => Err(format!("Found multiple definitions of {}", path)),
                (None, None) => Err(format!("Could not find item with the given path: {}", path)),
            }
        } else if path.len() == 1 {
            // If the path has just the item name, then check the local scope and
            // the parent scopes for the given symbol
            let item = &path[0];
            sym.get(item)
                .or_else(|| self.get(item))
                .map(|i| (i, canon_path))
                .ok_or(format!("{} is not defined", item))
        } else {
            Err("empty path passed to lookup_path".into())
        }
    }

    fn get_imported_symbol(&self, canonical_name: &Path) -> Option<&Symbol> {
        self.imported_symbols.get(&canonical_name.to_string())
    }

    /**
    Given a type reference that appears in a node that is not the curren node, will convert
    that type reference to a canonical path from a relative path.  If the type reference is
    already an absolute path then no change is made.  This is used for indirect type reference
    look ups: for example, if the current node is a routine call and the routine definition is
    looked up to validate the parameter types in the definition agains the parameters in the
    call, to canonize the routine definition's parameter types, this function would be used: as
    they are in the RoutineDef node not the RoutineCall node.
     */
    pub fn canonize_nonlocal_type_ref(&self, parent_path: &Path, ty: &Type) -> Result<Type> {
        match ty {
            Type::Custom(path) => Ok(Type::Custom(path.to_canonical(parent_path)?)),
            Type::Coroutine(ty) => Ok(Type::Coroutine(Box::new(
                self.canonize_nonlocal_type_ref(parent_path, &ty)?,
            ))),
            _ => Ok(ty.clone()),
        }
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
            Type::Custom(path) => Ok(Type::Custom(self.to_canonical(sym, path)?)),
            Type::Coroutine(ty) => Ok(Type::Coroutine(Box::new(
                self.canonize_local_type_ref(sym, &ty)?,
            ))),
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
