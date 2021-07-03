#[cfg(test)]
mod stack_tests {
    use super::super::super::*;
    use crate::compiler::{
        ast::{Module, Type, CANONICAL_ROOT},
        semantics::{semanticnode::SemanticAnnotations, symbol_table::SymbolTable},
    };
    use stack::*;

    #[test]
    fn test_empty_stack_to_path() {
        let m = Module::new(
            "test",
            SemanticAnnotations::new_module(1, 1, "test", Type::Unit),
        );
        let mut stack = SymbolTableScopeStack::new(&m);
        let sym = SymbolTable::new_module("test");
        stack.enter_scope(&sym);
        let local = SymbolTable::new();
        stack.enter_scope(&local);
        let path = stack.to_path().unwrap();
        assert_eq!(path, vec![CANONICAL_ROOT, "test"].into());
        assert!(path.is_canonical());
    }

    #[test]
    fn to_path_is_canonical() {
        let m = Module::new(
            "test",
            SemanticAnnotations::new_module(1, 1, "test", Type::Unit),
        );
        let mut stack = SymbolTableScopeStack::new(&m);
        let sym = SymbolTable::new_module("test");
        stack.enter_scope(&sym);
        let sym = SymbolTable::new_module("inner");
        stack.enter_scope(&sym);
        let local = SymbolTable::new();
        stack.enter_scope(&local);
        let path = stack.to_path().unwrap();
        assert!(path.is_canonical());
    }

    #[test]
    fn test_one_module_stack_to_path() {
        let m = Module::new(
            "test",
            SemanticAnnotations::new_module(1, 1, "test", Type::Unit),
        );
        let mut stack = SymbolTableScopeStack::new(&m);
        let sym = SymbolTable::new_module("test");
        stack.enter_scope(&sym);
        let sym = SymbolTable::new_module("inner");
        stack.enter_scope(&sym);
        let local = SymbolTable::new();
        stack.enter_scope(&local);
        let path = stack.to_path().unwrap();
        let expected = vec![CANONICAL_ROOT, "test", "inner"].into();
        assert_eq!(path, expected);
        assert!(path.is_canonical());
    }

    #[test]
    fn test_one_module_stack_module_current_to_path() {
        let m = Module::new(
            "test",
            SemanticAnnotations::new_module(1, 1, "test", Type::Unit),
        );
        let mut stack = SymbolTableScopeStack::new(&m);
        let sym = SymbolTable::new_module("test");
        stack.enter_scope(&sym);
        let sym = SymbolTable::new_module("test_mod");
        stack.enter_scope(&sym);
        let current = SymbolTable::new_module("inner");
        stack.enter_scope(&current);
        let path = stack.to_path().unwrap();
        let expected = vec![CANONICAL_ROOT, "test", "test_mod", "inner"].into();
        assert_eq!(path, expected);
    }

    #[test]
    fn test_local_then_one_module_stack_to_path() {
        let m = Module::new(
            "test",
            SemanticAnnotations::new_module(1, 1, "test", Type::Unit),
        );
        let mut stack = SymbolTableScopeStack::new(&m);
        let sym = SymbolTable::new_module("test");
        stack.enter_scope(&sym);
        let module = SymbolTable::new_module("inner");
        stack.enter_scope(&module);
        let local = SymbolTable::new();
        stack.enter_scope(&local);
        let local2 = SymbolTable::new();
        stack.enter_scope(&local2);
        let path = stack.to_path().unwrap();
        let expected = vec![CANONICAL_ROOT, "test", "inner"].into();
        assert_eq!(path, expected);
    }

    #[test]
    fn test_local_then_two_module_stack_to_path() {
        let m = Module::new(
            "test",
            SemanticAnnotations::new_module(1, 1, "test", Type::Unit),
        );
        let mut stack = SymbolTableScopeStack::new(&m);
        let sym = SymbolTable::new_module("test");
        stack.enter_scope(&sym);

        // Module 1
        let module = SymbolTable::new_module("first");
        stack.enter_scope(&module);

        // Module 2
        let module2 = SymbolTable::new_module("second");
        stack.enter_scope(&module2);
        let local = SymbolTable::new();
        stack.enter_scope(&local);
        let local2 = SymbolTable::new();
        stack.enter_scope(&local2);

        let path = stack.to_path().unwrap();
        let expected = vec![CANONICAL_ROOT, "test", "first", "second"].into();
        assert_eq!(path, expected);
    }

    #[test]
    fn test_local_get_symbol() {
        let m = Module::new(
            "test",
            SemanticAnnotations::new_module(1, 1, "test", Type::Unit),
        );
        let mut stack = SymbolTableScopeStack::new(&m);
        let sym = SymbolTable::new_module("test");
        stack.enter_scope(&sym);

        // Module 1
        let module = SymbolTable::new_module("first");
        stack.enter_scope(&module);

        let local = SymbolTable::new();
        stack.enter_scope(&local);
        stack.add("x", Type::I8, false, false).unwrap();

        let (s, _) = stack.lookup_symbol_by_path(&vec!["x"].into()).unwrap();
        assert_eq!(s.name, "x");
    }

    #[test]
    fn test_local_get_symbol_in_parent_scope() {
        let m = Module::new(
            "test",
            SemanticAnnotations::new_module(1, 1, "test", Type::Unit),
        );
        let mut stack = SymbolTableScopeStack::new(&m);
        let sym = SymbolTable::new_module("test");
        stack.enter_scope(&sym);

        // Module 1
        let module = SymbolTable::new_module("first");
        stack.enter_scope(&module);

        let local = SymbolTable::new();
        stack.enter_scope(&local);
        stack.add("x", Type::I8, false, false).unwrap();

        let local2 = SymbolTable::new();
        stack.enter_scope(&local2);

        let (s, _) = stack.lookup_symbol_by_path(&vec!["x"].into()).unwrap();
        assert_eq!(s.name, "x");
    }

    #[test]
    fn test_local_get_symbol_across_boundary() {
        let m = Module::new(
            "test",
            SemanticAnnotations::new_module(1, 1, "test", Type::Unit),
        );
        let mut stack = SymbolTableScopeStack::new(&m);
        let sym = SymbolTable::new_module("test");
        stack.enter_scope(&sym);

        // Module 1
        let module = SymbolTable::new_module("first");
        stack.enter_scope(&module);
        stack.add("x", Type::I8, false, false).unwrap();

        // Module 2
        let module2 = SymbolTable::new_module("second");
        stack.enter_scope(&module2);

        let local = SymbolTable::new();
        stack.enter_scope(&local);

        // across 1 boundary
        let (s, p) = stack.lookup_symbol_by_path(&vec!["x"].into()).unwrap();
        assert_eq!(s.name, "x");
        assert_eq!(p, vec![CANONICAL_ROOT, "test", "first", "x"].into());

        // across 2 boundaries
        // Module 2
        let module2 = SymbolTable::new_module("third");
        stack.enter_scope(&module2);

        let (s, p) = stack.lookup_symbol_by_path(&vec!["x"].into()).unwrap();
        assert_eq!(s.name, "x");
        assert_eq!(
            p,
            vec![CANONICAL_ROOT, "test", "first", "second", "x"].into()
        );
    }
}
