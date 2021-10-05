#[cfg(test)]
mod stack_tests {
    use super::super::super::*;
    use crate::{
        compiler::{
            ast::{Element, Module, Type},
            semantics::{semanticnode::SemanticContext, symbol_table::SymbolTable},
        },
        StringTable,
    };
    use stack::*;

    #[test]
    fn test_empty_stack_to_path() {
        let mut table = StringTable::new();
        let test = table.insert("test".into());

        let m = Module::new(test, SemanticContext::new_module(1, 1, test));
        let mut stack = SymbolTableScopeStack::new(&m, &vec![]);
        let sym = SymbolTable::new_module(test);
        stack.enter_scope(&sym);
        let local = SymbolTable::new();
        stack.enter_scope(&local);
        let path = stack.to_path().unwrap();
        assert_eq!(path, vec![Element::CanonicalRoot, Element::Id(test)].into());
        assert!(path.is_canonical());
    }

    #[test]
    fn to_path_is_canonical() {
        let mut table = StringTable::new();
        let test = table.insert("test".into());
        let inner = table.insert("inner".into());

        let m = Module::new(test, SemanticContext::new_module(1, 1, test));
        let mut stack = SymbolTableScopeStack::new(&m, &vec![]);
        let sym = SymbolTable::new_module(test);
        stack.enter_scope(&sym);
        let sym = SymbolTable::new_module(inner);
        stack.enter_scope(&sym);
        let local = SymbolTable::new();
        stack.enter_scope(&local);
        let path = stack.to_path().unwrap();
        assert!(path.is_canonical());
    }

    #[test]
    fn test_one_module_stack_to_path() {
        let mut table = StringTable::new();
        let test = table.insert("test".into());
        let inner = table.insert("inner".into());

        let m = Module::new(test, SemanticContext::new_module(1, 1, test));
        let mut stack = SymbolTableScopeStack::new(&m, &vec![]);
        let sym = SymbolTable::new_module(test);
        stack.enter_scope(&sym);
        let sym = SymbolTable::new_module(inner);
        stack.enter_scope(&sym);
        let local = SymbolTable::new();
        stack.enter_scope(&local);
        let path = stack.to_path().unwrap();
        let expected = vec![
            Element::CanonicalRoot,
            Element::Id(test),
            Element::Id(inner),
        ]
        .into();
        assert_eq!(path, expected);
        assert!(path.is_canonical());
    }

    #[test]
    fn test_one_module_stack_module_current_to_path() {
        let mut table = StringTable::new();
        let test = table.insert("test".into());
        let test_mod = table.insert("test_mod".into());
        let inner = table.insert("inner".into());

        let m = Module::new(test, SemanticContext::new_module(1, 1, test));
        let mut stack = SymbolTableScopeStack::new(&m, &vec![]);
        let sym = SymbolTable::new_module(test);
        stack.enter_scope(&sym);
        let sym = SymbolTable::new_module(test_mod);
        stack.enter_scope(&sym);
        let current = SymbolTable::new_module(inner);
        stack.enter_scope(&current);
        let path = stack.to_path().unwrap();
        let expected = vec![
            Element::CanonicalRoot,
            Element::Id(test),
            Element::Id(test_mod),
            Element::Id(inner),
        ]
        .into();
        assert_eq!(path, expected);
    }

    #[test]
    fn test_get_current_fn() {
        let mut table = StringTable::new();
        let test = table.insert("test".into());
        let test_mod = table.insert("test_mod".into());
        let inner = table.insert("inner".into());

        let m = Module::new(test, SemanticContext::new_module(1, 1, test));
        let mut stack = SymbolTableScopeStack::new(&m, &vec![]);
        let sym = SymbolTable::new_module(test);
        stack.enter_scope(&sym);
        let sym = SymbolTable::new_module(test_mod);
        stack.enter_scope(&sym);
        let current = SymbolTable::new_routine(inner);
        stack.enter_scope(&current);
        let current_fn = stack.get_current_fn();
        let expected = Some(inner);
        assert_eq!(current_fn, expected);
    }

    #[test]
    fn test_get_current_fn_none() {
        let mut table = StringTable::new();
        let test = table.insert("test".into());
        let test_mod = table.insert("test_mod".into());

        let m = Module::new(test, SemanticContext::new_module(1, 1, test));
        let mut stack = SymbolTableScopeStack::new(&m, &vec![]);
        let sym = SymbolTable::new_module(test);
        stack.enter_scope(&sym);
        let sym = SymbolTable::new_module(test_mod);
        stack.enter_scope(&sym);
        let current_fn = stack.get_current_fn();
        let expected = None;
        assert_eq!(current_fn, expected);
    }

    #[test]
    fn test_local_then_one_module_stack_to_path() {
        let mut table = StringTable::new();
        let test = table.insert("test".into());
        let inner = table.insert("inner".into());

        let m = Module::new(test, SemanticContext::new_module(1, 1, test));
        let mut stack = SymbolTableScopeStack::new(&m, &vec![]);
        let sym = SymbolTable::new_module(test);
        stack.enter_scope(&sym);
        let module = SymbolTable::new_module(inner);
        stack.enter_scope(&module);
        let local = SymbolTable::new();
        stack.enter_scope(&local);
        let local2 = SymbolTable::new();
        stack.enter_scope(&local2);
        let path = stack.to_path().unwrap();
        let expected = vec![
            Element::CanonicalRoot,
            Element::Id(test),
            Element::Id(inner),
        ]
        .into();
        assert_eq!(path, expected);
    }

    #[test]
    fn test_local_then_two_module_stack_to_path() {
        let mut table = StringTable::new();
        let test = table.insert("test".into());
        let first = table.insert("first".into());
        let second = table.insert("second".into());

        let m = Module::new(test, SemanticContext::new_module(1, 1, test));
        let mut stack = SymbolTableScopeStack::new(&m, &vec![]);
        let sym = SymbolTable::new_module(test);
        stack.enter_scope(&sym);

        // Module 1
        let module = SymbolTable::new_module(first);
        stack.enter_scope(&module);

        // Module 2
        let module2 = SymbolTable::new_module(second);
        stack.enter_scope(&module2);
        let local = SymbolTable::new();
        stack.enter_scope(&local);
        let local2 = SymbolTable::new();
        stack.enter_scope(&local2);

        let path = stack.to_path().unwrap();
        let expected = vec![
            Element::CanonicalRoot,
            Element::Id(test),
            Element::Id(first),
            Element::Id(second),
        ]
        .into();
        assert_eq!(path, expected);
    }

    #[test]
    fn test_local_get_symbol() {
        let mut table = StringTable::new();
        let test = table.insert("test".into());
        let first = table.insert("first".into());
        let x = table.insert("x".into());

        let m = Module::new(test, SemanticContext::new_module(1, 1, test));
        let mut stack = SymbolTableScopeStack::new(&m, &vec![]);
        let sym = SymbolTable::new_module(test);
        stack.enter_scope(&sym);

        // Module 1
        let module = SymbolTable::new_module(first);
        stack.enter_scope(&module);

        let local = SymbolTable::new();
        stack.enter_scope(&local);
        stack.add(x, Type::I8, false, false).unwrap();

        let (s, _) = stack
            .lookup_symbol_by_path(&vec![Element::Id(x)].into())
            .unwrap();
        assert_eq!(s.name, x);
    }

    #[test]
    fn test_local_get_symbol_in_parent_scope() {
        let mut table = StringTable::new();
        let test = table.insert("test".into());
        let first = table.insert("first".into());
        let x = table.insert("x".into());

        let m = Module::new(test, SemanticContext::new_module(1, 1, test));
        let mut stack = SymbolTableScopeStack::new(&m, &vec![]);
        let sym = SymbolTable::new_module(test);
        stack.enter_scope(&sym);

        // Module 1
        let module = SymbolTable::new_module(first);
        stack.enter_scope(&module);

        let local = SymbolTable::new();
        stack.enter_scope(&local);
        stack.add(x, Type::I8, false, false).unwrap();

        let local2 = SymbolTable::new();
        stack.enter_scope(&local2);

        let (s, _) = stack
            .lookup_symbol_by_path(&vec![Element::Id(x)].into())
            .unwrap();
        assert_eq!(s.name, x);
    }

    #[test]
    fn test_get_symbol_in_routine() {
        let mut table = StringTable::new();
        let test = table.insert("test".into());
        let first = table.insert("first".into());
        let x = table.insert("x".into());
        let my_func = table.insert("my_func".into());

        let m = Module::new(test, SemanticContext::new_module(1, 1, test));
        let mut stack = SymbolTableScopeStack::new(&m, &vec![]);
        let sym = SymbolTable::new_module(test);
        stack.enter_scope(&sym);

        // Module 1
        let module = SymbolTable::new_module(first);
        stack.enter_scope(&module);
        stack.add(x, Type::I8, false, false).unwrap();

        let func = SymbolTable::new_routine(my_func);
        stack.enter_scope(&func);

        let local2 = SymbolTable::new();
        stack.enter_scope(&local2);

        let (_s, p) = stack
            .lookup_symbol_by_path(&vec![Element::Id(x)].into())
            .unwrap();
        assert_eq!(
            p,
            vec![
                Element::CanonicalRoot,
                Element::Id(test),
                Element::Id(first),
                Element::Id(x)
            ]
            .into()
        );
    }

    #[test]
    fn test_local_get_symbol_across_boundary() {
        let mut table = StringTable::new();
        let test = table.insert("test".into());
        let first = table.insert("first".into());
        let second = table.insert("second".into());
        let third = table.insert("third".into());
        let x = table.insert("x".into());

        let m = Module::new(test, SemanticContext::new_module(1, 1, test));
        let mut stack = SymbolTableScopeStack::new(&m, &vec![]);
        let sym = SymbolTable::new_module(test);
        stack.enter_scope(&sym);

        // Module 1
        let module = SymbolTable::new_module(first);
        stack.enter_scope(&module);
        stack.add(x, Type::I8, false, false).unwrap();

        // Module 2
        let module2 = SymbolTable::new_module(second);
        stack.enter_scope(&module2);

        let local = SymbolTable::new();
        stack.enter_scope(&local);

        // across 1 boundary
        let (s, p) = stack
            .lookup_symbol_by_path(&vec![Element::Id(x)].into())
            .unwrap();
        assert_eq!(s.name, x);
        assert_eq!(
            p,
            vec![
                Element::CanonicalRoot,
                Element::Id(test),
                Element::Id(first),
                Element::Id(x)
            ]
            .into()
        );

        // across 2 boundaries
        // Module 2
        let module2 = SymbolTable::new_module(third);
        stack.enter_scope(&module2);

        let (s, p) = stack
            .lookup_symbol_by_path(&vec![Element::Id(x)].into())
            .unwrap();
        assert_eq!(s.name, x);
        assert_eq!(
            p,
            vec![
                Element::CanonicalRoot,
                Element::Id(test),
                Element::Id(first),
                Element::Id(second),
                Element::Id(x)
            ]
            .into()
        );
    }
}
