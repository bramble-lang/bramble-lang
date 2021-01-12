use crate::ast::Type;
use crate::compiler::ast::ast::CompilerNode;
use crate::compiler::ast::scope::Level;
use crate::{ast::RoutineDef, syntax::ast::Path};

use super::{struct_table::StructDefinition, symbol_table::Symbol};

#[derive(Debug)]
pub struct ScopeStack<'a> {
    stack: Vec<&'a CompilerNode>,
}

impl<'a> ScopeStack<'a> {
    pub fn new() -> ScopeStack<'a> {
        ScopeStack { stack: vec![] }
    }

    /// Push a new scope onto the stack.
    pub fn push(&mut self, scope: &'a CompilerNode) {
        self.stack.push(scope);
    }

    /// Pop the current scope off of the stack
    pub fn pop(&mut self) -> Option<&'a CompilerNode> {
        self.stack.pop()
    }

    /// Searches through the stack, starting at the top and going to the bottom, for a
    /// variable with the given name.  This will not search past the root scope of the
    /// current function.
    pub fn find(&self, name: &str) -> Option<&'a Symbol> {
        for node in self.stack.iter().rev() {
            let scope = node.get_metadata();
            let t = scope.get(name);
            if t.is_some() {
                return t;
            }
            match scope.level {
                Level::Local => (),
                Level::Routine { .. } | Level::Module { .. } => return None,
            }
        }

        None
    }

    /// Searched from the top of the stack to the bottom for a Node whose symbol table
    /// contains the given name.  If found it will return a reference to that node.
    /// Unlike `find` this function will not stop at Function boundaries. This allows
    /// it to be used for searching for functions defined in parent scopes all the way up
    /// to the module containing the current node.
    fn find_global(&self, name: &str) -> Option<&'a CompilerNode> {
        for node in self.stack.iter().rev() {
            let scope = node.get_metadata();
            if scope.get(name).is_some() {
                return Some(node);
            }
        }

        None
    }

    pub fn find_func(&self, name: &str) -> Option<&CompilerNode> {
        match self.find_global(name) {
            Some(ref node) => match node {
                CompilerNode::Module { functions, .. } => functions.iter().find(|v| match v {
                    CompilerNode::RoutineDef {
                        def: RoutineDef::Function,
                        name: n,
                        ..
                    } => n == name,
                    _ => false,
                }),
                _ => None,
            },
            None => None,
        }
    }

    pub fn find_coroutine(&self, name: &str) -> Option<&CompilerNode> {
        match self.find_global(name) {
            Some(ref node) => match node {
                CompilerNode::Module { coroutines, .. } => coroutines.iter().find(|v| match v {
                    CompilerNode::RoutineDef {
                        def: RoutineDef::Coroutine,
                        name: n,
                        ..
                    } => n == name,
                    _ => false,
                }),
                _ => None,
            },
            None => None,
        }
    }

    pub fn get_routine_allocation(&self, name: &str) -> Option<i32> {
        match self.find_func(name) {
            Some(func) => func,
            None => match self.find_coroutine(name) {
                Some(co) => co,
                None => return None,
            },
        }
        .get_metadata()
        .local_allocation()
    }

    pub fn get_struct(&self, name: &str) -> Option<&StructDefinition> {
        for node in self.stack.iter().rev() {
            match node.get_metadata().get_struct(name) {
                Some(def) => return Some(def),
                None => (),
            }
        }

        None
    }

    pub fn size_of(&self, ty: &Type) -> Option<i32> {
        for node in self.stack.iter().rev() {
            match node.get_metadata().size_of(ty) {
                Some(def) => return Some(def),
                None => (),
            }
        }

        None
    }

    /// Starting from the bottom of the stack this builds a path
    /// of all the modules that we are current in, in effect
    /// the current path within the AST.
    pub fn to_path(&self) -> Option<Path> {
        let mut steps = vec![];

        for node in self.stack.iter() {
            match node {
                CompilerNode::Module { name, .. } => {
                    steps.push(name.clone());
                }
                _ => (),
            }
        }

        if steps.len() > 0 {
            Some(steps.into())
        } else {
            None
        }
    }
}

impl std::fmt::Display for ScopeStack<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for scope in self.stack.iter() {
            f.write_fmt(format_args!("{}\n", scope.get_metadata()))?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::ast::scope::Scope;
    use crate::syntax::ast::Type;

    #[test]
    fn test_find_symbol_in_current_scope() {
        let mut scope = Scope::new(0, Level::Local, vec!["root"].into(), Type::Unit);
        scope.insert("x", 4, 4);
        let node = CompilerNode::ExpressionBlock(scope, vec![]);
        let mut stack = ScopeStack::new();
        stack.push(&node);

        let sym = stack.find("x").unwrap();
        assert_eq!(sym.name, "x");
        assert_eq!(sym.size, 4);
        assert_eq!(sym.offset, 8);
    }

    #[test]
    fn test_find_symbol_in_outer_scope() {
        let mut stack = ScopeStack::new();

        let mut outer_scope = Scope::new(0, Level::Local, vec!["root"].into(), Type::Unit);
        outer_scope.insert("x", 4, 4);
        let outer_node = CompilerNode::ExpressionBlock(outer_scope, vec![]);
        stack.push(&outer_node);
        let inner_scope = Scope::new(0, Level::Local, vec!["root"].into(), Type::Unit);
        let inner_node = CompilerNode::ExpressionBlock(inner_scope, vec![]);
        stack.push(&inner_node);

        let sym = stack.find("x").unwrap();
        assert_eq!(sym.name, "x");
        assert_eq!(sym.size, 4);
        assert_eq!(sym.offset, 8);
    }

    #[test]
    fn test_find_symbol_defined_in_both_scopes() {
        let mut stack = ScopeStack::new();

        let mut outer_scope = Scope::new(0, Level::Local, vec!["root"].into(), Type::Unit);
        outer_scope.insert("x", 4, 4);
        let outer_node = CompilerNode::ExpressionBlock(outer_scope, vec![]);
        stack.push(&outer_node);
        let mut inner_scope = Scope::new(0, Level::Local, vec!["root"].into(), Type::Unit);
        inner_scope.insert("x", 4, 16);
        let inner_node = CompilerNode::ExpressionBlock(inner_scope, vec![]);
        stack.push(&inner_node);

        let sym = stack.find("x").unwrap();
        assert_eq!(sym.name, "x");
        assert_eq!(sym.size, 4);
        assert_eq!(sym.offset, 20);
    }

    #[test]
    fn test_find_symbol_does_not_exist() {
        let mut stack = ScopeStack::new();

        let mut outer_scope = Scope::new(0, Level::Local, vec!["root"].into(), Type::Unit);
        outer_scope.insert("x", 4, 4);
        let outer_node = CompilerNode::ExpressionBlock(outer_scope, vec![]);
        stack.push(&outer_node);
        let inner_scope = Scope::new(0, Level::Local, vec!["root"].into(), Type::Unit);
        let inner_node = CompilerNode::ExpressionBlock(inner_scope, vec![]);
        stack.push(&inner_node);

        assert_eq!(stack.find("y").is_none(), true);
    }

    #[test]
    fn test_find_symbol_does_not_pass_function() {
        let mut stack = ScopeStack::new();

        let mut outer_scope = Scope::new(0, Level::Local, vec!["root"].into(), Type::Unit);
        outer_scope.insert("nope", 4, 4);
        let outer_node = CompilerNode::ExpressionBlock(outer_scope, vec![]);
        stack.push(&outer_node);

        let mut fun_scope = Scope::new(
            0,
            Level::Routine {
                next_label: 0,
                allocation: 8,
            },
            vec!["root"].into(),
            Type::Unit,
        );
        fun_scope.insert("y", 4, 4);
        fun_scope.insert("z", 4, 8);
        let outer_node = CompilerNode::RoutineDef {
            meta: fun_scope,
            def: RoutineDef::Function,
            name: "func".into(),
            params: vec![],
            ty: Type::I32,
            body: vec![],
        };
        stack.push(&outer_node);

        let mut inner_scope = Scope::new(0, Level::Local, vec!["root"].into(), Type::Unit);
        inner_scope.insert("x", 4, 4);
        let inner_node = CompilerNode::ExpressionBlock(inner_scope, vec![]);
        stack.push(&inner_node);

        assert_eq!(stack.find("x").is_some(), true);
        assert_eq!(stack.find("y").is_some(), true);
        assert_eq!(stack.find("nope").is_some(), false);
    }

    #[test]
    fn test_get_routine_parameters() {
        let mut stack = ScopeStack::new();

        let mut fun_scope = Scope::new(
            0,
            Level::Routine {
                next_label: 0,
                allocation: 8,
            },
            vec!["root"].into(),
            Type::Unit,
        );
        fun_scope.insert("y", 4, 0);
        fun_scope.insert("z", 4, 4);
        let fun_node = CompilerNode::RoutineDef {
            meta: fun_scope,
            def: RoutineDef::Function,
            name: "func".into(),
            params: vec![("y".into(), Type::I32)],
            ty: Type::I32,
            body: vec![],
        };

        let mut module_scope = Scope::new(0, Level::Local, vec!["root"].into(), Type::Unit);
        module_scope.insert("func", 0, 0);
        let module_node = CompilerNode::Module {
            meta: module_scope,
            name: "test".into(),
            modules: vec![],
            functions: vec![fun_node],
            coroutines: vec![],
            structs: vec![],
        };
        stack.push(&module_node);

        let fun2_scope = Scope::new(
            0,
            Level::Routine {
                next_label: 0,
                allocation: 0,
            },
            vec!["root"].into(),
            Type::Unit,
        );
        let fun2_node = CompilerNode::RoutineDef {
            meta: fun2_scope,
            def: RoutineDef::Function,
            name: "func2".into(),
            params: vec![],
            ty: Type::I32,
            body: vec![],
        };
        stack.push(&fun2_node);

        assert_eq!(stack.find("func").is_none(), true);

        let node = stack.find_func("func").unwrap();
        match node {
            CompilerNode::RoutineDef {
                meta,
                def: RoutineDef::Function,
                name,
                params,
                ..
            } => {
                assert_eq!(name, "func");
                assert_eq!(params.len(), 1);
                let y_param = meta.get("y").unwrap();
                assert_eq!(y_param.offset, 4);
                assert_eq!(y_param.size, 4);
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn test_get_coroutine_parameters() {
        let mut stack = ScopeStack::new();

        let mut cor_scope = Scope::new(
            0,
            Level::Routine {
                next_label: 0,
                allocation: 8,
            },
            vec!["root"].into(),
            Type::Unit,
        );
        cor_scope.insert("y", 4, 20);
        cor_scope.insert("z", 4, 24);
        let cor_node = CompilerNode::RoutineDef {
            meta: cor_scope,
            def: RoutineDef::Coroutine,
            name: "cor".into(),
            params: vec![("y".into(), Type::I32)],
            ty: Type::I32,
            body: vec![],
        };

        let mut module_scope = Scope::new(0, Level::Local, vec!["root"].into(), Type::Unit);
        module_scope.insert("cor", 0, 0);
        let module_node = CompilerNode::Module {
            meta: module_scope,
            name: "test".into(),
            modules: vec![],
            functions: vec![],
            coroutines: vec![cor_node],
            structs: vec![],
        };
        stack.push(&module_node);

        let fun2_scope = Scope::new(
            0,
            Level::Routine {
                next_label: 0,
                allocation: 0,
            },
            vec!["root"].into(),
            Type::Unit,
        );
        let fun2_node = CompilerNode::RoutineDef {
            meta: fun2_scope,
            def: RoutineDef::Coroutine,
            name: "func2".into(),
            params: vec![],
            ty: Type::I32,
            body: vec![],
        };
        stack.push(&fun2_node);

        let node = stack.find_coroutine("cor").unwrap();
        match node {
            CompilerNode::RoutineDef {
                meta,
                def: RoutineDef::Coroutine,
                name,
                params,
                ..
            } => {
                assert_eq!(name, "cor");
                assert_eq!(params.len(), 1);
                let y_param = meta.get("y").unwrap();
                assert_eq!(y_param.offset, 24);
                assert_eq!(y_param.size, 4);
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn test_single_level_to_path() {
        let mut stack = ScopeStack::new();
        let mut module_scope = Scope::new(
            0,
            Level::Module {
                name: "root".into(),
            },
            vec!["root"].into(),
            Type::Unit,
        );
        module_scope.insert("cor", 0, 0);
        let module_node = CompilerNode::Module {
            meta: module_scope,
            name: "root".into(),
            modules: vec![],
            functions: vec![],
            coroutines: vec![],
            structs: vec![],
        };
        stack.push(&module_node);

        let path = stack.to_path().unwrap();
        let expected: Path = vec!["root"].into();
        assert_eq!(path, expected);
    }

    #[test]
    fn test_func_in_module_to_path() {
        let mut stack = ScopeStack::new();
        let mut module_scope = Scope::new(
            0,
            Level::Module {
                name: "root".into(),
            },
            vec!["root"].into(),
            Type::Unit,
        );
        module_scope.insert("cor", 0, 0);
        let module_node = CompilerNode::Module {
            meta: module_scope,
            name: "root".into(),
            modules: vec![],
            functions: vec![],
            coroutines: vec![],
            structs: vec![],
        };
        stack.push(&module_node);

        let fun2_scope = Scope::new(
            0,
            Level::Routine {
                next_label: 0,
                allocation: 0,
            },
            vec!["root"].into(),
            Type::Unit,
        );
        let fun2_node = CompilerNode::RoutineDef {
            meta: fun2_scope,
            def: RoutineDef::Coroutine,
            name: "func2".into(),
            params: vec![],
            ty: Type::I32,
            body: vec![],
        };
        stack.push(&fun2_node);

        let path = stack.to_path().unwrap();
        let expected: Path = vec!["root"].into();
        assert_eq!(path, expected);
    }

    #[test]
    fn test_nested_module_to_path() {
        let mut stack = ScopeStack::new();
        let module_scope = Scope::new(
            0,
            Level::Module {
                name: "root".into(),
            },
            vec!["root"].into(),
            Type::Unit,
        );
        let module_node = CompilerNode::Module {
            meta: module_scope,
            name: "root".into(),
            modules: vec![],
            functions: vec![],
            coroutines: vec![],
            structs: vec![],
        };
        stack.push(&module_node);

        let module2_scope = Scope::new(
            0,
            Level::Module {
                name: "inner".into(),
            },
            vec!["root"].into(),
            Type::Unit,
        );
        let module2_node = CompilerNode::Module {
            meta: module2_scope,
            name: "inner".into(),
            modules: vec![],
            functions: vec![],
            coroutines: vec![],
            structs: vec![],
        };
        stack.push(&module2_node);

        let fun2_scope = Scope::new(
            0,
            Level::Routine {
                next_label: 0,
                allocation: 0,
            },
            vec!["root"].into(),
            Type::Unit,
        );
        let fun2_node = CompilerNode::RoutineDef {
            meta: fun2_scope,
            def: RoutineDef::Coroutine,
            name: "func2".into(),
            params: vec![],
            ty: Type::I32,
            body: vec![],
        };
        stack.push(&fun2_node);

        let path = stack.to_path().unwrap();
        let expected: Path = vec!["root", "inner"].into();
        assert_eq!(path, expected);
    }
}
