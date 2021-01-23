use crate::compiler::ast::scope::Level;
use crate::syntax::routinedef::RoutineDefType;

use super::{scope::Scope, symbol_table::Symbol};

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
            match scope.level {
                Level::Local => (),
                Level::Routine { .. } | Level::Module { .. } => return None,
            }
        }

        None
    }

    pub fn in_coroutine(&self) -> bool {
        for scope in self.stack.iter().rev() {
            match scope.level {
                Level::Routine { routine_type, .. } => {
                    return routine_type == RoutineDefType::Coroutine
                }
                _ => (),
            }
        }

        false
    }
}

impl std::fmt::Display for ScopeStack<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for scope in self.stack.iter() {
            f.write_fmt(format_args!("{}\n", scope))?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::ty::Type;
    use crate::{
        compiler::ast::{ast::CompilerNode, scope::Scope},
        syntax::{module, routinedef::RoutineDef},
    };

    #[test]
    fn test_find_symbol_in_current_scope() {
        let mut scope = Scope::new(0, Level::Local, vec!["root"].into(), Type::Unit);
        scope.insert("x", 4, 4);
        let node = CompilerNode::ExpressionBlock(scope, vec![], None);
        let mut stack = ScopeStack::new();
        stack.push(&node.get_annotations());

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
        let outer_node = CompilerNode::ExpressionBlock(outer_scope, vec![], None);
        stack.push(&outer_node.get_annotations());
        let inner_scope = Scope::new(0, Level::Local, vec!["root"].into(), Type::Unit);
        let inner_node = CompilerNode::ExpressionBlock(inner_scope, vec![], None);
        stack.push(&inner_node.get_annotations());

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
        let outer_node = CompilerNode::ExpressionBlock(outer_scope, vec![], None);
        stack.push(&outer_node.get_annotations());
        let mut inner_scope = Scope::new(0, Level::Local, vec!["root"].into(), Type::Unit);
        inner_scope.insert("x", 4, 16);
        let inner_node = CompilerNode::ExpressionBlock(inner_scope, vec![], None);
        stack.push(&inner_node.get_annotations());

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
        let outer_node = CompilerNode::ExpressionBlock(outer_scope, vec![], None);
        stack.push(&outer_node.get_annotations());
        let inner_scope = Scope::new(0, Level::Local, vec!["root"].into(), Type::Unit);
        let inner_node = CompilerNode::ExpressionBlock(inner_scope, vec![], None);
        stack.push(&inner_node.get_annotations());

        assert_eq!(stack.find("y").is_none(), true);
    }

    #[test]
    fn test_find_symbol_does_not_pass_function() {
        let mut stack = ScopeStack::new();

        let mut outer_scope = Scope::new(0, Level::Local, vec!["root"].into(), Type::Unit);
        outer_scope.insert("nope", 4, 4);
        let outer_node = CompilerNode::ExpressionBlock(outer_scope, vec![], None);
        stack.push(&outer_node.get_annotations());

        let mut fun_scope = Scope::new(
            0,
            Level::Routine {
                next_label: 0,
                allocation: 8,
                routine_type: RoutineDefType::Function,
            },
            vec!["root"].into(),
            Type::Unit,
        );
        fun_scope.insert("y", 4, 4);
        fun_scope.insert("z", 4, 8);
        let outer_node = RoutineDef {
            annotations: fun_scope,
            def: RoutineDefType::Function,
            name: "func".into(),
            params: vec![],
            ty: Type::I32,
            body: vec![],
        };
        stack.push(&outer_node.get_annotations());

        let mut inner_scope = Scope::new(0, Level::Local, vec!["root"].into(), Type::Unit);
        inner_scope.insert("x", 4, 4);
        let inner_node = CompilerNode::ExpressionBlock(inner_scope, vec![], None);
        stack.push(&inner_node.get_annotations());

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
                routine_type: RoutineDefType::Function,
            },
            vec!["root"].into(),
            Type::Unit,
        );
        fun_scope.insert("y", 4, 0);
        fun_scope.insert("z", 4, 4);
        let fun_node = RoutineDef {
            annotations: fun_scope,
            def: RoutineDefType::Function,
            name: "func".into(),
            params: vec![("y".into(), Type::I32)],
            ty: Type::I32,
            body: vec![],
        };

        let mut module_scope = Scope::new(0, Level::Local, vec!["root"].into(), Type::Unit);
        module_scope.insert("func", 0, 0);
        let mut module_node = module::Module::new("test", module_scope);
        module_node.add_function(fun_node).unwrap();
        stack.push(&module_node.get_annotations());

        let fun2_scope = Scope::new(
            0,
            Level::Routine {
                next_label: 0,
                allocation: 0,
                routine_type: RoutineDefType::Function,
            },
            vec!["root"].into(),
            Type::Unit,
        );
        let fun2_node = RoutineDef {
            annotations: fun2_scope,
            def: RoutineDefType::Function,
            name: "func2".into(),
            params: vec![],
            ty: Type::I32,
            body: vec![],
        };
        stack.push(&fun2_node.get_annotations());

        assert_eq!(stack.find("func").is_none(), true);
    }

    #[test]
    fn test_get_coroutine_parameters() {
        let mut stack = ScopeStack::new();

        let mut cor_scope = Scope::new(
            0,
            Level::Routine {
                next_label: 0,
                allocation: 8,
                routine_type: RoutineDefType::Coroutine,
            },
            vec!["root"].into(),
            Type::Unit,
        );
        cor_scope.insert("y", 4, 20);
        cor_scope.insert("z", 4, 24);
        let cor_node = RoutineDef {
            annotations: cor_scope,
            def: RoutineDefType::Coroutine,
            name: "cor".into(),
            params: vec![("y".into(), Type::I32)],
            ty: Type::I32,
            body: vec![],
        };

        let mut module_scope = Scope::new(0, Level::Local, vec!["root"].into(), Type::Unit);
        module_scope.insert("cor", 0, 0);

        let mut module_node = module::Module::new("test", module_scope);
        module_node.add_coroutine(cor_node).unwrap();
        stack.push(&module_node.get_annotations());

        let fun2_scope = Scope::new(
            0,
            Level::Routine {
                next_label: 0,
                allocation: 0,
                routine_type: RoutineDefType::Coroutine,
            },
            vec!["root"].into(),
            Type::Unit,
        );
        let fun2_node = RoutineDef {
            annotations: fun2_scope,
            def: RoutineDefType::Coroutine,
            name: "func2".into(),
            params: vec![],
            ty: Type::I32,
            body: vec![],
        };
        stack.push(&fun2_node.get_annotations());

        assert!(stack.in_coroutine());
    }
}
