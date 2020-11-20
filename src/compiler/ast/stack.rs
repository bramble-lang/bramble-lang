use crate::compiler::ast::ast::CompilerNode;
use crate::compiler::ast::scope::{Symbol, Type};

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
            match scope.ty {
                Type::Block => (),
                Type::Routine { .. } => return None,
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
            Some(ref node) => {
                match node {
                    CompilerNode::Module(_, funcs, _) =>
                        funcs.iter().find(|v| match v {CompilerNode::FunctionDef(_, n, _, _, _) => n == name, _ => false}),
                    _ => None,
                }
            },
            None => None,
        }
    }

    pub fn find_coroutine(&self, name: &str) -> Option<&CompilerNode> {
        match self.find_global(name) {
            Some(ref node) => {
                match node {
                    CompilerNode::Module(_, _, cors) =>
                        cors.iter().find(|v| match v {CompilerNode::CoroutineDef(_, n, _, _, _) => n == name, _ => false}),
                    _ => None,
                }
            },
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::ast::scope::Scope;
    use super::*;

    #[test]
    fn test_find_symbol_in_current_scope() {
        let mut scope = Scope::new(Type::Block);
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

        let mut outer_scope = Scope::new(Type::Block);
        outer_scope.insert("x", 4, 4);
        let outer_node = CompilerNode::ExpressionBlock(outer_scope, vec![]);
        stack.push(&outer_node);
        let inner_scope = Scope::new(Type::Block);
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

        let mut outer_scope = Scope::new(Type::Block);
        outer_scope.insert("x", 4, 4);
        let outer_node = CompilerNode::ExpressionBlock(outer_scope, vec![]);
        stack.push(&outer_node);
        let mut inner_scope = Scope::new(Type::Block);
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

        let mut outer_scope = Scope::new(Type::Block);
        outer_scope.insert("x", 4, 4);
        let outer_node = CompilerNode::ExpressionBlock(outer_scope, vec![]);
        stack.push(&outer_node);
        let inner_scope = Scope::new(Type::Block);
        let inner_node = CompilerNode::ExpressionBlock(inner_scope, vec![]);
        stack.push(&inner_node);

        assert_eq!(stack.find("y").is_none(), true);
    }

    #[test]
    fn test_find_symbol_does_not_pass_function() {
        use crate::syntax::ast::Primitive;

        let mut stack = ScopeStack::new();

        let mut outer_scope = Scope::new(Type::Block);
        outer_scope.insert("nope", 4, 4);
        let outer_node = CompilerNode::ExpressionBlock(outer_scope, vec![]);
        stack.push(&outer_node);

        let mut fun_scope = Scope::new(Type::Routine {
            next_label: 0,
            allocation: 8,
        });
        fun_scope.insert("y", 4, 4);
        fun_scope.insert("z", 4, 8);
        let outer_node = CompilerNode::FunctionDef(fun_scope, "func".into(), vec![], Primitive::I32, vec![]);
        stack.push(&outer_node);

        let mut inner_scope = Scope::new(Type::Block);
        inner_scope.insert("x", 4, 4);
        let inner_node = CompilerNode::ExpressionBlock(inner_scope, vec![]);
        stack.push(&inner_node);

        assert_eq!(stack.find("x").is_some(), true);
        assert_eq!(stack.find("y").is_some(), true);
        assert_eq!(stack.find("nope").is_some(), false);
    }

    #[test]
    fn test_get_routine_parameters() {
        use crate::syntax::ast::Primitive;

        let mut stack = ScopeStack::new();

        let mut fun_scope = Scope::new(Type::Routine {
            next_label: 0,
            allocation: 8,
        });
        fun_scope.insert("y", 4, 0);
        fun_scope.insert("z", 4, 4);
        let fun_node = CompilerNode::FunctionDef(fun_scope, "func".into(), vec![("y".into(), Primitive::I32)], Primitive::I32, vec![]);

        let mut module_scope = Scope::new(Type::Block);
        module_scope.insert("func", 0, 0);
        let module_node = CompilerNode::Module(module_scope, vec![fun_node], vec![]);
        stack.push(&module_node);

        let fun2_scope = Scope::new(Type::Routine {
            next_label: 0,
            allocation: 0,
        });
        let fun2_node = CompilerNode::FunctionDef(fun2_scope, "func2".into(), vec![], Primitive::I32, vec![]);
        stack.push(&fun2_node);
        
        assert_eq!(stack.find("func").is_none(), true);

        let node = stack.find_func("func").unwrap();
        match node {
            CompilerNode::FunctionDef(meta, name, params, _, _) => {
                assert_eq!(name, "func");
                assert_eq!(params.len(), 1);
                let y_param = meta.get("y").unwrap();
                assert_eq!(y_param.offset, 4);
                assert_eq!(y_param.size, 4);
            },
            _ => assert!(false),
        }
    }

    #[test]
    fn test_get_coroutine_parameters() {
        use crate::syntax::ast::Primitive;

        let mut stack = ScopeStack::new();

        let mut cor_scope = Scope::new(Type::Routine {
            next_label: 0,
            allocation: 8,
        });
        cor_scope.insert("y", 4, 20);
        cor_scope.insert("z", 4, 24);
        let cor_node = CompilerNode::CoroutineDef(cor_scope, "cor".into(), vec![("y".into(), Primitive::I32)], Primitive::I32, vec![]);

        let mut module_scope = Scope::new(Type::Block);
        module_scope.insert("cor", 0, 0);
        let module_node = CompilerNode::Module(module_scope, vec![], vec![cor_node]);
        stack.push(&module_node);

        let fun2_scope = Scope::new(Type::Routine {
            next_label: 0,
            allocation: 0,
        });
        let fun2_node = CompilerNode::FunctionDef(fun2_scope, "func2".into(), vec![], Primitive::I32, vec![]);
        stack.push(&fun2_node);

        let node = stack.find_coroutine("cor").unwrap();
        match node {
            CompilerNode::CoroutineDef(meta, name, params, _, _) => {
                assert_eq!(name, "cor");
                assert_eq!(params.len(), 1);
                let y_param = meta.get("y").unwrap();
                assert_eq!(y_param.offset, 24);
                assert_eq!(y_param.size, 4);
            },
            _ => assert!(false),
        }
    }
}