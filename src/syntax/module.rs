use super::{ast::Ast, path::Path};

#[derive(Clone, Debug, PartialEq)]
pub struct Module<M> {
    meta: M,
    name: String,
    modules: Vec<Module<M>>,
    functions: Vec<Ast<M>>,
    coroutines: Vec<Ast<M>>,
    structs: Vec<Ast<M>>,
}

impl<M> Module<M> {
    pub fn new(name: &str, metadata: M) -> Module<M> {
        Module {
            meta: metadata,
            name: name.into(),
            modules: Vec::new(),
            functions: Vec::new(),
            coroutines: Vec::new(),
            structs: Vec::new(),
        }
    }

    pub fn add_module(&mut self, m: Module<M>) {
        self.modules.push(m);
    }

    pub fn add_function(&mut self, f: Ast<M>) -> Result<(), String> {
        let fname = f.get_name().expect("Function must have a name");
        if self.get_item(fname).is_none() {
            self.functions.push(f);
            Ok(())
        } else {
            Err(format!("{} already exists in module", fname))
        }
    }

    pub fn add_coroutine(&mut self, c: Ast<M>) -> Result<(), String> {
        let cname = c.get_name().expect("Coroutine must have a name");
        if self.get_item(cname).is_none() {
            self.coroutines.push(c);
            Ok(())
        } else {
            Err(format!("{} already exists in module", cname))
        }
    }

    pub fn add_struct(&mut self, s: Ast<M>) -> Result<(), String> {
        let name = s.get_name().expect("Struct must have a name");
        if self.get_item(name).is_none() {
            self.structs.push(s);
            Ok(())
        } else {
            Err(format!("{} already exists in module", name))
        }
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_metadata(&self) -> &M {
        &self.meta
    }

    pub fn get_metadata_mut(&mut self) -> &mut M {
        &mut self.meta
    }

    pub fn get_modules(&self) -> &Vec<Module<M>> {
        &self.modules
    }

    pub fn get_modules_mut(&mut self) -> &mut Vec<Module<M>> {
        &mut self.modules
    }

    pub fn get_functions(&self) -> &Vec<Ast<M>> {
        &self.functions
    }

    pub fn get_functions_mut(&mut self) -> &mut Vec<Ast<M>> {
        &mut self.functions
    }

    pub fn get_coroutines(&self) -> &Vec<Ast<M>> {
        &self.coroutines
    }

    pub fn get_coroutines_mut(&mut self) -> &mut Vec<Ast<M>> {
        &mut self.coroutines
    }

    pub fn get_structs(&self) -> &Vec<Ast<M>> {
        &self.structs
    }

    pub fn get_structs_mut(&mut self) -> &mut Vec<Ast<M>> {
        &mut self.structs
    }

    pub fn get_module(&self, name: &str) -> Option<&Module<M>> {
        self.modules.iter().find(|m| m.name == name)
    }

    pub fn get_item(&self, name: &str) -> Option<&Ast<M>> {
        self.functions
            .iter()
            .find(|f| f.get_name().map_or(false, |n| n == name))
            .or(self
                .coroutines
                .iter()
                .find(|c| c.get_name().map_or(false, |n| n == name))
                .or(self
                    .structs
                    .iter()
                    .find(|c| c.get_name().map_or(false, |n| n == name))))
    }

    pub fn go_to(&self, path: &Path) -> Option<&Ast<M>> {
        // If the path is empty, then return None as it is not possible for
        // anything to match
        if path.len() == 0 {
            None
        }
        // If path has one element, then that is the item name
        // and return the matching item
        else if path.len() == 1 {
            path.item().and_then(|item| self.get_item(item))
        } else {
            // otherwise, get the parent of the path and traverse the
            // module hierarchy by the parent, returning None if at
            // any point no module matches the parent path

            // If the parent path terminates on a module, then get
            // the item from the path (the last element in teh path)
            // and search the terminating module for that item and
            // return the result
            let parent_path = path.parent();
            match self.go_to_module(&parent_path) {
                Some(parent) => {
                    let item = path.item().expect("Path with >1 length has no item");
                    parent.get_item(item)
                }
                None => None
            }
        }
    }

    pub fn go_to_module(&self, path: &Path) -> Option<&Module<M>> {
        if path.len() == 0 {
            None
        } else {
            // check to make sure that the first step in the path
            // is this module, and then use the path to traverse
            // through descendent modules
            if self.name == path[0] {
                let mut current = self;
                for idx in 1..path.len() {
                    match current.get_module(&path[idx]) {
                        Some(m) => current = m,
                        None => return None,
                    }
                }
                Some(current)
            } else {
                None
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::syntax::routinedef::RoutineDef;
    use crate::syntax::ty::Type;

    use super::*;

    #[test]
    pub fn test_new_module() {
        let module = Module::new("test", 1);
        assert_eq!(module.get_name(), "test");
        assert_eq!(*module.get_metadata(), 1);
    }

    #[test]
    pub fn test_get_nonexistant_item() {
        let mut module = Module::new("test", 1);
        let fdef = Ast::RoutineDef {
            meta: 1,
            name: "func".into(),
            def: RoutineDef::Function,
            params: vec![],
            ty: Type::I32,
            body: vec![],
        };
        module.add_function(fdef.clone()).unwrap();
        let f = module.get_item("not_found");
        assert_eq!(f, None);
    }

    #[test]
    pub fn test_add_function() {
        let mut module = Module::new("test", 1);
        let fdef = Ast::RoutineDef {
            meta: 1,
            name: "func".into(),
            def: RoutineDef::Function,
            params: vec![],
            ty: Type::I32,
            body: vec![],
        };
        module.add_function(fdef.clone()).unwrap();
        let f = module.get_item("func");
        assert_eq!(f, Some(&fdef));
    }

    #[test]
    pub fn test_add_function_that_already_exists() {
        let mut module = Module::new("test", 1);
        let fdef = Ast::RoutineDef {
            meta: 1,
            name: "func".into(),
            def: RoutineDef::Function,
            params: vec![],
            ty: Type::I32,
            body: vec![],
        };
        module.add_function(fdef.clone()).unwrap();
        let result = module.add_function(fdef.clone());
        assert_eq!(result, Err("func already exists in module".into()));
    }

    #[test]
    pub fn test_add_coroutine() {
        let mut module = Module::new("test", 1);
        let cdef = Ast::RoutineDef {
            meta: 1,
            name: "cor".into(),
            def: RoutineDef::Coroutine,
            params: vec![],
            ty: Type::I32,
            body: vec![],
        };
        module.add_coroutine(cdef.clone()).unwrap();
        let c = module.get_item("cor").unwrap();
        assert_eq!(c, &cdef);
    }

    #[test]
    pub fn test_add_coroutine_that_already_exists() {
        let mut module = Module::new("test", 1);
        let cdef = Ast::RoutineDef {
            meta: 1,
            name: "cor".into(),
            def: RoutineDef::Coroutine,
            params: vec![],
            ty: Type::I32,
            body: vec![],
        };
        module.add_coroutine(cdef.clone()).unwrap();
        let result = module.add_coroutine(cdef.clone());
        assert_eq!(result, Err("cor already exists in module".into()));
    }

    #[test]
    pub fn test_add_coroutine_with_same_name_as_function() {
        let mut module = Module::new("test", 1);
        let fdef = Ast::RoutineDef {
            meta: 1,
            name: "dupe".into(),
            def: RoutineDef::Function,
            params: vec![],
            ty: Type::I32,
            body: vec![],
        };
        module.add_function(fdef.clone()).unwrap();

        let cdef = Ast::RoutineDef {
            meta: 1,
            name: "dupe".into(),
            def: RoutineDef::Coroutine,
            params: vec![],
            ty: Type::I32,
            body: vec![],
        };
        let result = module.add_coroutine(cdef.clone());
        assert_eq!(result, Err("dupe already exists in module".into()));
    }

    #[test]
    pub fn test_add_function_with_same_name_as_coroutine() {
        let mut module = Module::new("test", 1);
        let cdef = Ast::RoutineDef {
            meta: 1,
            name: "dupe".into(),
            def: RoutineDef::Coroutine,
            params: vec![],
            ty: Type::I32,
            body: vec![],
        };
        module.add_coroutine(cdef.clone()).unwrap();

        let fdef = Ast::RoutineDef {
            meta: 1,
            name: "dupe".into(),
            def: RoutineDef::Function,
            params: vec![],
            ty: Type::I32,
            body: vec![],
        };
        let result = module.add_function(fdef.clone());
        assert_eq!(result, Err("dupe already exists in module".into()));
    }

    #[test]
    pub fn test_go_to_item_does_not_exist() {
        let mut module = Module::new("test", 1);
        let fdef = Ast::RoutineDef {
            meta: 1,
            name: "func".into(),
            def: RoutineDef::Function,
            params: vec![],
            ty: Type::I32,
            body: vec![],
        };
        module.add_function(fdef.clone()).unwrap();
        let f = module.go_to(&vec!["test", "nothing"].into());
        assert_eq!(f, None);
    }

    #[test]
    pub fn test_go_to_root_does_not_match() {
        let mut module = Module::new("test", 1);
        let fdef = Ast::RoutineDef {
            meta: 1,
            name: "func".into(),
            def: RoutineDef::Function,
            params: vec![],
            ty: Type::I32,
            body: vec![],
        };
        module.add_function(fdef.clone()).unwrap();
        let f = module.go_to(&vec!["bad", "func"].into());
        assert_eq!(f, None);
    }

    #[test]
    pub fn test_go_to_function() {
        let mut module = Module::new("test", 1);
        let fdef = Ast::RoutineDef {
            meta: 1,
            name: "func".into(),
            def: RoutineDef::Function,
            params: vec![],
            ty: Type::I32,
            body: vec![],
        };
        module.add_function(fdef.clone()).unwrap();
        let f = module.go_to(&vec!["test", "func"].into());
        assert_eq!(f, Some(&fdef));
    }

    #[test]
    pub fn test_go_to_coroutine() {
        let mut module = Module::new("test", 1);
        let fdef = Ast::RoutineDef {
            meta: 1,
            name: "co".into(),
            def: RoutineDef::Coroutine,
            params: vec![],
            ty: Type::I32,
            body: vec![],
        };
        module.add_coroutine(fdef.clone()).unwrap();
        let f = module.go_to(&vec!["test", "co"].into());
        assert_eq!(f, Some(&fdef));
    }

    #[test]
    pub fn test_go_to_nested() {
        let mut module = Module::new("inner", 1);
        let fdef = Ast::RoutineDef {
            meta: 1,
            name: "co".into(),
            def: RoutineDef::Coroutine,
            params: vec![],
            ty: Type::I32,
            body: vec![],
        };
        module.add_coroutine(fdef.clone()).unwrap();
        let mut outer = Module::new("outer", 2);
        outer.add_module(module.clone());
        let f = outer.go_to(&vec!["outer", "inner", "co"].into());
        assert_eq!(f, Some(&fdef));
    }
}
