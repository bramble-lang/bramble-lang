use super::{
    extern_decl::Extern,
    node::{
        Context, Node, NodeType, {PostOrderIter, PreOrderIter},
    },
    path::{Element, Path},
    routinedef::{RoutineDef, RoutineDefType},
    structdef::StructDef,
    AstError,
};
use crate::compiler::CompilerError;
use crate::StringId;

type AstResult<T> = Result<T, CompilerError<AstError>>;

#[derive(Clone, Debug, PartialEq)]
pub struct Module<M> {
    context: M,
    name: StringId,
    modules: Vec<Module<M>>,
    functions: Vec<Item<M>>,
    coroutines: Vec<Item<M>>,
    structs: Vec<Item<M>>,
    externs: Vec<Item<M>>,
}

impl<M: Context> Node<M> for Module<M> {
    fn get_context(&self) -> &M {
        &self.context
    }

    fn get_context_mut(&mut self) -> &mut M {
        &mut self.context
    }

    fn node_type(&self) -> NodeType {
        NodeType::Module
    }

    fn children(&self) -> Vec<&dyn Node<M>> {
        let mut v: Vec<&dyn Node<M>> = vec![];
        for m in self.modules.iter() {
            v.push(m);
        }
        for f in self.functions.iter() {
            v.push(f);
        }
        for c in self.coroutines.iter() {
            v.push(c);
        }
        for s in self.structs.iter() {
            v.push(s);
        }
        v
    }

    fn name(&self) -> Option<StringId> {
        Some(self.name)
    }

    fn iter_postorder(&self) -> PostOrderIter<M> {
        PostOrderIter::new(self)
    }

    fn iter_preorder(&self) -> PreOrderIter<M> {
        PreOrderIter::new(self)
    }
}

impl<M> std::fmt::Display for Module<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_fmt(format_args!("{}", self.get_name()))
    }
}

impl<M> Module<M> {
    pub fn new(name: StringId, context: M) -> Module<M> {
        Module {
            context,
            name: name,
            modules: Vec::new(),
            functions: Vec::new(),
            coroutines: Vec::new(),
            structs: Vec::new(),
            externs: Vec::new(),
        }
    }

    pub fn add_module(&mut self, m: Module<M>) {
        self.modules.push(m);
    }

    pub fn add_function(&mut self, f: RoutineDef<M>) -> AstResult<()> {
        let fname = f.get_name();
        if self.get_item(fname).is_none() {
            self.functions.push(Item::Routine(f));
            Ok(())
        } else {
            err!(0, AstError::ModuleAlreadyContains(fname))
        }
    }

    pub fn add_coroutine(&mut self, c: RoutineDef<M>) -> AstResult<()> {
        let cname = c.get_name();
        if self.get_item(cname).is_none() {
            self.coroutines.push(Item::Routine(c));
            Ok(())
        } else {
            err!(0, AstError::ModuleAlreadyContains(cname))
        }
    }

    pub fn add_struct(&mut self, s: StructDef<M>) -> AstResult<()> {
        let name = s.get_name();
        if self.get_item(name).is_none() {
            self.structs.push(Item::Struct(s));
            Ok(())
        } else {
            err!(0, AstError::ModuleAlreadyContains(name))
        }
    }

    pub fn add_extern(&mut self, e: Extern<M>) -> AstResult<()> {
        let name = e.get_name();
        if self.get_item(name).is_none() {
            self.externs.push(Item::Extern(e));
            Ok(())
        } else {
            err!(0, AstError::ModuleAlreadyContains(name))
        }
    }

    pub fn add_item(&mut self, i: Item<M>) -> AstResult<()> {
        match i {
            Item::Routine(r) => {
                if *r.get_def() == RoutineDefType::Function {
                    self.add_function(r)
                } else {
                    self.add_coroutine(r)
                }
            }
            Item::Struct(s) => self.add_struct(s),
            Item::Extern(e) => self.add_extern(e),
        }
    }

    pub fn get_name(&self) -> StringId {
        self.name
    }

    pub fn get_modules(&self) -> &Vec<Module<M>> {
        &self.modules
    }

    pub fn get_modules_mut(&mut self) -> &mut Vec<Module<M>> {
        &mut self.modules
    }

    pub fn get_functions(&self) -> &Vec<Item<M>> {
        &self.functions
    }

    pub fn get_functions_mut(&mut self) -> &mut Vec<Item<M>> {
        &mut self.functions
    }

    /// Gets all the functions in this module and its submodules
    pub fn deep_get_functions(&self) -> Vec<&Item<M>> {
        let mut funcs = vec![];

        // Add all my functions to the vector
        for f in self.get_functions() {
            funcs.push(f);
        }

        // Get all the functions from my submodules and add them to the vector
        for m in self.get_modules() {
            let mut subfuncs = m.deep_get_functions();
            funcs.append(&mut subfuncs);
        }

        funcs
    }

    pub fn get_coroutines(&self) -> &Vec<Item<M>> {
        &self.coroutines
    }

    pub fn get_coroutines_mut(&mut self) -> &mut Vec<Item<M>> {
        &mut self.coroutines
    }

    pub fn get_structs(&self) -> &Vec<Item<M>> {
        &self.structs
    }

    pub fn get_structs_mut(&mut self) -> &mut Vec<Item<M>> {
        &mut self.structs
    }

    /// Gets all the functions in this module and its submodules
    pub fn deep_get_structs(&self) -> Vec<&StructDef<M>> {
        let mut structs = vec![];

        // Add all my structures to the vector
        for s in self.get_structs() {
            match s {
                Item::Struct(sd) => structs.push(sd),
                _ => panic!("Non StructDef returned by get_structs"),
            }
        }

        // Get all the structures from my submodules and add them to the vector
        for m in self.get_modules() {
            let mut substructs = m.deep_get_structs();
            structs.append(&mut substructs);
        }

        structs
    }

    pub fn get_externs(&self) -> &Vec<Item<M>> {
        &self.externs
    }

    pub fn get_externs_mut(&mut self) -> &mut Vec<Item<M>> {
        &mut self.externs
    }

    pub fn get_module(&self, name: StringId) -> Option<&Module<M>> {
        self.modules.iter().find(|m| m.name == name)
    }

    pub fn get_module_mut(&mut self, name: StringId) -> Option<&mut Module<M>> {
        self.modules.iter_mut().find(|m| m.name == name)
    }

    pub fn get_item(&self, name: StringId) -> Option<&Item<M>> {
        self.functions.iter().find(|f| f.get_name() == name).or(self
            .coroutines
            .iter()
            .find(|c| c.get_name() == name)
            .or(self
                .structs
                .iter()
                .find(|c| c.get_name() == name)
                .or(self.externs.iter().find(|e| e.get_name() == name))))
    }

    pub fn go_to_module(&self, path: &Path) -> Option<&Module<M>> {
        if path.len() == 0 {
            None
        } else {
            // check to make sure that the first step in the path
            // is this module, and then use the path to traverse
            // through descendent modules
            if Element::Id(self.name) == path[0] {
                let mut current = self;
                for idx in 1..path.len() {
                    if let Element::Id(el) = path[idx] {
                        match current.get_module(el) {
                            Some(m) => current = m,
                            None => return None,
                        }
                    } else {
                        return None;
                    }
                }
                Some(current)
            } else {
                None
            }
        }
    }
}

/*
#[cfg(test)]
mod test {
    use crate::compiler::ast::routinedef::{RoutineDef, RoutineDefType};
    use crate::compiler::ast::ty::Type;

    use super::*;

    #[test]
    pub fn test_new_module() {
        let module = Module::new("test", 1);
        assert_eq!(module.get_name(), "test");
        assert_eq!(*module.get_context(), 1);
    }

    #[test]
    pub fn test_get_nonexistant_item() {
        let mut module = Module::new("test", 1);
        let fdef = RoutineDef {
            context: 1,
            name: "func".into(),
            def: RoutineDefType::Function,
            params: vec![],
            ret_ty: Type::I64,
            body: vec![],
        };
        module.add_function(fdef.clone()).unwrap();
        let f = module.get_item("not_found");
        assert_eq!(f, None);
    }

    #[test]
    pub fn test_add_function() {
        let mut module = Module::new("test", 1);
        let fdef = RoutineDef {
            context: 1,
            name: "func".into(),
            def: RoutineDefType::Function,
            params: vec![],
            ret_ty: Type::I64,
            body: vec![],
        };
        module.add_function(fdef.clone()).unwrap();
        let f = module.get_item("func");
        assert_eq!(f, Some(&Item::Routine(fdef)));
    }

    #[test]
    pub fn test_add_function_that_already_exists() {
        let mut module = Module::new("test", 1);
        let fdef = RoutineDef {
            context: 1,
            name: "func".into(),
            def: RoutineDefType::Function,
            params: vec![],
            ret_ty: Type::I64,
            body: vec![],
        };
        module.add_function(fdef.clone()).unwrap();
        let result = module.add_function(fdef.clone());
        assert_eq!(result, Err("func already exists in module".into()));
    }

    #[test]
    pub fn test_add_coroutine() {
        let mut module = Module::new("test", 1);
        let cdef = RoutineDef {
            context: 1,
            name: "cor".into(),
            def: RoutineDefType::Coroutine,
            params: vec![],
            ret_ty: Type::I64,
            body: vec![],
        };
        module.add_coroutine(cdef.clone()).unwrap();
        let c = module.get_item("cor").unwrap();
        assert_eq!(c, &Item::Routine(cdef));
    }

    #[test]
    pub fn test_add_coroutine_that_already_exists() {
        let mut module = Module::new("test", 1);
        let cdef = RoutineDef {
            context: 1,
            name: "cor".into(),
            def: RoutineDefType::Coroutine,
            params: vec![],
            ret_ty: Type::I64,
            body: vec![],
        };
        module.add_coroutine(cdef.clone()).unwrap();
        let result = module.add_coroutine(cdef.clone());
        assert_eq!(result, Err("cor already exists in module".into()));
    }

    #[test]
    pub fn test_add_coroutine_with_same_name_as_function() {
        let mut module = Module::new("test", 1);
        let fdef = RoutineDef {
            context: 1,
            name: "dupe".into(),
            def: RoutineDefType::Function,
            params: vec![],
            ret_ty: Type::I64,
            body: vec![],
        };
        module.add_function(fdef.clone()).unwrap();

        let cdef = RoutineDef {
            context: 1,
            name: "dupe".into(),
            def: RoutineDefType::Coroutine,
            params: vec![],
            ret_ty: Type::I64,
            body: vec![],
        };
        let result = module.add_coroutine(cdef.clone());
        assert_eq!(result, Err("dupe already exists in module".into()));
    }

    #[test]
    pub fn test_add_function_with_same_name_as_coroutine() {
        let mut module = Module::new("test", 1);
        let cdef = RoutineDef {
            context: 1,
            name: "dupe".into(),
            def: RoutineDefType::Coroutine,
            params: vec![],
            ret_ty: Type::I64,
            body: vec![],
        };
        module.add_coroutine(cdef.clone()).unwrap();

        let fdef = RoutineDef {
            context: 1,
            name: "dupe".into(),
            def: RoutineDefType::Function,
            params: vec![],
            ret_ty: Type::I64,
            body: vec![],
        };
        let result = module.add_function(fdef.clone());
        assert_eq!(result, Err("dupe already exists in module".into()));
    }

    #[test]
    pub fn test_get_item_that_does_not_exist() {
        let mut module = Module::new("test", 1);
        let fdef = RoutineDef {
            context: 1,
            name: "func".into(),
            def: RoutineDefType::Function,
            params: vec![],
            ret_ty: Type::I64,
            body: vec![],
        };
        module.add_function(fdef.clone()).unwrap();
        let f = module.get_item("nothing");
        assert_eq!(f, None);
    }

    #[test]
    pub fn test_get_function() {
        let mut module = Module::new("test", 1);
        let fdef = RoutineDef {
            context: 1,
            name: "func".into(),
            def: RoutineDefType::Function,
            params: vec![],
            ret_ty: Type::I64,
            body: vec![],
        };
        module.add_function(fdef.clone()).unwrap();
        let f = module.get_item("func");
        assert_eq!(f, Some(&Item::Routine(fdef)));
    }

    #[test]
    pub fn test_get_coroutine() {
        let mut module = Module::new("test", 1);
        let fdef = RoutineDef {
            context: 1,
            name: "co".into(),
            def: RoutineDefType::Coroutine,
            params: vec![],
            ret_ty: Type::I64,
            body: vec![],
        };
        module.add_coroutine(fdef.clone()).unwrap();
        let f = module.get_item("co");
        assert_eq!(f, Some(&Item::Routine(fdef)));
    }

    #[test]
    pub fn test_go_to_nested() {
        let mut inner = Module::new("inner", 1);
        let fdef = RoutineDef {
            context: 1,
            name: "co".into(),
            def: RoutineDefType::Coroutine,
            params: vec![],
            ret_ty: Type::I64,
            body: vec![],
        };
        inner.add_coroutine(fdef.clone()).unwrap();
        let mut outer = Module::new("outer", 2);
        outer.add_module(inner.clone());
        let m = outer.go_to_module(&vec!["outer", "inner"].into()).unwrap();
        let f = m.get_item("co");
        assert_eq!(f, Some(&Item::Routine(fdef)));
    }

    #[test]
    pub fn test_add_extern() {
        let mut module = Module::new("test", 1);
        let edef = Extern::new("puts", 1, vec![], false, Type::Unit);
        module.add_extern(edef.clone()).unwrap();
        let c = module.get_item("puts").unwrap();
        assert_eq!(c, &Item::Extern(edef));
    }

    #[test]
    pub fn test_add_extern_that_already_exists() {
        let mut module = Module::new("test", 1);
        let edef = Extern::new("puts", 1, vec![], false, Type::Unit);
        module.add_extern(edef.clone()).unwrap();
        let result = module.add_extern(edef.clone());
        assert_eq!(result, Err("puts already exists in module".into()));
    }

    #[test]
    pub fn test_add_extern_with_same_name_as_function() {
        let mut module = Module::new("test", 1);
        let fdef = RoutineDef {
            context: 1,
            name: "dupe".into(),
            def: RoutineDefType::Function,
            params: vec![],
            ret_ty: Type::I64,
            body: vec![],
        };
        module.add_function(fdef.clone()).unwrap();

        let edef = Extern::new("dupe", 1, vec![], false, Type::Unit);
        let result = module.add_extern(edef.clone());
        assert_eq!(result, Err("dupe already exists in module".into()));
    }

    #[test]
    pub fn test_add_function_with_same_name_as_extern() {
        let mut module = Module::new("test", 1);
        let edef = Extern::new("dupe", 1, vec![], false, Type::Unit);
        module.add_extern(edef.clone()).unwrap();

        let fdef = RoutineDef {
            context: 1,
            name: "dupe".into(),
            def: RoutineDefType::Function,
            params: vec![],
            ret_ty: Type::I64,
            body: vec![],
        };
        let result = module.add_function(fdef.clone());
        assert_eq!(result, Err("dupe already exists in module".into()));
    }
}*/

#[derive(Clone, Debug, PartialEq)]
pub enum Item<M> {
    Routine(RoutineDef<M>),
    Struct(StructDef<M>),
    Extern(Extern<M>),
}

impl<M: Context> Node<M> for Item<M> {
    fn get_context(&self) -> &M {
        match self {
            Item::Routine(r) => r.get_context(),
            Item::Struct(s) => s.get_context(),
            Item::Extern(e) => e.get_context(),
        }
    }

    fn get_context_mut(&mut self) -> &mut M {
        match self {
            Item::Routine(r) => r.get_context_mut(),
            Item::Struct(s) => s.get_context_mut(),
            Item::Extern(e) => e.get_context_mut(),
        }
    }

    fn node_type(&self) -> NodeType {
        match self {
            Item::Routine(r) => r.node_type(),
            Item::Struct(s) => s.node_type(),
            Item::Extern(e) => e.node_type(),
        }
    }

    fn children(&self) -> Vec<&dyn Node<M>> {
        match self {
            Item::Routine(r) => r.children(),
            Item::Struct(s) => s.children(),
            Item::Extern(e) => e.children(),
        }
    }

    fn name(&self) -> Option<StringId> {
        match self {
            Item::Routine(r) => r.name(),
            Item::Struct(s) => s.name(),
            Item::Extern(e) => e.name(),
        }
    }

    fn iter_postorder(&self) -> PostOrderIter<M> {
        PostOrderIter::new(self)
    }

    fn iter_preorder(&self) -> PreOrderIter<M> {
        PreOrderIter::new(self)
    }
}

impl<M> std::fmt::Display for Item<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_fmt(format_args!("{}", self.get_name()))
    }
}

impl<M> Item<M> {
    pub fn get_name(&self) -> StringId {
        match self {
            Item::Routine(r) => r.get_name(),
            Item::Struct(s) => s.get_name(),
            Item::Extern(e) => e.get_name(),
        }
    }

    pub fn to_routine(&self) -> Option<&RoutineDef<M>> {
        match self {
            Item::Routine(r) => Some(r),
            Item::Struct(_) => None,
            Item::Extern(_) => None,
        }
    }

    pub fn root_str(&self) -> String {
        match self {
            Item::Routine(r) => format!("{} {}", r.get_def(), r.get_name()),
            Item::Struct(s) => s.root_str(),
            Item::Extern(e) => e.root_str(),
        }
    }
}
