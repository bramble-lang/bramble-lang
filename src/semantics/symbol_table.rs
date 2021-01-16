use crate::syntax::{module::{Item, Module}, routinedef::{RoutineDef, RoutineDefType}, ty::Type};
use crate::semantics::semanticnode::SemanticMetadata;
use crate::{ast, syntax::path::Path};

#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub ty: Type,
    pub mutable: bool,
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{} | {} | {}",
            self.name, self.ty, self.mutable
        ))
    }
}

#[derive(Clone, Debug, PartialEq)]
enum ScopeType {
    Local,
    Routine,
    Module { name: String },
}

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
    ty: ScopeType,
    sym: Vec<Symbol>,
}

impl std::fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("\tName | Type | Mutable\n")?;
        for symbol in self.sym.iter() {
            f.write_fmt(format_args!("\t{}\n", symbol))?;
        }
        Ok(())
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            ty: ScopeType::Local,
            sym: vec![],
        }
    }

    pub fn new_module(name: &str) -> Self {
        SymbolTable {
            ty: ScopeType::Module { name: name.into() },
            sym: vec![],
        }
    }

    pub fn table(&self) -> &Vec<Symbol> {
        &self.sym
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        //self.sym.get(name)
        self.sym.iter().find(|s| s.name == name)
    }

    pub fn get_path(&self, name: &Path) -> Option<&Symbol> {
        if name.len() == 1 {
            self.sym.iter().find(|s| s.name == name[0])
        } else {
            None
        }
    }

    pub fn add(&mut self, name: &str, ty: Type, mutable: bool) -> Result<(), String> {
        if self.get(name).is_some() {
            Err(format!("{} already declared", name))
        } else {
            self.sym.push(Symbol {
                name: name.into(),
                ty,
                mutable,
            });
            Ok(())
        }
    }

    pub fn from_module(module: &mut Module<SemanticMetadata>) -> Result<(), String> {
        Self::generate(module)
    }

    pub fn generate(m: &mut Module<SemanticMetadata>) -> Result<(), String> {
        let mut metadata = m.get_metadata().clone();
        
        let fm = m.get_functions_mut();
        for f in fm.iter_mut() {
            SymbolTable::traverse(f, &mut metadata)?;
        }

        let cm = m.get_coroutines_mut();
        for co in cm.iter_mut() {
            SymbolTable::traverse(co, &mut metadata)?;
        }
        for st in m.get_structs_mut().iter_mut() {
            SymbolTable::traverse(st, &mut metadata)?;
        }

        for m in m.get_modules_mut().iter_mut() {
            SymbolTable::generate(m)?;
        }

        *m.get_metadata_mut() = metadata;

        Ok(())
    }

    fn traverse(item: &mut Item<SemanticMetadata>, sym: &mut SemanticMetadata) -> Result<(), String> {
        use ast::Ast;
        match item {
            Item::Routine(RoutineDef {
                def: RoutineDefType::Function,
                name,
                params,
                ty,
                ..
            }) => {
                sym.sym.add(
                    name,
                    Type::FunctionDef(
                        params
                            .iter()
                            .map(|(_, ty)| ty.clone())
                            .collect::<Vec<Type>>(),
                        Box::new(ty.clone()),
                    ),
                    false,
                )?;
            }
            Item::Routine(RoutineDef {
                def: RoutineDefType::Coroutine,
                name,
                params,
                ty,
                ..
            }) => {
                sym.sym.add(
                    name,
                    Type::CoroutineDef(
                        params
                            .iter()
                            .map(|(_, ty)| ty.clone())
                            .collect::<Vec<Type>>(),
                        Box::new(ty.clone()),
                    ),
                    false,
                )?;
            }
            Item::Struct(Ast::StructDef(_, name, members)) => {
                sym.sym.add(name, Type::StructDef(members.clone()), false)?;
            }
            _ => panic!(
                "Type analysis: expected function or coroutine in module, found {}",
                item.root_str()
            ),
        }

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ScopeStack {
    stack: Vec<SymbolTable>,
}

impl std::fmt::Display for ScopeStack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut i = 0;
        for scope in self.stack.iter() {
            f.write_fmt(format_args!("{}: {}\n", i, scope))?;
            i += 1;
        }
        Ok(())
    }
}

impl ScopeStack {
    pub fn new() -> ScopeStack {
        ScopeStack { stack: vec![] }
    }

    pub fn push(&mut self, sym: SymbolTable) {
        self.stack.push(sym);
    }

    pub fn pop(&mut self) {
        self.stack.pop();
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

    /// Starting from the bottom of the stack this builds a path
    /// of all the modules that we are current in, in effect
    /// the current path within the AST.
    pub fn to_path(&self, current: &SymbolTable) -> Option<Path> {
        let mut steps = vec![];

        for node in self.stack.iter() {
            match &node.ty {
                ScopeType::Module { name } => {
                    steps.push(name.clone());
                }
                _ => (),
            }
        }

        match current {
            SymbolTable {
                ty: ScopeType::Module { name },
                ..
            } => steps.push(name.clone()),
            _ => (),
        }

        if steps.len() > 0 {
            Some(steps.into())
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_stack_to_path() {
        let stack = ScopeStack::new();
        let local = SymbolTable::new();
        let path = stack.to_path(&local);
        assert_eq!(path, None);
    }

    #[test]
    fn test_one_module_stack_to_path() {
        let mut stack = ScopeStack::new();
        let sym = SymbolTable::new_module("root");
        stack.push(sym);
        let local = SymbolTable::new();
        let path = stack.to_path(&local).unwrap();
        let expected = vec!["root"].into();
        assert_eq!(path, expected);
    }

    #[test]
    fn test_one_module_stack_module_current_to_path() {
        let mut stack = ScopeStack::new();
        let sym = SymbolTable::new_module("root");
        stack.push(sym);
        let current = SymbolTable::new_module("inner");
        let path = stack.to_path(&current).unwrap();
        let expected = vec!["root", "inner"].into();
        assert_eq!(path, expected);
    }

    #[test]
    fn test_local_then_one_module_stack_to_path() {
        let mut stack = ScopeStack::new();
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
        let mut stack = ScopeStack::new();
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
