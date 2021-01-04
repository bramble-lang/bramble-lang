use crate::ast;
use crate::ast::Type;
use crate::semantics::semanticnode::SemanticMetadata;
use crate::semantics::semanticnode::SemanticNode;

#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub ty: Type,
    pub mutable: bool,
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{} | {} | {}", self.name, self.ty, self.mutable))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
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
        SymbolTable { sym: vec![] }
    }

    pub fn table(&self) -> &Vec<Symbol> {
        &self.sym
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        //self.sym.get(name)
        self.sym.iter().find(|s| s.name == name)
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

    pub fn generate(ast: &mut SemanticNode) -> Result<(), String> {
        use ast::Ast;
        match ast {
            Ast::Module {
                meta,
                functions,
                coroutines,
                structs,
                ..
            } => {
                for f in functions.iter_mut() {
                    SymbolTable::traverse(f, meta)?;
                }
                for co in coroutines.iter_mut() {
                    SymbolTable::traverse(co, meta)?;
                }
                for st in structs.iter_mut() {
                    SymbolTable::traverse(st, meta)?;
                }
            }
            _ => panic!("Type analysis: expected Module at root level of the AST"),
        }

        Ok(())
    }

    fn traverse(ast: &mut SemanticNode, sym: &mut SemanticMetadata) -> Result<(), String> {
        use ast::Ast;
        match &ast {
            Ast::RoutineDef(_, ast::RoutineDef::Function, name, params, ty, _) => {
                sym.sym.add(
                    name,
                    Type::FunctionDef(
                        params
                            .iter()
                            .map(|(_, ty)| ty.clone())
                            .collect::<Vec<ast::Type>>(),
                        Box::new(ty.clone()),
                    ),
                    false,
                )?;
            }
            Ast::RoutineDef(_, ast::RoutineDef::Coroutine, name, params, ty, _) => {
                sym.sym.add(
                    name,
                    Type::CoroutineDef(
                        params
                            .iter()
                            .map(|(_, ty)| ty.clone())
                            .collect::<Vec<ast::Type>>(),
                        Box::new(ty.clone()),
                    ),
                    false,
                )?;
            }
            Ast::StructDef(_, name, members) => {
                sym.sym.add(name, Type::StructDef(members.clone()), false)?;
            }
            _ => panic!(
                "Type analysis: expected function or coroutine in module, found {}",
                ast.root_str()
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
}
