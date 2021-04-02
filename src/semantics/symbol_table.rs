use crate::ast::*;
use crate::semantics::semanticnode::SemanticAnnotations;
use braid_lang::result::Result;

/**
 * `SymbolTable` is an AST node annotation that contains information about symbols that
 * are defined by immediate children of this node. For example:
 *
 *```
 * mod my_mod {
 *      fn hello(i: i64) {
 *          let x: i64 := i * i;
 *          // do something
 *      }
 * }
 *```
 *
 * The node representing `my_mod` would have a child node representing the definition of
 * `hello`; the `SymbolTable` annotation on `my_mod`'s node would contain information for
 * the symbol `hello`, but it would _not_ contain the symbol for `x` which is whithin the
 * body of `hello`.
 *
 * The `SymbolTable` combined with the hierarchical structure of the AST provides the
 * solution for symbol scopes.  At any given node, the only symbols that node can possibly
 * know about are the ones in the `SymbolTable`s of the nodes that comprise the path from
 * the root of the AST to the given node.
 */
#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
    ty: ScopeType,
    sym: Vec<Symbol>,
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

    /**
     * Traverses the items in a module (function, coroutine, structure, and module definitions)
     * and adds their definition information to the symbol table of the module.  The result
     * is that each Module node's symbol table will include all the routines (functions and
     * coroutines) and structures defined in that module and likewise for child modules.compiler
     *
     * This function is recursively applied to child modules.
     */
    pub fn add_item_defs_to_table(module: &mut Module<SemanticAnnotations>) -> Result<()> {
        let mut annotations = module.annotation().clone();

        let fm = module.get_functions_mut();
        for f in fm.iter_mut() {
            SymbolTable::for_item(f, &mut annotations)?;
        }

        let cm = module.get_coroutines_mut();
        for co in cm.iter_mut() {
            SymbolTable::for_item(co, &mut annotations)?;
        }

        for st in module.get_structs_mut().iter_mut() {
            SymbolTable::for_item(st, &mut annotations)?;
        }

        for e in module.get_externs_mut().iter_mut() {
            SymbolTable::for_item(e, &mut annotations)?;
        }

        for m in module.get_modules_mut().iter_mut() {
            SymbolTable::add_item_defs_to_table(m)?;
        }

        *module.annotation_mut() = annotations;

        Ok(())
    }

    fn for_item(item: &mut Item<SemanticAnnotations>, sym: &mut SemanticAnnotations) -> Result<()> {
        match item {
            Item::Routine(rd) => SymbolTable::add_routine_parameters(rd, sym),
            Item::Struct(sd) => SymbolTable::add_structdef(sd, sym),
            Item::Extern(e) => SymbolTable::add_extern(e, sym),
        }
    }

    fn add_structdef(
        structdef: &mut StructDef<SemanticAnnotations>,
        sym: &mut SemanticAnnotations,
    ) -> Result<()> {
        sym.sym.add(
            structdef.get_name(),
            Type::StructDef(
                structdef
                    .get_fields()
                    .iter()
                    .map(|f| (f.name.clone(), f.ty.clone()))
                    .collect(),
            ),
            false,
            false,
        )
    }

    fn add_extern(
        ex: &mut Extern<SemanticAnnotations>,
        sym: &mut SemanticAnnotations,
    ) -> Result<()> {
        let Extern {
            name, params, ty, ..
        } = ex;

        let def = Type::FunctionDef(Self::get_types_for_params(params), Box::new(ty.clone()));

        sym.sym.add(name, def, false, true)
    }

    fn add_routine_parameters(
        routine: &mut RoutineDef<SemanticAnnotations>,
        sym: &mut SemanticAnnotations,
    ) -> Result<()> {
        let RoutineDef {
            def,
            name,
            params,
            ty,
            ..
        } = routine;

        let def = match def {
            RoutineDefType::Function => {
                Type::FunctionDef(Self::get_types_for_params(params), Box::new(ty.clone()))
            }
            RoutineDefType::Coroutine => {
                Type::CoroutineDef(Self::get_types_for_params(params), Box::new(ty.clone()))
            }
        };

        sym.sym.add(name, def, false, false)
    }

    fn get_types_for_params(params: &Vec<Parameter<SemanticAnnotations>>) -> Vec<Type> {
        params.iter().map(|p| p.ty.clone()).collect::<Vec<Type>>()
    }

    pub(super) fn scope_type(&self) -> &ScopeType {
        &self.ty
    }

    pub fn table(&self) -> &Vec<Symbol> {
        &self.sym
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.sym.iter().find(|s| s.name == name)
    }

    pub fn get_path(&self, name: &Path) -> Option<&Symbol> {
        if name.len() == 1 {
            self.sym.iter().find(|s| s.name == name[0])
        } else {
            None
        }
    }

    pub fn add(&mut self, name: &str, ty: Type, mutable: bool, is_extern: bool) -> Result<()> {
        if self.get(name).is_some() {
            Err(format!("{} already declared", name))
        } else {
            self.sym.push(Symbol {
                name: name.into(),
                ty,
                mutable,
                is_extern,
            });
            Ok(())
        }
    }
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

#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub ty: Type,
    pub mutable: bool,
    pub is_extern: bool,
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
pub(super) enum ScopeType {
    Local,
    Routine,
    Module { name: String },
}
