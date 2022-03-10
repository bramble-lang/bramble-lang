use log::debug;

use crate::{
    compiler::{ast::*, semantics::semanticnode::SemanticContext, source::SourceIr, Span},
    StringId,
};

use super::SemanticError;

/**
 `SymbolTable` is an AST node context that contains information about symbols that
 are defined by immediate children of this node. For example:

``` ignore
 mod my_mod {
      fn hello(i: i64) {
          let x: i64 := i * i;
          // do something
      }
 }
```

 The node representing `my_mod` would have a child node representing the definition of
 `hello`; the `SymbolTable` context on `my_mod`'s node would contain information for
 the symbol `hello`, but it would _not_ contain the symbol for `x` which is whithin the
 body of `hello`.

 The `SymbolTable` combined with the hierarchical structure of the AST provides the
 solution for symbol scopes.  At any given node, the only symbols that node can possibly
 know about are the ones in the `SymbolTable`s of the nodes that comprise the path from
 the root of the AST to the given node.
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

    pub fn new_routine(name: StringId) -> Self {
        SymbolTable {
            ty: ScopeType::Routine(name),
            sym: vec![],
        }
    }

    pub fn new_module(name: StringId) -> Self {
        SymbolTable {
            ty: ScopeType::Module(name),
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
    pub fn add_item_defs_to_table(
        module: &mut Module<SemanticContext>,
    ) -> Result<(), SemanticError> {
        debug!("Initialize symbol tables for AST");
        let mut context = module.context().clone();

        let fm = module.get_functions_mut();
        for f in fm.iter_mut() {
            SymbolTable::for_item(f, &mut context)?;
        }

        let cm = module.get_coroutines_mut();
        for co in cm.iter_mut() {
            SymbolTable::for_item(co, &mut context)?;
        }

        for st in module.get_structs_mut().iter_mut() {
            SymbolTable::for_item(st, &mut context)?;
        }

        for e in module.get_externs_mut().iter_mut() {
            SymbolTable::for_item(e, &mut context)?;
        }

        for m in module.get_modules_mut().iter_mut() {
            SymbolTable::add_item_defs_to_table(m)?;
        }

        *module.get_context_mut() = context;

        Ok(())
    }

    fn for_item(
        item: &mut Item<SemanticContext>,
        sym: &mut SemanticContext,
    ) -> Result<(), SemanticError> {
        match item {
            Item::Routine(rd) => SymbolTable::add_routine_parameters(rd, sym),
            Item::Struct(sd) => SymbolTable::add_structdef(sd, sym),
            Item::Extern(e) => SymbolTable::add_extern(e, sym),
        }
    }

    fn add_structdef(
        structdef: &mut StructDef<SemanticContext>,
        sym: &mut SemanticContext,
    ) -> Result<(), SemanticError> {
        sym.add_symbol(
            structdef.get_name(),
            Type::StructDef(
                structdef
                    .get_fields()
                    .iter()
                    .map(|f| (f.name, f.ty.clone()))
                    .collect(),
            ),
            false,
            false,
            structdef.span(),
        )
    }

    fn add_extern(
        ex: &mut Extern<SemanticContext>,
        sym: &mut SemanticContext,
    ) -> Result<(), SemanticError> {
        let Extern {
            name, params, ty, ..
        } = ex;

        let def = Type::ExternDecl(
            Self::get_types_for_params(params),
            ex.has_varargs,
            Box::new(ty.clone()),
        );

        sym.add_symbol(*name, def, false, true, ex.span())
    }

    fn add_routine_parameters(
        routine: &mut RoutineDef<SemanticContext>,
        sym: &mut SemanticContext,
    ) -> Result<(), SemanticError> {
        let RoutineDef {
            def,
            name,
            params,
            ret_ty: ty,
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

        sym.add_symbol(*name, def, false, false, routine.span())
    }

    fn get_types_for_params(params: &[Parameter<SemanticContext>]) -> Vec<Type> {
        params.iter().map(|p| p.ty.clone()).collect::<Vec<Type>>()
    }

    pub(super) fn scope_type(&self) -> &ScopeType {
        &self.ty
    }

    pub fn table(&self) -> &Vec<Symbol> {
        &self.sym
    }

    pub fn table_mut(&mut self) -> &mut Vec<Symbol> {
        &mut self.sym
    }

    pub fn get(&self, name: StringId) -> Option<&Symbol> {
        self.sym.iter().find(|s| s.name == name)
    }

    pub fn get_path(&self, name: &Path) -> Option<&Symbol> {
        if name.len() == 1 {
            self.sym.iter().find(|s| Element::Id(s.name) == name[0])
        } else {
            None
        }
    }

    pub fn add(
        &mut self,
        name: StringId,
        ty: Type,
        mutable: bool,
        is_extern: bool,
        span: Span,
    ) -> Result<(), SemanticError> {
        if self.get(name).is_some() {
            Err(SemanticError::AlreadyDeclared(name))
        } else {
            self.sym.push(Symbol {
                name,
                ty,
                is_mutable: mutable,
                is_extern,
                span: Some(span),
            });
            Ok(())
        }
    }

    pub fn size(&self) -> usize {
        self.sym.len()
    }
}

impl std::fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}\n", self.ty))?;
        f.write_str("\tName | Type | Mutable\n")?;
        for symbol in self.sym.iter() {
            f.write_fmt(format_args!("\t{}\n", symbol))?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
    pub name: StringId,
    pub ty: Type,
    pub is_mutable: bool,
    pub is_extern: bool,
    pub span: Option<Span>,
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{} | {} | {}",
            self.name, self.ty, self.is_mutable
        ))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(super) enum ScopeType {
    Local,
    Routine(StringId),
    Module(StringId),
}

impl ScopeType {
    pub fn is_boundary(&self) -> bool {
        match self {
            Self::Routine(..) | Self::Local => false,
            Self::Module(..) => true,
        }
    }

    pub fn get_name(&self) -> Option<StringId> {
        match self {
            Self::Local => None,
            Self::Routine(name) | Self::Module(name) => Some(*name),
        }
    }
}

impl std::fmt::Display for ScopeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ScopeType::Local => f.write_str("Local"),
            ScopeType::Routine(name) => f.write_fmt(format_args!("Routine({})", name)),
            ScopeType::Module(name) => f.write_fmt(format_args!("Module({})", name)),
        }
    }
}
