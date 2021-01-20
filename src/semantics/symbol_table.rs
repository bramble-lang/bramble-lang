use crate::{semantics::semanticnode::SemanticMetadata, syntax::structdef::StructDef};
use crate::syntax::path::Path;
use crate::syntax::{
    module::{Item, Module},
    routinedef::{RoutineDef, RoutineDefType},
    ty::Type,
};
use braid_lang::result::Result;

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

    pub(super) fn scope_type(&self) -> &ScopeType{
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

    pub fn add(&mut self, name: &str, ty: Type, mutable: bool) -> Result<()> {
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

    pub fn for_module(module: &mut Module<SemanticMetadata>) -> Result<()> {
        let mut metadata = module.get_metadata().clone();

        let fm = module.get_functions_mut();
        for f in fm.iter_mut() {
            SymbolTable::for_item(f, &mut metadata)?;
        }

        let cm = module.get_coroutines_mut();
        for co in cm.iter_mut() {
            SymbolTable::for_item(co, &mut metadata)?;
        }
        for st in module.get_structs_mut().iter_mut() {
            SymbolTable::for_item(st, &mut metadata)?;
        }

        for m in module.get_modules_mut().iter_mut() {
            SymbolTable::for_module(m)?;
        }

        *module.get_metadata_mut() = metadata;

        Ok(())
    }

    fn for_item(item: &mut Item<SemanticMetadata>, sym: &mut SemanticMetadata) -> Result<()> {
        match item {
            Item::Routine(rd) =>
                SymbolTable::for_routine(rd, sym),
            Item::Struct(sd) =>
                SymbolTable::add_structdef(sd, sym),
        }
    }

    fn add_structdef(
        structdef: &mut StructDef<SemanticMetadata>,
        sym: &mut SemanticMetadata,
    ) -> Result<()> {
        sym.sym.add(
            structdef.get_name(),
            Type::StructDef(structdef.get_fields().clone()),
            false,
        )
    }

    fn for_routine(
        routine: &mut RoutineDef<SemanticMetadata>,
        sym: &mut SemanticMetadata,
    ) -> Result<()> {
        let RoutineDef {
            def,
            name,
            params,
            ty,
            ..
        } = routine;

        let def = match def {
            RoutineDefType::Function => Type::FunctionDef(
                Self::get_types_for_params(params),
                Box::new(ty.clone()),
            ),
            RoutineDefType::Coroutine => Type::CoroutineDef(
                Self::get_types_for_params(params),
                Box::new(ty.clone()),
            ),
        };

        sym.sym.add(name, def, false)
    }

    fn get_types_for_params(params: &Vec<(String,Type)>) -> Vec<Type> {
        params
            .iter()
            .map(|(_, ty)| ty.clone())
            .collect::<Vec<Type>>()
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
