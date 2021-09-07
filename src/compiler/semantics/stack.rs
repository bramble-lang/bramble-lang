use std::collections::HashMap;

use log::*;

use crate::{
    compiler::ast::{Element, Module, Node, Path, StructDef, Type},
    project::manifest::Manifest,
};
use crate::{compiler::lexer::stringtable::StringId, result::Result};

use super::{
    semanticnode::SemanticContext,
    symbol_table::{ScopeType, Symbol, SymbolTable},
};

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTableScopeStack {
    root: *const Module<SemanticContext>,

    stack: Vec<SymbolTable>,
    head: SymbolTable,
    imported_symbols: HashMap<String, Symbol>, // TODO: change this to a SymbolTable?
}

impl<'a> std::fmt::Display for SymbolTableScopeStack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut i = 0;
        f.write_fmt(format_args!("{}: {}\n", i, self.head))?;
        for scope in self.stack.iter().rev() {
            i += 1;
            f.write_fmt(format_args!("{}: {}\n", i, scope))?;
        }
        Ok(())
    }
}

impl<'a> SymbolTableScopeStack {
    pub fn new(root: &'a Module<SemanticContext>, imports: &[Manifest]) -> SymbolTableScopeStack {
        let mut ss = SymbolTableScopeStack {
            stack: vec![],
            head: SymbolTable::new(),
            root,
            imported_symbols: HashMap::new(),
        };

        ss.add_imports(imports);

        ss
    }

    pub fn get_root(&self) -> &'a Module<SemanticContext> {
        unsafe { self.root.as_ref().expect("Root has does not exist") }
    }

    fn add_imports(&mut self, manifests: &[Manifest]) {
        debug!("Adding Imports");
        // Load all struct imports first because imported functions may depend upon
        // imported structures (If any semantic analysis is done on functions)
        for manifest in manifests.into_iter() {
            for sd in manifest.get_structs().iter() {
                debug!("Import struct {}", sd);
                self.import_structdef(sd);
            }
        }

        for manifest in manifests.into_iter() {
            for (path, params, ret_ty) in manifest.get_functions().iter() {
                debug!("Import function {}", path);
                self.import_function(path.clone(), params.clone(), ret_ty.clone());
            }
        }
    }

    /// Add a function from another module to this symbol table
    /// So that calls to external functions can be type checked.
    pub fn import_function(
        &mut self,
        canonical_name: Path,
        params: Vec<Type>,
        return_ty: Type,
    ) -> Option<Symbol> {
        match canonical_name.item() {
            Some(item) => self.imported_symbols.insert(
                canonical_name.to_string(),
                Symbol {
                    name: item.into(),
                    ty: Type::FunctionDef(params, Box::new(return_ty)),
                    mutable: false,
                    is_extern: false,
                },
            ),
            None => None,
        }
    }

    /// Add a function from another module to this symbol table
    /// So that calls to external functions can be type checked.
    pub fn import_structdef(&mut self, sd: &StructDef<SemanticContext>) -> Option<Symbol> {
        let canon_path = sd.get_context().get_canonical_path();
        match canon_path.item() {
            Some(item) => self.imported_symbols.insert(
                canon_path.to_string(),
                Symbol {
                    name: item.into(),
                    ty: Type::StructDef(
                        sd.get_fields()
                            .iter()
                            .map(|p| (p.name.clone(), p.ty.clone()))
                            .collect(),
                    ),
                    mutable: false,
                    is_extern: false,
                },
            ),
            None => None,
        }
    }

    fn get_imported_symbol(&self, canonical_name: &Path) -> Option<&Symbol> {
        self.imported_symbols.get(&canonical_name.to_string())
    }

    pub fn enter_scope(&mut self, sym: &SymbolTable) {
        self.stack.push(self.head.clone());
        self.head = sym.clone();
    }

    pub fn leave_scope(&mut self) -> SymbolTable {
        let tmp = self.head.clone();
        self.head = self
            .stack
            .pop()
            .expect("SymbolTable stack should never be empty on a pop");
        tmp
    }

    pub fn get_current_fn(&self) -> Option<StringId> {
        // Check if the top of the stack is a routine
        if let ScopeType::Routine(name) = self.head.scope_type() {
            return Some(*name);
        } else {
            // Search through the rest of the stack for the Routine closest to the top
            for scope in self.stack.iter().rev() {
                if let ScopeType::Routine(name) = scope.scope_type() {
                    return Some(*name);
                }
            }
        }
        None
    }

    /// Searches SymbolStack, starting at the top of the stack and moving down,
    /// for a symbol that matches `name`.
    ///
    /// Returns the first match and the canonical path to that match.  
    /// Returns `None` if no matching symbol was found.
    fn get_symbol(&self, name: StringId) -> Option<(&Symbol, Path)> {
        let mut cpath = self.to_path()?;
        let s = self.head.get(name).or_else(|| {
            self.stack.iter().rev().find_map(|scope| {
                let sym = scope.get(name);
                if sym.is_none() && scope.scope_type().is_boundary() {
                    // If we reach the end of the canonical path, there can be no more locations
                    // for the symbol to exist and so we should return None
                    cpath.pop()?;
                }
                sym
            })
        });

        s.map(|s| {
            cpath.push(Element::Id(name));
            (s, cpath)
        })
    }

    /// Add a new symbol to the current symbol table (the SymbolTable that is at the
    /// top of the stack).
    pub fn add(&mut self, name: StringId, ty: Type, mutable: bool, is_extern: bool) -> Result<()> {
        self.head.add(name, ty, mutable, is_extern)
    }

    /// Finds the given variable in the current symbol table or in the symbol table history
    /// Follows scoping rules, so when a boundary scope is reached (e.g. a Routine) it will
    /// stop searching
    pub fn lookup_var(&'a self, id: StringId) -> Result<&'a Symbol> {
        let (symbol, _) = &self.lookup_symbol_by_path(&vec![Element::Id(id)].into())?;
        match symbol.ty {
            Type::FunctionDef(..)
            | Type::CoroutineDef(..)
            | Type::ExternDecl(..)
            | Type::StructDef { .. }
            | Type::Unknown => return Err(format!("{} is not a variable", id)),
            Type::Custom(..)
            | Type::Coroutine(_)
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::Bool
            | Type::StringLiteral
            | Type::Array(_, _)
            | Type::Unit => Ok(symbol),
        }
    }

    /// Specifically looks for a routine (function or coroutine) with the given ID.  Will search upward through the scope
    /// hierarchy until a symbol is found that matches `id`. If that symbol is a routine it is returned
    /// if the symbol is not a routine `Err` is returned.  If no symbol is found `Err` is returned.
    pub fn lookup_func_or_cor(&'a self, id: StringId) -> Result<(&Vec<Type>, &Type)> {
        match self.lookup_symbol_by_path(&vec![Element::Id(id)].into())?.0 {
            Symbol {
                ty: Type::CoroutineDef(params, p),
                ..
            }
            | Symbol {
                ty: Type::FunctionDef(params, p),
                ..
            } => Ok((params, p)),
            _ => return Err(format!("{} is not a coroutine or function", id)),
        }
    }

    /// Specifically looks for a coroutine with the given ID.  Will search upward through the scope
    /// hierarchy until a symbol is found that matches `id`. If that symbol is a coroutine it is returned
    /// if the symbol is not a coroutine `Err` is returned.  If no symbol is found `Err` is returned.
    pub fn lookup_coroutine(&'a self, id: StringId) -> Result<(&Vec<Type>, &Type)> {
        match self.lookup_symbol_by_path(&vec![Element::Id(id)].into())?.0 {
            Symbol {
                ty: Type::CoroutineDef(params, p),
                ..
            } => Ok((params, p)),
            _ => return Err(format!("{} is not a coroutine", id)),
        }
    }

    /// Uses the given path to navigate through the Symbol table hierarchy to locate
    /// a specific symbol.  If the path is invalid (contains modules which do not exist)
    /// or the item identified by the path does not exist, then an error is returned.
    ///
    /// This function will work with relative and canonical paths.
    pub fn lookup_symbol_by_path(&'a self, path: &Path) -> Result<(&'a Symbol, Path)> {
        if path.len() > 1 {
            let canon_path = self.to_canonical(path)?;

            // Look in the project being compiled
            let project_symbol = self.get_item(&canon_path);

            // look in any imported symbols
            let imported_symbol = self.get_imported_symbol(&canon_path);

            // Make sure that there is no ambiguity about what is being referenced
            match (project_symbol, imported_symbol) {
                (Some(ps), None) => Ok((ps, canon_path)),
                (None, Some(is)) => Ok((is, canon_path)),
                (Some(_), Some(_)) => Err(format!("Found multiple definitions of {}", path)),
                (None, None) => Err(format!(
                    "Could not find item with the given path: {} ({})",
                    path, canon_path
                )),
            }
        } else if path.len() == 1 {
            // If the path has just the item name, then check the local scope and
            // the parent scopes for the given symbol
            match path.item() {
                Some(item) => self
                    .get_symbol(item)
                    .ok_or(format!("{} is not defined", item)),
                None => Err("Path does not end with a valid identifier".into()),
            }
        } else {
            Err("empty path passed to lookup_path".into())
        }
    }

    fn get_item(&self, canon_path: &Path) -> Option<&Symbol> {
        // If the path contains more than just the item's name then
        // traverse the parent path to find the specified item
        let item = canon_path
            .item()
            .expect("Expected a canonical path with at least one step in it");

        if !canon_path.is_canonical() {
            panic!("Given path is not canonical: {}", canon_path);
        }

        let mut current = self.root;
        // Follow the path, up to, but not including the final element of the path
        // (which is the item being looked for);
        unsafe {
            for idx in 1..canon_path.len() - 1 {
                if let Element::Id(id) = canon_path[idx] {
                    match (*current).get_module(id) {
                        Some(m) => current = m,
                        None => return None,
                    }
                } else {
                    panic!("Canonical path must consistent entirely of identifiers after the root element")
                }
            }

            (*current).get_context().sym.get(item)
        }
    }

    /**
    Given a type reference that appears in the current node, will convert that type reference
    to a canonical path from a relative path.  If the type reference is already an absolute
    path then no change is made.

    For example, the path `super::MyStruct` would be converted to `root::my_module::MyStruct`
    if the current node were in a module contained within `my_module`.
     */
    pub fn canonize_type(&self, ty: &Type) -> Result<Type> {
        match ty {
            Type::Custom(path) => self
                .lookup_symbol_by_path(path)
                .map(|(_, p)| Type::Custom(p)),
            Type::Coroutine(ty) => Ok(Type::Coroutine(Box::new(self.canonize_type(&ty)?))),
            Type::CoroutineDef(params, ret_ty) => {
                let cparams = params
                    .iter()
                    .map(|pty| self.canonize_type(pty))
                    .collect::<Result<Vec<Type>>>()?;
                let cret_ty = self.canonize_type(ret_ty)?;
                Ok(Type::CoroutineDef(cparams, Box::new(cret_ty)))
            }
            Type::FunctionDef(params, ret_ty) => {
                let cparams = params
                    .iter()
                    .map(|pty| self.canonize_type(pty))
                    .collect::<Result<Vec<Type>>>()?;
                let cret_ty = self.canonize_type(ret_ty)?;
                Ok(Type::FunctionDef(cparams, Box::new(cret_ty)))
            }
            Type::StructDef(params) => {
                let cparams = params
                    .iter()
                    .map(|(name, ty)| self.canonize_type(ty).map(|ty| (*name, ty)))
                    .collect::<Result<Vec<(StringId, Type)>>>()?;
                Ok(Type::StructDef(cparams))
            }
            Type::ExternDecl(params, has_varargs, ret_ty) => {
                let cparams = params
                    .iter()
                    .map(|pty| self.canonize_type(pty))
                    .collect::<Result<Vec<Type>>>()?;
                let cret_ty = self.canonize_type(ret_ty)?;
                Ok(Type::ExternDecl(cparams, *has_varargs, Box::new(cret_ty)))
            }
            Type::Array(el_ty, len) => {
                if *len <= 0 {
                    Err(format!("Expected length > 0 for array, but found {}", *len))
                } else {
                    Ok(Type::Array(box self.canonize_type(el_ty)?, *len))
                }
            }
            _ => Ok(ty.clone()),
        }
    }

    /// Converts a relative path, `path`, into a canonical path by merging it with
    /// the path to the current node, as represented by the stack.
    pub fn to_canonical(&self, path: &Path) -> Result<Path> {
        let current_path = self.to_path().ok_or("A valid path is expected")?;
        path.to_canonical(&current_path)
    }

    /// Starting from the bottom of the stack this builds a path
    /// of all the modules that we are current in, in effect
    /// the current path within the AST.
    pub fn to_path(&self) -> Option<Path> {
        let mut steps = vec![Element::CanonicalRoot];

        for node in self.stack.iter() {
            match node.scope_type() {
                ScopeType::Module(name) => steps.push(Element::Id(*name)),
                ScopeType::Local | ScopeType::Routine(_) => (),
            }
        }

        match self.head.scope_type() {
            ScopeType::Module(name) => steps.push(Element::Id(*name)),
            ScopeType::Local | ScopeType::Routine(_) => (),
        }

        if steps.len() > 0 {
            Some(steps.into())
        } else {
            None
        }
    }
}
