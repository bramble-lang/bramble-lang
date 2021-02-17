use crate::{compiler::x86::assembly::Reg64, diagnostics::{Diag, DiagData}};
use crate::semantics::semanticnode::SemanticAnnotations;
use crate::{ast::*, compiler::arch::registers::RegSize};
use crate::{compiler::x86::assembly::Reg, semantics};

use super::{
    struct_table::ResolvedStructTable,
    symbol_table::{Symbol, SymbolTable},
};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct LayoutData {
    pub(super) offset: i32,
}

impl LayoutData {
    pub fn new(offset: i32) -> LayoutData {
        LayoutData { offset }
    }
}

/**
 * For every node in the AST, this models the scope of what symbols are
 * available to a node and all children of that node. The `level` of the
 * scope dictates its semantic role in determing whether the symbols in
 * its parents are available.
 *
 * There are three types of scopes: Local, Routine, and Module.  For a
 * specific node that has a local scope, it can traverse its ancestors
 * and use symbols that are in its scope up to and including the Routine
 * level, and then it cannot access any variables above that node.
 *
 * Routine CompilerAnnotations will also track the amount of space which must be allocated
 * for the routine's stackframe (in order to store all parameters and
 * local variables).
 *
 * The Symbol Table stores all the symbols that are defined at this node
 * and their size in bytes and their relative offset to the stack frame.
 */
#[derive(Clone, Debug, PartialEq)]
pub struct CompilerAnnotation {
    pub(super) id: u32,
    pub(super) line: u32,
    pub(super) level: Level,
    pub(super) ty: Type,
    pub(super) symbols: SymbolTable,
    pub(super) canon_path: Path,
    pub(super) reg_size: Option<RegSize>,
}

impl Annotation for CompilerAnnotation {
    fn line(&self) -> u32 {
        self.line
    }

    fn id(&self) -> u32 {
        self.id
    }
}

impl Diag for CompilerAnnotation {
    fn diag(&self) -> DiagData {
        let mut dd = DiagData::new(self.line, self.id);
        dd.add("ty", &format!("{}", self.ty));
        dd.add("reg_size", &format!("{:?}", self.reg_size));
        dd
    }
}

impl CompilerAnnotation {
    pub fn new(id: u32, level: Level, canon_path: Path, ty: Type) -> CompilerAnnotation {
        CompilerAnnotation {
            id,
            line: 0,
            level,
            ty,
            symbols: SymbolTable::new(),
            canon_path,
            reg_size: None,
        }
    }

    pub fn from(a: &SemanticAnnotations, level: Level) -> CompilerAnnotation {
        CompilerAnnotation {
            id: a.id.clone(),
            line: a.line(),
            level,
            ty: a.ty.clone(),
            symbols: SymbolTable::new(),
            canon_path: a.canonical_path.clone(),
            reg_size: None,
        }
    }

    pub fn new_routine(
        a: &SemanticAnnotations,
        routine_type: RoutineDefType,
    ) -> CompilerAnnotation {
        CompilerAnnotation::from(a, Level::Routine { routine_type })
    }

    pub fn new_module(a: &SemanticAnnotations, name: &str) -> CompilerAnnotation {
        CompilerAnnotation::from(a, Level::Module { name: name.into() })
    }

    pub fn id(&self) -> u32 {
        self.id
    }

    pub fn level(&self) -> &Level {
        &self.level
    }

    pub fn symbols(&self) -> &SymbolTable {
        &self.symbols
    }

    pub fn insert(&mut self, name: &str, size: i32, offset: i32) -> i32 {
        self.symbols
            .table
            .insert(name.into(), Symbol::new(name, size, offset + size));
        offset + size
    }

    pub fn merge(
        &mut self,
        symbols: &semantics::symbol_table::SymbolTable,
        mut current_offset: i32,
        struct_table: &ResolvedStructTable,
    ) -> i32 {
        for s in symbols.table().iter() {
            current_offset = self.insert(
                &s.name,
                struct_table.size_of(&s.ty).expect(&format!(
                    "Cannot get size for {}\nStruct Table:\n{}\n",
                    s.ty, struct_table
                )),
                current_offset,
            );
        }
        current_offset
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.symbols.table.get(name)
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn ty(&self) -> &Type {
        &self.ty
    }

    pub fn canon_path(&self) -> &Path {
        &self.canon_path
    }

    pub fn anonymous_name(&self) -> String {
        format!("!{}_{}", self.canon_path, self.id)
    }

    pub fn reg_size(&self) -> Option<RegSize> {
        self.reg_size
    }

    pub fn scale_reg(&self, reg: Reg64) -> Option<Reg> {
        let reg_sz = self.reg_size()?;
        reg.scale(reg_sz)
    }

    pub fn set_reg_size(&mut self, r: Option<RegSize>) {
        self.reg_size = r;
    }

    pub(super) fn local_from(
        m: &SemanticAnnotations,
        struct_table: &ResolvedStructTable,
        current_layout: LayoutData,
    ) -> (CompilerAnnotation, LayoutData) {
        let mut layout = current_layout;
        let mut scope = CompilerAnnotation::from(m, Level::Local);
        scope.line = m.ln;
        layout.offset = scope.merge(&m.sym, layout.offset, struct_table);
        (scope, layout)
    }

    pub(super) fn routine_from(
        m: &SemanticAnnotations,
        routine_type: RoutineDefType,
        struct_table: &ResolvedStructTable,
    ) -> (CompilerAnnotation, LayoutData) {
        let mut layout = LayoutData::new(match routine_type {
            RoutineDefType::Function => 0,
            RoutineDefType::Coroutine => 40,
        });

        let mut scope = CompilerAnnotation::new_routine(m, routine_type);

        layout.offset = scope.merge(&m.sym, layout.offset, struct_table);

        (scope, layout)
    }

    pub(super) fn module_from(
        m: &SemanticAnnotations,
        name: &str,
        struct_table: &ResolvedStructTable,
    ) -> (CompilerAnnotation, LayoutData) {
        let mut scope = CompilerAnnotation::new_module(m, name);

        let layout = LayoutData::new(0);
        scope.merge(&m.sym, layout.offset, struct_table);

        (scope, layout)
    }

    pub(super) fn structdef_from(m: &SemanticAnnotations) -> (CompilerAnnotation, LayoutData) {
        let mut scope = CompilerAnnotation::from(m, Level::Local);
        scope.line = m.ln;
        let layout = LayoutData::new(0);
        (scope, layout)
    }
}

impl std::fmt::Display for CompilerAnnotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("ID: {} | ", self.id))?;
        f.write_fmt(format_args!("Level: {} | ", self.level))?;
        f.write_fmt(format_args!("Type: {}\n", self.ty))?;
        f.write_fmt(format_args!(
            "Symbols (! prefix indicates anonymous symbol):\n{}",
            self.symbols
        ))?;

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Level {
    Local,
    Routine { routine_type: RoutineDefType },
    Module { name: String },
}

impl std::fmt::Display for Level {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Level::Local => f.write_str("Local"),
            Level::Routine { routine_type } => {
                f.write_fmt(format_args!("Routine: [Type: {}]", routine_type))
            }
            Level::Module { name } => f.write_fmt(format_args!("Module: {}", name)),
        }
    }
}
