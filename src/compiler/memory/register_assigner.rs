use crate::{compiler::arch::registers::RegSize, syntax::traversal::TraverserMut};
use crate::compiler::memory::scope::CompilerAnnotation;
use crate::compiler::memory::struct_table::ResolvedStructTable;
use crate::syntax::module::*;
use crate::syntax::ty::Type;
use crate::TracingConfig;

/**
 * This traverses the AST and determines what size register to
 * assign to each node in the AST: if it makes sense to assign
 * it to a register.
 */

pub fn assign(tracing: TracingConfig, m: &mut Module<CompilerAnnotation>, struct_table: &ResolvedStructTable) {
    let tm = TraverserMut::new("Register Assigner", tracing, trace);
    tm.for_module(m, |a| {
        assign_register(a, struct_table);
    })
}

fn trace(ca: &CompilerAnnotation) -> String {
    match ca.reg_size {
        Some(rs) => format!("{}", rs),
        None => "Unassigned".into()
    }
}

/**
    * Determine the size of register needed to store a value, based upon the number of bytes
    * the type takes.
    *
    * Custom types are always represented using 64bit registers because they are currently
    * always referred to via addresses.
    */
pub fn register_size_for_type(
    ty: &Type,
    struct_table: &ResolvedStructTable,
) -> Option<RegSize> {
    match ty {
        Type::Custom(_) => Some(RegSize::R64),
        _ => {
            let sz = struct_table.size_of(ty);
            sz.and_then(|sz| RegSize::assign(sz as usize))
        }
    }
}

fn assign_register(a: &mut CompilerAnnotation, struct_table: &ResolvedStructTable) {
    let reg = register_size_for_type(a.ty(), struct_table);
    a.set_reg_size(reg);
}
