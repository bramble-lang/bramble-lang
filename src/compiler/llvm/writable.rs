use inkwell::values::{AnyValue, BasicValueEnum};

use crate::compiler::diagnostics::Writable;

impl<'ctx> Writable for BasicValueEnum<'ctx> {
    fn write(&self, w: &dyn crate::compiler::diagnostics::Writer) {
        w.write_str(&format!("{}", self.print_to_string()));
    }
}
