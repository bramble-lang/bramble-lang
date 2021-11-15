use inkwell::values::*;

use crate::compiler::diagnostics::Writable;

impl<'ctx> Writable for &BasicValueEnum<'ctx> {
    fn write(&self, w: &dyn crate::compiler::diagnostics::Writer) {
        w.write_str(&format!("{}", self.print_to_string()));
    }
}

impl<'ctx> Writable for &InstructionValue<'ctx> {
    fn write(&self, w: &dyn crate::compiler::diagnostics::Writer) {
        w.write_str(&format!("{}", self.print_to_string()));
    }
}

impl<'ctx> Writable for &PointerValue<'ctx> {
    fn write(&self, w: &dyn crate::compiler::diagnostics::Writer) {
        w.write_str(&format!("{}", self.print_to_string()));
    }
}

impl<'ctx> Writable for &FunctionValue<'ctx> {
    fn write(&self, w: &dyn crate::compiler::diagnostics::Writer) {
        w.write_str(&format!("{}", self.print_to_string()));
    }
}
