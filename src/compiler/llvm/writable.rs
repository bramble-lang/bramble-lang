use inkwell::{types::*, values::*};

use crate::compiler::diagnostics::Writable;

impl<'ctx> Writable for &BasicValueEnum<'ctx> {
    fn write(&self, w: &dyn crate::compiler::diagnostics::Writer) {
        let s = self.print_to_string().to_string().replace("\n", "\\n");
        w.write_text(&format!("{}", s));
    }
}

impl<'ctx> Writable for &InstructionValue<'ctx> {
    fn write(&self, w: &dyn crate::compiler::diagnostics::Writer) {
        let s = self.print_to_string().to_string().replace("\n", "\\n");
        w.write_text(&format!("{}", s));
    }
}

impl<'ctx> Writable for &PointerValue<'ctx> {
    fn write(&self, w: &dyn crate::compiler::diagnostics::Writer) {
        let s = self.print_to_string().to_string().replace("\n", "\\n");
        w.write_text(&format!("{}", s));
    }
}

impl<'ctx> Writable for &FunctionValue<'ctx> {
    fn write(&self, w: &dyn crate::compiler::diagnostics::Writer) {
        let s = self.print_to_string().to_string().replace("\n", "\\n");
        w.write_text(&format!("{}", s));
    }
}

impl<'ctx> Writable for &CallSiteValue<'ctx> {
    fn write(&self, w: &dyn crate::compiler::diagnostics::Writer) {
        let s = self.print_to_string().to_string().replace("\n", "\\n");
        w.write_text(&format!("{}", s));
    }
}

impl<'ctx> Writable for &StructType<'ctx> {
    fn write(&self, w: &dyn crate::compiler::diagnostics::Writer) {
        let s = self.print_to_string().to_string().replace("\n", "\\n");
        w.write_text(&format!("{}", s));
    }
}

impl<'ctx> Writable for &BasicTypeEnum<'ctx> {
    fn write(&self, w: &dyn crate::compiler::diagnostics::Writer) {
        let s = self.print_to_string().to_string().replace("\n", "\\n");
        w.write_text(&format!("{}", s));
    }
}
