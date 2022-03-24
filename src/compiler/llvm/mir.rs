//! Transforms the MIR representation into LLVM

use inkwell::values::BasicValueEnum;

use crate::compiler::mir::{ir::*, Transformer};

struct LlvmTransformer {}

impl<'ctx, L> Transformer<L, BasicValueEnum<'ctx>> for LlvmTransformer {
    fn start_bb(&mut self, bb: BasicBlockId) {
        todo!()
    }

    fn add_var(&mut self) {
        todo!()
    }

    fn add_temp(&mut self) {
        todo!()
    }

    fn term_return(&mut self) {
        todo!()
    }

    fn assign(&mut self, span: crate::compiler::Span, l: L, v: BasicValueEnum<'ctx>) {
        todo!()
    }

    fn lvalue(&self, l: &LValue) -> L {
        todo!()
    }

    fn constant(&self, c: Constant) -> BasicValueEnum<'ctx> {
        todo!()
    }

    fn load(&self, lv: L) -> BasicValueEnum<'ctx> {
        todo!()
    }

    fn add(&self, a: BasicValueEnum<'ctx>, b: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        todo!()
    }

    fn sub(&self, a: BasicValueEnum<'ctx>, b: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        todo!()
    }
}
