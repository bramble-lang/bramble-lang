//! Transforms the MIR representation into LLVM

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{BasicValueEnum, PointerValue},
};

use crate::compiler::mir::{ir::*, Transformer};

struct LlvmTransformer<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
}

impl<'ctx> LlvmTransformer<'ctx> {
    pub fn new(ctx: &'ctx Context, module: &str) -> Self {
        Self {
            context: ctx,
            module: ctx.create_module(module),
            builder: ctx.create_builder(),
        }
    }
}

impl<'ctx> Transformer<PointerValue<'ctx>, BasicValueEnum<'ctx>> for LlvmTransformer<'ctx> {
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

    fn assign(
        &mut self,
        span: crate::compiler::Span,
        l: PointerValue<'ctx>,
        v: BasicValueEnum<'ctx>,
    ) {
        todo!()
    }

    fn lvalue(&self, l: &LValue) -> PointerValue<'ctx> {
        todo!()
    }

    fn constant(&self, c: Constant) -> BasicValueEnum<'ctx> {
        todo!()
    }

    fn load(&self, lv: PointerValue<'ctx>) -> BasicValueEnum<'ctx> {
        todo!()
    }

    fn add(&self, a: BasicValueEnum<'ctx>, b: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        todo!()
    }

    fn sub(&self, a: BasicValueEnum<'ctx>, b: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        todo!()
    }
}
