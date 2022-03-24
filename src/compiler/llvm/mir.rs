//! Transforms the MIR representation into LLVM

use inkwell::{builder::Builder, context::Context, module::Module, values::*};

use crate::{
    compiler::mir::{ir::*, Transformer},
    StringId, StringTable,
};

struct LlvmTransformer<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    table: &'ctx StringTable,
    function: FunctionValue<'ctx>,
}

impl<'ctx> LlvmTransformer<'ctx> {
    pub fn new(
        func_name: StringId,
        ctx: &'ctx Context,
        module: &str,
        table: &'ctx StringTable,
    ) -> Self {
        let module = ctx.create_module(module);

        // Create a function to build
        let ft = ctx.void_type().fn_type(&[], false);
        let name = table.get(func_name).unwrap();
        let function = module.add_function(&name, ft, None);

        let builder = ctx.create_builder();

        Self {
            context: ctx,
            module,
            builder,
            table,
            function,
        }
    }
}

impl<'ctx> Transformer<PointerValue<'ctx>, BasicValueEnum<'ctx>> for LlvmTransformer<'ctx> {
    fn start_bb(&mut self, bb: BasicBlockId) {
        let bb = self.context.append_basic_block(self.function, "");
        self.builder.position_at_end(bb);
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
        self.builder.build_store(l, v);
    }

    fn lvalue(&self, l: &LValue) -> PointerValue<'ctx> {
        match l {
            LValue::Static(_) => todo!(),
            LValue::Var(v) => {
                // TODO: VarId should not exist outside of the MIR.  The TFormer should get VarDecl or TempDecl?
                // One reason for this is that the Xform should be one-way/push.  If the VarId is passed to the XFormer
                // it needs to be able to query the MirProcedure and pull the VarDecl, which makes this a two-way xform.
                //
                // Exception to this is the TypeTable/TypeId? What about FunctionTable/FunctionId?
                //
                // Create label for variable
                // Create pointer to label
                todo!()
            }
            LValue::Temp(_) => todo!(),
            LValue::Access(_, _) => todo!(),
            LValue::ReturnPointer => todo!(),
        }
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
