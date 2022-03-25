//! Transforms the MIR representation into LLVM

use std::collections::{hash_map::Entry, HashMap};

use inkwell::{builder::Builder, context::Context, module::Module, values::*};

use crate::{
    compiler::{
        mir::{ir::*, Transformer, TransformerResult},
        Span,
    },
    StringId, StringTable,
};

struct LlvmFunctionTransformer<'a, 'ctx> {
    /// LLVVM Context
    context: &'ctx Context,

    /// LLVM Module
    module: &'a Module<'ctx>,

    /// Used to construct actual LLVM instructions and add them to a function
    builder: &'a Builder<'ctx>,

    /// Table mapping [`StringIds`](StringId) to the string value
    str_table: &'ctx StringTable,

    /// The LLVM function instance that is currently being built by the transformer
    /// all insructions will be added to this function.
    function: FunctionValue<'ctx>,

    /// Mapping of [`VarIds`](VarId) to their LLVM value, this is used to look up
    /// variables after they have been allocated.
    vars: HashMap<VarId, PointerValue<'ctx>>,

    /// Table to manage looking up the LLVM BasicBlock via the [`BasicBlockId`].
    blocks: HashMap<BasicBlockId, inkwell::basic_block::BasicBlock<'ctx>>,
}

impl<'a, 'ctx> LlvmFunctionTransformer<'a, 'ctx> {
    pub fn new(
        func_name: StringId,
        ctx: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
        table: &'ctx StringTable,
    ) -> Self {
        // Create a function to build
        let ft = ctx.void_type().fn_type(&[], false);
        let name = table.get(func_name).unwrap();
        let function = module.add_function(&name, ft, None);

        Self {
            context: ctx,
            module,
            builder,
            str_table: table,
            function,
            vars: HashMap::default(),
            blocks: HashMap::new(),
        }
    }

    fn to_label(&self, vd: &VarDecl) -> String {
        let name = self.str_table.get(vd.name()).unwrap();
        let scope = vd.scope();
        format!("{}_{}", name, scope)
    }
}

impl<'a, 'ctx> Transformer<PointerValue<'ctx>, BasicValueEnum<'ctx>>
    for LlvmFunctionTransformer<'a, 'ctx>
{
    fn create_bb(&mut self, id: BasicBlockId) -> Result<(), TransformerResult> {
        let bb = self
            .context
            .append_basic_block(self.function, &id.to_string());
        if self.blocks.insert(id, bb).is_none() {
            Ok(())
        } else {
            Err(TransformerResult::BasicBlockAlreadyCreated)
        }
    }

    fn set_bb(&mut self, id: BasicBlockId) -> Result<(), TransformerResult> {
        match self.blocks.get(&id) {
            Some(bb) => {
                self.builder.position_at_end(*bb);
                Ok(())
            }
            None => Err(TransformerResult::BasicBlockNotFound),
        }
    }

    fn alloc_var(&mut self, id: VarId, decl: &VarDecl) -> Result<(), TransformerResult> {
        let name = self.to_label(decl);

        // Check if variable name already exists
        match self.vars.entry(id) {
            // If it does -> return an error
            Entry::Occupied(_) => Err(TransformerResult::VariableAlreadyAllocated),

            // If not, then allocate a pointer in the Builder
            Entry::Vacant(ve) => {
                // and add a mapping from VarID to the pointer in the local var table
                let ty = self.context.i64_type();
                let ptr = self.builder.build_alloca(ty, &name);
                ve.insert(ptr);
                Ok(())
            }
        }
    }

    fn alloc_temp(&mut self) {
        todo!()
    }

    fn term_return(&mut self) {
        self.builder.build_return(None);
    }

    fn term_cond_goto(
        &mut self,
        cond: BasicValueEnum<'ctx>,
        then_bb: BasicBlockId,
        else_bb: BasicBlockId,
    ) {
        // Look up then_bb
        let then_bb = self.blocks.get(&then_bb).unwrap();
        // Look up else_bb
        let else_bb = self.blocks.get(&else_bb).unwrap();
        // Create conditional jump to then or else
        self.builder
            .build_conditional_branch(cond.into_int_value(), *then_bb, *else_bb);
    }

    fn term_goto(&mut self, target: BasicBlockId) {
        let target = self.blocks.get(&target).unwrap();
        self.builder.build_unconditional_branch(*target);
    }

    fn assign(&mut self, span: Span, l: PointerValue<'ctx>, v: BasicValueEnum<'ctx>) {
        self.builder.build_store(l, v);
    }

    fn var(&self, v: VarId) -> PointerValue<'ctx> {
        *self
            .vars
            .get(&v)
            .expect("Cound not find given VarId in vars table")
    }

    fn const_i64(&self, i: i64) -> BasicValueEnum<'ctx> {
        self.context.i64_type().const_int(i as u64, true).into()
    }

    fn const_bool(&self, b: bool) -> BasicValueEnum<'ctx> {
        let bt = self.context.bool_type();
        bt.const_int(b as u64, true).into()
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

#[cfg(test)]
mod mir2llvm_tests_visual {
    //! A set of unit tests which compile small examples of Bramble code and output
    //! the LLVM IR generated by transforming MIR to LLVM IR.
    //!
    //! These visual tests are used to help with development, testing, and debugging
    //! of the MIR to LLVM transformation process. They also serve to fill in a gap
    //! caused by the complexity that is created if we try to use LLVM's JIT to do
    //! automated unit testing for MIR to LLVM transformation.

    use inkwell::context::Context;

    use crate::{
        compiler::{
            ast::{Module, MAIN_MODULE},
            diagnostics::Logger,
            lexer::{tokens::Token, LexerError},
            mir::{transform, MirProject, Traverser},
            parser::Parser,
            semantics::semanticnode::SemanticContext,
            CompilerDisplay, CompilerError, Lexer, SourceMap,
        },
        resolve_types, StringId, StringTable,
    };

    use super::LlvmFunctionTransformer;

    #[test]
    fn var_declaration() {
        let text = "
            fn test() {
                let x: i64 := 5;
                let b: bool := true;
                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn if_expr_empty() {
        compile_and_print_llvm(
            "
            fn test() {
                if (true) {} else {};
                return;
            }
        ",
        );
    }

    type LResult = std::result::Result<Vec<Token>, CompilerError<LexerError>>;

    fn compile_and_print_llvm(text: &str) {
        let (table, module) = compile(text);
        let mut project = MirProject::new();
        transform::transform(&module, &mut project).unwrap();

        let context = Context::create();
        let module = context.create_module("test");
        let builder = context.create_builder();
        let mut llvm =
            LlvmFunctionTransformer::new(StringId::new(), &context, &module, &builder, &table);

        let mut mvr = Traverser::new(&project, &mut llvm);

        // Traverser is given a MirProject
        // call traverser.map(llvm) this will use the llvm xfmr to map MirProject to LlvmProject
        mvr.map();
        // Print LLVM
        llvm.module.print_to_stderr();
    }

    fn compile(input: &str) -> (StringTable, Module<SemanticContext>) {
        let table = StringTable::new();
        let mut sm = SourceMap::new();
        sm.add_string(input, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let main = table.insert("main".into());
        let main_mod = table.insert(MAIN_MODULE.into());
        let main_fn = table.insert("my_main".into());

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();

        let parser = Parser::new(&logger);
        let ast = match parser.parse(main, &tokens) {
            Ok(ast) => ast.unwrap(),
            Err(err) => {
                panic!("{}", err.fmt(&sm, &table).unwrap());
            }
        };
        match resolve_types(&ast, main_mod, main_fn, &logger) {
            Ok(module) => (table, module),
            Err(err) => {
                panic!("{}", err.fmt(&sm, &table).unwrap());
            }
        }
    }
}
