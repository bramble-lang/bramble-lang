//! Transforms the MIR representation into LLVM

use std::collections::{hash_map::Entry, HashMap};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::{CodeModel, InitializationConfig, RelocMode},
    types::{AnyTypeEnum, BasicTypeEnum},
    values::*,
    OptimizationLevel,
};
use log::debug;

use crate::{
    compiler::{
        ast::Path,
        mir::{
            ir::*, DefId, FunctionTransformer, MirBaseType, MirTypeDef, ProgramTransformer,
            TransformerError, TypeId,
        },
        CompilerDisplay, SourceMap, Span,
    },
    StringTable,
};

use super::llvmir::LlvmToBasicTypeEnum;

/// Represents the final result of a Bramble program in LLVM IR.
struct LlvmProgram<'a, 'ctx> {
    /// LLVM Module
    module: &'a Module<'ctx>,
}

impl<'a, 'ctx> LlvmProgram<'a, 'ctx> {
    /// Print the LLVM IR to `stderr`.
    pub fn print_to_stderr(&self) {
        self.module.print_to_stderr()
    }

    /// Print the assembly representation of this Bramble program to `stdout`
    pub fn print_asm(&self) {
        // Get target for current machine
        let triple = inkwell::targets::TargetMachine::get_default_triple();

        let config = InitializationConfig::default();
        inkwell::targets::Target::initialize_all(&config);
        let target = inkwell::targets::Target::from_triple(&triple).unwrap();

        let machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                OptimizationLevel::None,
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or("Could not create a target machine for compilation")
            .unwrap();
        let data = machine.get_target_data();

        // Configure the module
        self.module.set_data_layout(&data.get_data_layout());
        self.module.set_triple(&triple);

        // If emit asm is true, then also write the assembly for the target machine to a file
        let path = std::path::Path::new("/tmp/output.s");
        machine
            .write_to_file(self.module, inkwell::targets::FileType::Assembly, &path)
            .unwrap();
        let contents =
            std::fs::read_to_string(&path).expect("Something went wrong reading the file");
        println!("{contents}");
    }
}

/// Transforms a complete program from MIR to LLVM IR.
struct LlvmProgramTransformer<'a, 'ctx> {
    /// LLVVM Context
    context: &'ctx Context,

    /// LLVM Module
    module: &'a Module<'ctx>,

    /// Used to construct actual LLVM instructions and add them to a function
    builder: &'a Builder<'ctx>,

    /// Table mapping the [`DefId`] used to identify a function in MIR to the
    /// [`FunctionValue`] used to identify a function in LLVM.
    fn_table: HashMap<DefId, FunctionValue<'ctx>>,

    /// Reference to the source map for the program being transformed to LLVM
    source_map: &'ctx SourceMap,

    /// Table mapping [`StringIds`](StringId) to the string value
    str_table: &'ctx StringTable,

    /// Table mapping [`TypeId`] to the LLVM IR associated type.
    ty_table: HashMap<TypeId, AnyTypeEnum<'ctx>>,
}

impl<'a, 'ctx> LlvmProgramTransformer<'a, 'ctx> {
    pub fn new(
        ctx: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
        source_map: &'ctx SourceMap,
        table: &'ctx StringTable,
    ) -> Self {
        debug!("Creating LLVM Program Transformer");

        Self {
            context: ctx,
            module,
            builder,
            fn_table: HashMap::new(),
            source_map,
            str_table: table,
            ty_table: HashMap::new(),
        }
    }

    /// Transforms this into the final [`LlvmProgram`] result, which can be used to
    /// actually generate the object code necessary for linking and final compilation.
    pub fn complete(self) -> LlvmProgram<'a, 'ctx> {
        LlvmProgram {
            module: self.module,
        }
    }

    fn to_label(&self, path: &Path) -> String {
        path.iter()
            .map(|element| element.fmt(self.source_map, self.str_table).unwrap())
            .collect::<Vec<_>>()
            .join("_")
    }
}

impl<'a, 'ctx, 'p>
    ProgramTransformer<
        'p,
        PointerValue<'ctx>,
        BasicValueEnum<'ctx>,
        LlvmFunctionTransformer<'a, 'ctx, 'p>,
    > for LlvmProgramTransformer<'a, 'ctx>
{
    fn add_function(
        &mut self,
        func_id: DefId,
        canonical_path: &Path,
    ) -> Result<(), TransformerError> {
        let name = self.to_label(canonical_path);

        debug!("Adding function to Module: {}", name);

        // Create a function to build
        let ft = self.context.void_type().fn_type(&[], false);
        let function = self.module.add_function(&name, ft, None);

        // Add function to function table
        match self.fn_table.insert(func_id, function) {
            Some(_) => Err(TransformerError::FunctionAlreadyDeclared),
            None => Ok(()),
        }
    }

    fn add_type(&mut self, id: TypeId, ty: &MirTypeDef) -> Result<(), TransformerError> {
        let r = match ty {
            MirTypeDef::Base(base) => match base {
                MirBaseType::I64 => self.ty_table.insert(id, self.context.i64_type().into()),
                MirBaseType::Bool => self.ty_table.insert(id, self.context.bool_type().into()),
                MirBaseType::Unit => self.ty_table.insert(id, self.context.void_type().into()),
                // TODO: Should Null this actually make it to MIR?
                MirBaseType::Null => None,
                MirBaseType::U8 => None,
                MirBaseType::U16 => None,
                MirBaseType::U32 => None,
                MirBaseType::U64 => None,
                MirBaseType::I8 => None,
                MirBaseType::I16 => None,
                MirBaseType::I32 => None,
                MirBaseType::F64 => None,
                MirBaseType::StringLiteral => None,
            },
            MirTypeDef::Array { ty, sz } => todo!(),
            MirTypeDef::RawPointer { mutable, target } => todo!(),
            MirTypeDef::Structure { path, def } => todo!(),
        };

        match r {
            None => Ok(()),
            Some(_) => Err(TransformerError::TypeAlreadyDefined),
        }
    }

    fn get_function_transformer(
        &'p self,
        id: DefId,
    ) -> std::result::Result<LlvmFunctionTransformer<'a, 'ctx, 'p>, TransformerError> {
        // Look up the FunctionValue associated with the given id
        let fv = self
            .fn_table
            .get(&id)
            .ok_or(TransformerError::FunctionNotFound)?;

        // Create a new fucntion transformer that will populate the assoicated function value
        Ok(LlvmFunctionTransformer::new(
            *fv,
            self.context,
            self.module,
            self.builder,
            self.str_table,
            self,
        ))
    }
}

struct LlvmFunctionTransformer<'a, 'ctx, 'p> {
    /// LLVVM Context
    context: &'ctx Context,

    /// LLVM Module
    module: &'a Module<'ctx>,

    /// Used to construct actual LLVM instructions and add them to a function
    builder: &'a Builder<'ctx>,

    hm: &'p LlvmProgramTransformer<'a, 'ctx>,

    /// Table mapping [`StringIds`](StringId) to the string value
    str_table: &'ctx StringTable,

    /// The LLVM function instance that is currently being built by the transformer
    /// all insructions will be added to this function.
    function: FunctionValue<'ctx>,

    /// Mapping of [`VarIds`](VarId) to their LLVM value, this is used to look up
    /// variables after they have been allocated.
    vars: HashMap<VarId, PointerValue<'ctx>>,

    /// Mapping of [`TempIds`](TempId) to their LLVM value, this is used to look up
    /// variables after they have been allocated.
    temps: HashMap<TempId, PointerValue<'ctx>>,

    /// Table to manage looking up the LLVM BasicBlock via the [`BasicBlockId`].
    blocks: HashMap<BasicBlockId, inkwell::basic_block::BasicBlock<'ctx>>,
}

impl<'a, 'ctx, 'p> LlvmFunctionTransformer<'a, 'ctx, 'p> {
    pub fn new(
        function: FunctionValue<'ctx>,
        ctx: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
        table: &'ctx StringTable,
        hm: &'p LlvmProgramTransformer<'a, 'ctx>,
    ) -> Self {
        debug!("Creating LLVM Function Transformer for function");

        Self {
            context: ctx,
            module,
            builder,
            str_table: table,
            function,
            hm,
            vars: HashMap::default(),
            temps: HashMap::default(),
            blocks: HashMap::new(),
        }
    }

    fn var_label(&self, vd: &VarDecl) -> String {
        let name = self.str_table.get(vd.name()).unwrap();
        let scope = vd.scope();
        format!("{}_{}", name, scope)
    }

    fn temp_label(&self, id: TempId) -> String {
        format!("_{}", id.index())
    }
}

impl<'a, 'ctx, 'p> FunctionTransformer<PointerValue<'ctx>, BasicValueEnum<'ctx>>
    for LlvmFunctionTransformer<'a, 'ctx, 'p>
{
    fn create_bb(&mut self, id: BasicBlockId) -> Result<(), TransformerError> {
        let bb = self
            .context
            .append_basic_block(self.function, &id.to_string());
        if self.blocks.insert(id, bb).is_none() {
            Ok(())
        } else {
            Err(TransformerError::BasicBlockAlreadyCreated)
        }
    }

    fn set_bb(&mut self, id: BasicBlockId) -> Result<(), TransformerError> {
        match self.blocks.get(&id) {
            Some(bb) => {
                self.builder.position_at_end(*bb);
                Ok(())
            }
            None => Err(TransformerError::BasicBlockNotFound),
        }
    }

    fn alloc_var(&mut self, id: VarId, decl: &VarDecl) -> Result<(), TransformerError> {
        let name = self.var_label(decl);

        // Check if variable name already exists
        match self.vars.entry(id) {
            // If it does -> return an error
            Entry::Occupied(_) => Err(TransformerError::VariableAlreadyAllocated),

            // If not, then allocate a pointer in the Builder
            Entry::Vacant(ve) => {
                // and add a mapping from VarID to the pointer in the local var table
                let ty = self.hm.ty_table.get(&decl.ty()).unwrap();
                let ptr = self
                    .builder
                    .build_alloca(ty.into_basic_type().unwrap(), &name);
                ve.insert(ptr);
                Ok(())
            }
        }
    }

    fn alloc_temp(&mut self, id: TempId, vd: &TempDecl) -> Result<(), TransformerError> {
        let name = self.temp_label(id);

        // Check if variable name already exists
        match self.temps.entry(id) {
            // If it does -> return an error
            Entry::Occupied(_) => Err(TransformerError::VariableAlreadyAllocated),

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

    fn var(&self, v: VarId) -> Result<PointerValue<'ctx>, TransformerError> {
        self.vars
            .get(&v)
            .copied()
            .ok_or(TransformerError::VarNotFound)
    }

    fn temp(&self, v: TempId) -> Result<PointerValue<'ctx>, TransformerError> {
        self.temps
            .get(&v)
            .copied()
            .ok_or(TransformerError::TempNotFound)
    }

    fn const_i64(&self, i: i64) -> BasicValueEnum<'ctx> {
        self.context.i64_type().const_int(i as u64, true).into()
    }

    fn const_bool(&self, b: bool) -> BasicValueEnum<'ctx> {
        let bt = self.context.bool_type();
        bt.const_int(b as u64, true).into()
    }

    fn load(&self, lv: PointerValue<'ctx>) -> BasicValueEnum<'ctx> {
        self.builder.build_load(lv, "")
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
            mir::{transform, MirProject, ProgramTraverser},
            parser::Parser,
            semantics::semanticnode::SemanticContext,
            CompilerDisplay, CompilerError, Lexer, SourceMap,
        },
        llvm::mir::LlvmProgramTransformer,
        resolve_types, StringTable,
    };

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
    fn if_expr() {
        compile_and_print_llvm(
            "
            fn test() {
                let x: i64 := if (true) {2} else {3};
                return;
            }
        ",
        );
    }

    #[test]
    fn two_functions() {
        compile_and_print_llvm(
            "
            fn foo() {
                let x: i64 := if (true) {2} else {3};
                return;
            }
           
            mod bats {
                fn bar() {
                    let x: i64 := 5;
                    let b: bool := true;
                    return;
                }
            }
        ",
        );
    }

    type LResult = std::result::Result<Vec<Token>, CompilerError<LexerError>>;

    fn compile_and_print_llvm(text: &str) {
        let (sm, table, module) = compile(text);
        let mut project = MirProject::new();
        transform::transform(&module, &mut project).unwrap();

        let context = Context::create();
        let module = context.create_module("test");
        let builder = context.create_builder();

        let mut xfmr = LlvmProgramTransformer::new(&context, &module, &builder, &sm, &table);

        let proj_traverser = ProgramTraverser::new(&project);

        // Traverser is given a MirProject
        // call traverser.map(llvm) this will use the llvm xfmr to map MirProject to LlvmProject
        proj_traverser.map(&mut xfmr);

        let llvm = xfmr.complete();

        // Print LLVM
        println!("=== LLVM IR ===:");
        llvm.print_to_stderr();

        println!("\n\n=== x86 ===");
        llvm.print_asm();
    }

    fn compile(input: &str) -> (SourceMap, StringTable, Module<SemanticContext>) {
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
            Ok(module) => (sm, table, module),
            Err(err) => {
                panic!("{}", err.fmt(&sm, &table).unwrap());
            }
        }
    }
}
