//! Transforms the MIR representation into LLVM

use std::collections::{hash_map::Entry, HashMap};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::{CodeModel, InitializationConfig, RelocMode},
    types::{AnyTypeEnum, BasicType, BasicTypeEnum},
    values::*,
    OptimizationLevel,
};
use log::debug;

use crate::{
    compiler::{
        ast::Path,
        mir::{
            ir::*, DefId, FunctionBuilder, MirBaseType, MirTypeDef, ProgramBuilder,
            TransformerError, TypeId,
        },
        CompilerDisplay, SourceMap, Span,
    },
    StringTable,
};

use super::llvmir::LlvmToBasicTypeEnum;

/// Represents the final result of a Bramble program in LLVM IR.
pub struct LlvmProgram<'module, 'ctx> {
    /// LLVM Module
    module: &'module Module<'ctx>,
}

impl<'module, 'ctx> LlvmProgram<'module, 'ctx> {
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

struct FunctionData<'ctx> {
    function: FunctionValue<'ctx>,
}

/// Transforms a complete program from MIR to LLVM IR.
pub struct LlvmProgramBuilder<'module, 'ctx> {
    /// LLVVM Context
    context: &'ctx Context,

    /// LLVM Module
    module: &'module Module<'ctx>,

    /// Used to construct actual LLVM instructions and add them to a function
    builder: &'module Builder<'ctx>,

    /// Table mapping the [`DefId`] used to identify a function in MIR to the
    /// [`FunctionValue`] used to identify a function in LLVM.
    fn_table: HashMap<DefId, FunctionData<'ctx>>,

    /// Reference to the source map for the program being transformed to LLVM
    source_map: &'ctx SourceMap,

    /// Table mapping [`StringIds`](StringId) to the string value
    str_table: &'ctx StringTable,

    /// Table mapping [`TypeId`] to the LLVM IR associated type.
    ty_table: HashMap<TypeId, AnyTypeEnum<'ctx>>,
}

impl<'module, 'ctx> LlvmProgramBuilder<'module, 'ctx> {
    pub fn new(
        ctx: &'ctx Context,
        module: &'module Module<'ctx>,
        builder: &'module Builder<'ctx>,
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
    pub fn complete(self) -> LlvmProgram<'module, 'ctx> {
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

    /// Given a [`TypeId`] this will return its associated LLVM [`AnyTypeEnum`] variant.
    /// If the [`TypeId`] has no associated LLVM type then an error is returned.
    fn get_type(&self, id: TypeId) -> Result<&AnyTypeEnum<'ctx>, TransformerError> {
        self.ty_table.get(&id).ok_or(TransformerError::TypeNotFound)
    }
}

impl<'p, 'module, 'ctx>
    ProgramBuilder<
        'p,
        PointerValue<'ctx>,
        BasicValueEnum<'ctx>,
        LlvmFunctionBuilder<'p, 'module, 'ctx>,
    > for LlvmProgramBuilder<'module, 'ctx>
{
    fn add_function(
        &mut self,
        func_id: DefId,
        canonical_path: &Path,
        args: &[ArgDecl],
    ) -> Result<(), TransformerError> {
        let name = self.to_label(canonical_path);

        debug!("Adding function to Module: {}", name);

        // Determine the channel for the return value
        // Set the return channel property for the function

        // If the functoin returns values via a Output Reference Parameter
        // then make that parameter the first parameter

        // Convert list of arguments into a list of LLVM types
        let llvm_args = args
            .iter()
            .map(|arg| arg.into_basic_type_enum(self))
            .collect::<Result<Vec<_>, _>>()?;

        // Create a function to build
        let ft = self.context.void_type().fn_type(&llvm_args, false);
        let function = self.module.add_function(&name, ft, None);

        // Add function to function table
        match self.fn_table.insert(func_id, FunctionData { function }) {
            Some(_) => Err(TransformerError::FunctionAlreadyDeclared),
            None => Ok(()),
        }
    }

    fn add_type(&mut self, id: TypeId, ty: &MirTypeDef) -> Result<(), TransformerError> {
        debug!("Adding a type to the Module");

        let previous_value = ty
            .into_basic_type_enum(self)
            .and_then(|llvm_ty| self.ty_table.insert(id, llvm_ty));

        // If `insert` returns `None` then it means there was no previous value associated with `id`
        // otherwise, `id` was already defined and this should thrown an error.
        match previous_value {
            None => Ok(()),
            Some(_) => Err(TransformerError::TypeAlreadyDefined),
        }
    }

    fn get_function_transformer(
        &'p self,
        id: DefId,
    ) -> std::result::Result<LlvmFunctionBuilder<'p, 'module, 'ctx>, TransformerError> {
        // Look up the FunctionValue associated with the given id
        let fv = self
            .fn_table
            .get(&id)
            .ok_or(TransformerError::FunctionNotFound)?;

        // Create a new fucntion transformer that will populate the assoicated function value
        Ok(LlvmFunctionBuilder::new(fv.function, self))
    }
}

impl ArgDecl {
    /// Convert the type of this [`ArgDecl`] into an LLVM [`BasicTypeEnum`].
    fn into_basic_type_enum<'module, 'ctx>(
        &self,
        p: &LlvmProgramBuilder<'module, 'ctx>,
    ) -> Result<BasicTypeEnum<'ctx>, TransformerError> {
        p.get_type(self.ty()).map(|ty| {
            ty.into_basic_type()
                .expect("Argument type must be a Basic Type")
        })
    }
}

impl MirTypeDef {
    fn into_basic_type_enum<'module, 'ctx>(
        &self,
        p: &LlvmProgramBuilder<'module, 'ctx>,
    ) -> Option<AnyTypeEnum<'ctx>> {
        match self {
            MirTypeDef::Base(base) => base.into_basic_type_enum(p.context),
            MirTypeDef::Array { ty, sz } => {
                let el_llvm_ty = p.get_type(*ty).unwrap();
                let len = *sz as u32;
                let bt = el_llvm_ty.into_basic_type().unwrap().as_basic_type_enum(); // I don't know why as_basic_type_enum has to be called but without it the array_type method doesn't work!
                Some(bt.array_type(len).into())
            }
            MirTypeDef::RawPointer { mutable, target } => todo!(),
            MirTypeDef::Structure { path, def } => todo!(),
        }
    }
}

impl MirBaseType {
    /// Convert into the corresponding LLVM type and then wrap that in an [`AnyTypeEnum`] variant.
    fn into_basic_type_enum<'ctx>(&self, context: &'ctx Context) -> Option<AnyTypeEnum<'ctx>> {
        match self {
            MirBaseType::U8 | MirBaseType::I8 => Some(context.i8_type().into()),
            MirBaseType::U16 | MirBaseType::I16 => Some(context.i16_type().into()),
            MirBaseType::U32 | MirBaseType::I32 => Some(context.i32_type().into()),
            MirBaseType::U64 | MirBaseType::I64 => Some(context.i64_type().into()),
            MirBaseType::F64 => Some(context.f64_type().into()),
            MirBaseType::Bool => Some(context.bool_type().into()),
            MirBaseType::Unit => Some(context.void_type().into()),
            // TODO: Should Null this actually make it to MIR?
            MirBaseType::Null => None,
            MirBaseType::StringLiteral => None,
        }
    }
}

pub struct LlvmFunctionBuilder<'p, 'module, 'ctx> {
    program: &'p LlvmProgramBuilder<'module, 'ctx>,

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

impl<'p, 'module, 'ctx> LlvmFunctionBuilder<'p, 'module, 'ctx> {
    pub fn new(
        function: FunctionValue<'ctx>,
        program: &'p LlvmProgramBuilder<'module, 'ctx>,
    ) -> Self {
        debug!("Creating LLVM Function Transformer for function");

        Self {
            function,
            program,
            vars: HashMap::default(),
            temps: HashMap::default(),
            blocks: HashMap::new(),
        }
    }

    fn var_label(&self, vd: &VarDecl) -> String {
        let name = self.program.str_table.get(vd.name()).unwrap();
        let scope = vd.scope();
        format!("{}_{}", name, scope)
    }

    fn temp_label(&self, id: TempId) -> String {
        format!("_{}", id.index())
    }
}

impl<'p, 'module, 'ctx> FunctionBuilder<PointerValue<'ctx>, BasicValueEnum<'ctx>>
    for LlvmFunctionBuilder<'p, 'module, 'ctx>
{
    fn create_bb(&mut self, id: BasicBlockId) -> Result<(), TransformerError> {
        let bb = self
            .program
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
                self.program.builder.position_at_end(*bb);
                Ok(())
            }
            None => Err(TransformerError::BasicBlockNotFound),
        }
    }

    fn store_arg(&mut self, arg_id: ArgId, var_id: VarId) -> Result<(), TransformerError> {
        // Get the argument value
        let arg_value = self
            .function
            .get_nth_param(arg_id.to_u32())
            .ok_or(TransformerError::ArgNotFound)?;

        // Get the location in the stack where the argument will be stored
        let stack_location = self
            .vars
            .get(&var_id)
            .ok_or(TransformerError::VarNotFound)?;

        // Move the argument into the stack
        self.program.builder.build_store(*stack_location, arg_value);
        Ok(())
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
                let ty = self.program.get_type(decl.ty())?;
                let ptr = self
                    .program
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
                let ty = self.program.context.i64_type();
                let ptr = self.program.builder.build_alloca(ty, &name);
                ve.insert(ptr);
                Ok(())
            }
        }
    }

    fn term_return(&mut self) {
        self.program.builder.build_return(None);
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
        self.program
            .builder
            .build_conditional_branch(cond.into_int_value(), *then_bb, *else_bb);
    }

    fn term_goto(&mut self, target: BasicBlockId) {
        let target = self.blocks.get(&target).unwrap();
        self.program.builder.build_unconditional_branch(*target);
    }

    fn assign(&mut self, span: Span, l: PointerValue<'ctx>, v: BasicValueEnum<'ctx>) {
        self.program.builder.build_store(l, v);
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

    fn const_i8(&self, i: i8) -> BasicValueEnum<'ctx> {
        self.program
            .context
            .i8_type()
            .const_int(i as u64, true)
            .into()
    }

    fn const_i16(&self, i: i16) -> BasicValueEnum<'ctx> {
        self.program
            .context
            .i16_type()
            .const_int(i as u64, true)
            .into()
    }

    fn const_i32(&self, i: i32) -> BasicValueEnum<'ctx> {
        self.program
            .context
            .i32_type()
            .const_int(i as u64, true)
            .into()
    }

    fn const_i64(&self, i: i64) -> BasicValueEnum<'ctx> {
        self.program
            .context
            .i64_type()
            .const_int(i as u64, true)
            .into()
    }

    fn const_u8(&self, i: u8) -> BasicValueEnum<'ctx> {
        self.program
            .context
            .i8_type()
            .const_int(i as u64, false)
            .into()
    }

    fn const_u16(&self, i: u16) -> BasicValueEnum<'ctx> {
        self.program
            .context
            .i16_type()
            .const_int(i as u64, false)
            .into()
    }

    fn const_u32(&self, i: u32) -> BasicValueEnum<'ctx> {
        self.program
            .context
            .i32_type()
            .const_int(i as u64, false)
            .into()
    }

    fn const_u64(&self, i: u64) -> BasicValueEnum<'ctx> {
        self.program
            .context
            .i64_type()
            .const_int(i as u64, false)
            .into()
    }

    fn const_bool(&self, b: bool) -> BasicValueEnum<'ctx> {
        let bt = self.program.context.bool_type();
        bt.const_int(b as u64, true).into()
    }

    fn const_f64(&self, f: f64) -> BasicValueEnum<'ctx> {
        self.program.context.f64_type().const_float(f).into()
    }

    fn load(&self, lv: PointerValue<'ctx>) -> BasicValueEnum<'ctx> {
        self.program.builder.build_load(lv, "")
    }

    fn add(&self, a: BasicValueEnum<'ctx>, b: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        todo!()
    }

    fn sub(&self, a: BasicValueEnum<'ctx>, b: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        todo!()
    }
}
