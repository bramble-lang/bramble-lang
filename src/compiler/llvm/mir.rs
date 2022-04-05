//! Transforms the MIR representation into LLVM

use std::collections::{hash_map::Entry, HashMap};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::{CodeModel, InitializationConfig, RelocMode},
    types::{AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::*,
    AddressSpace, OptimizationLevel,
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

use super::llvmir::{get_ptr_alignment, LlvmIsAggregateType, LlvmToBasicTypeEnum};

const ADDRESS_SPACE: AddressSpace = AddressSpace::Generic;

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

/// Groups the data which describes an LLVM function together.
#[derive(Clone, Copy)]
struct FunctionData<'ctx> {
    /// A reference to the function within the LLVM module.
    function: FunctionValue<'ctx>,

    /// Describes how values are returned from the funciton to the caller.
    ret_method: ReturnMethod,
}

/// Specifies the method that will be used to pass the result of this
/// function back to its caller.
#[derive(Clone, Copy)]
enum ReturnMethod {
    /// An extra "out" parameter is added to this function's parameters that
    /// contains a pointer to a location in the caller's stack. The function's
    /// result will be written to that location.
    ///
    /// This method should be used for aggregate types such as structures or
    /// arrays.
    OutParam,

    /// This will use LLVM's return operator to pass the result back to the
    /// caller function, via the platform appropriate method.
    Return,
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

    /// Given a type, determine how a value of that type is returned from a function to
    /// its caller.
    fn determine_ret_method(
        &self,
        ty: TypeId,
    ) -> Result<(ReturnMethod, AnyTypeEnum<'ctx>), TransformerError> {
        let llvm_ty = self.get_type(ty)?;
        let method = match llvm_ty {
            AnyTypeEnum::PointerType(_) | AnyTypeEnum::FloatType(_) | AnyTypeEnum::IntType(_) => {
                (ReturnMethod::Return, *llvm_ty)
            }
            AnyTypeEnum::ArrayType(a_ty) => {
                (ReturnMethod::OutParam, a_ty.ptr_type(ADDRESS_SPACE).into())
            }
            AnyTypeEnum::FunctionType(_) => todo!(),
            AnyTypeEnum::StructType(_) => todo!(),
            AnyTypeEnum::VectorType(_) => todo!(),
            AnyTypeEnum::VoidType(_) => (ReturnMethod::Return, self.context.void_type().into()),
        };
        Ok(method)
    }

    fn fn_type(
        &self,
        args: &[ArgDecl],
        ret_ty: TypeId,
    ) -> Result<(FunctionType<'ctx>, ReturnMethod), TransformerError> {
        // Determine the channel for the return value
        // Set the return channel property for the function
        let (ret_method, llvm_ret_ty) = self.determine_ret_method(ret_ty)?;

        // If the functoin returns values via a Output Reference Parameter
        // then make that parameter the first parameter

        // Create a function to build
        let ft = match ret_method {
            ReturnMethod::OutParam => {
                // prepend the out parameter to the args list
                let basic_ty = llvm_ret_ty.into_basic_type().unwrap();
                let mut llvm_args = vec![basic_ty];
                for a in args {
                    let llvm_ty = a.into_arg_type(self)?;
                    llvm_args.push(llvm_ty);
                }

                self.context.void_type().fn_type(&llvm_args, false)
            }
            ReturnMethod::Return => {
                // Convert list of arguments into a list of LLVM types
                let llvm_args = args
                    .iter()
                    .map(|arg| arg.into_arg_type(self))
                    .collect::<Result<Vec<_>, _>>()?;

                match llvm_ret_ty {
                    AnyTypeEnum::IntType(it) => it.fn_type(&llvm_args, false),
                    AnyTypeEnum::VoidType(vt) => vt.fn_type(&llvm_args, false),
                    AnyTypeEnum::FloatType(ft) => ft.fn_type(&llvm_args, false),
                    AnyTypeEnum::PointerType(pt) => pt.fn_type(&llvm_args, false),
                    AnyTypeEnum::FunctionType(_) => todo!(),
                    AnyTypeEnum::VectorType(_) => todo!(),
                    AnyTypeEnum::ArrayType(_) => {
                        panic!("Returning array values need to use the out parameter method.")
                    }
                    AnyTypeEnum::StructType(_) => {
                        panic!("Returning structure value need to use the out parameter method.")
                    }
                }
            }
        };

        Ok((ft, ret_method))
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
        ret_ty: TypeId,
    ) -> Result<(), TransformerError> {
        let name = self.to_label(canonical_path);

        debug!("Adding function to Module: {}", name);

        // Determine the channel for the return value
        // Set the return channel property for the function
        let (fn_type, ret_method) = self.fn_type(args, ret_ty)?;

        let function = self.module.add_function(&name, fn_type, None);

        // Add function to function table
        let function = FunctionData {
            ret_method,
            function,
        };

        match self.fn_table.insert(func_id, function) {
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
        Ok(LlvmFunctionBuilder::new(*fv, self))
    }
}

impl ArgDecl {
    /// Convert the type of this [`ArgDecl`] into an LLVM [`BasicTypeEnum`].
    fn into_arg_type<'module, 'ctx>(
        &self,
        p: &LlvmProgramBuilder<'module, 'ctx>,
    ) -> Result<BasicTypeEnum<'ctx>, TransformerError> {
        p.get_type(self.ty())
            .map(|ty| {
                ty.into_basic_type()
                    .expect("Argument type must be a Basic Type")
            })
            .map(|ty| {
                if ty.is_aggregate_type() {
                    ty.ptr_type(ADDRESS_SPACE).into()
                } else {
                    ty
                }
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
    function: FunctionData<'ctx>,

    /// Channel used to return the result back to the caller
    ret_ptr: ReturnPointer<'ctx>,

    /// Mapping of [`VarIds`](VarId) to their LLVM value, this is used to look up
    /// variables after they have been allocated.
    vars: HashMap<VarId, PointerValue<'ctx>>,

    /// Mapping of [`TempIds`](TempId) to their LLVM value, this is used to look up
    /// variables after they have been allocated.
    temps: HashMap<TempId, PointerValue<'ctx>>,

    /// Table to manage looking up the LLVM BasicBlock via the [`BasicBlockId`].
    /// Some [`BasicBlockIds`](BasicBlockId) may map to the same
    /// [`inkwell::BasicBlock`](inkwell::basic_block::BasicBlock). This is because
    /// the LLVM Builder may decide that two adjacent MIR BasicBlocks need to be
    /// merged into a single LLVM BasicBlock in order to maintain idiomatic LLVM
    /// code.
    blocks: HashMap<BasicBlockId, inkwell::basic_block::BasicBlock<'ctx>>,
}

enum ReturnPointer<'ctx> {
    /// This function returns no value
    Unit,

    /// This function returns this primitive value and will use the LLVM return
    /// operator to return the value back using the platform appropriate methods.
    ///
    /// This variant is initialized to [`None`] because the actual value to use
    /// is not known until the the function has been converted from MIR to LLVM.
    Value(Option<BasicValueEnum<'ctx>>),

    /// This function uses an out paramter which contains an address to a location in
    /// the calling function's stack to return a value.
    OutParam(PointerValue<'ctx>),
}

impl<'p, 'module, 'ctx> LlvmFunctionBuilder<'p, 'module, 'ctx> {
    fn new(function: FunctionData<'ctx>, program: &'p LlvmProgramBuilder<'module, 'ctx>) -> Self {
        debug!("Creating LLVM Function Transformer for function");
        let ret_ptr = match function.ret_method {
            ReturnMethod::OutParam => {
                let out_ptr = Self::get_out_param(&function).unwrap();
                ReturnPointer::OutParam(out_ptr)
            }
            ReturnMethod::Return => match function.function.get_type().get_return_type() {
                None => ReturnPointer::Unit,
                Some(_) => ReturnPointer::Value(None),
            },
        };

        Self {
            function,
            program,
            ret_ptr,
            vars: HashMap::default(),
            temps: HashMap::default(),
            blocks: HashMap::new(),
        }
    }

    fn get_out_param(f: &FunctionData<'ctx>) -> Option<PointerValue<'ctx>> {
        match f.ret_method {
            ReturnMethod::OutParam => {
                Some(f.function.get_nth_param(0).unwrap().into_pointer_value())
            }
            ReturnMethod::Return => None,
        }
    }

    fn get_arg(&self, id: ArgId) -> Result<BasicValueEnum, TransformerError> {
        // If this function is using an out parameter to return a value to the caller then
        // the `ArgId` index will be off by one, because the out parameter will be pushed
        // to the head of the parameter list, shifting all the user defined parameter down
        // by 1.
        let arg_offset = match self.function.ret_method {
            ReturnMethod::OutParam => 1,
            ReturnMethod::Return => 0,
        };

        self.function
            .function
            .get_nth_param(id.to_u32() - arg_offset)
            .ok_or(TransformerError::ArgNotFound)
    }

    fn var_label(&self, vd: &VarDecl) -> String {
        let name = self.program.str_table.get(vd.name()).unwrap();
        let scope = vd.scope();
        format!("{}_{}", name, scope)
    }

    fn temp_label(&self, id: TempId) -> String {
        format!("_{}", id.index())
    }

    fn build_memcpy(&mut self, dest: PointerValue<'ctx>, src: PointerValue<'ctx>, span: Span) {
        let dest_align = get_ptr_alignment(dest);
        let src_align = get_ptr_alignment(src);
        self.program
            .builder
            .build_memcpy(
                dest,
                dest_align,
                src,
                src_align,
                dest.get_type().get_element_type().size_of().unwrap(),
            )
            .unwrap();
    }
}

impl<'p, 'module, 'ctx> FunctionBuilder<PointerValue<'ctx>, BasicValueEnum<'ctx>>
    for LlvmFunctionBuilder<'p, 'module, 'ctx>
{
    fn create_bb(&mut self, id: BasicBlockId) -> Result<(), TransformerError> {
        let bb = self
            .program
            .context
            .append_basic_block(self.function.function, &id.to_string());
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
        let arg_value = self.get_arg(arg_id)?;

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
        match self.ret_ptr {
            ReturnPointer::Unit => self.program.builder.build_return(None),
            ReturnPointer::Value(v) => {
                let val = v.unwrap();
                self.program.builder.build_return(Some(&val))
            }
            ReturnPointer::OutParam(_) => self.program.builder.build_return(None),
        };
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

    fn term_fn_call(
        &mut self,
        target: DefId,
        args: &[BasicValueEnum<'ctx>],
        reentry: (PointerValue<'ctx>, BasicBlockId),
    ) {
    }

    fn term_goto(&mut self, target: BasicBlockId) {
        let target = self.blocks.get(&target).unwrap();
        self.program.builder.build_unconditional_branch(*target);
    }

    fn store(&mut self, span: Span, l: &LValue, r: BasicValueEnum<'ctx>) {
        match l {
            LValue::Static(_) => todo!(),
            LValue::Var(id) => {
                let var = self.var(*id).unwrap();
                self.program.builder.build_store(var, r);
            }
            LValue::Temp(id) => {
                let temp = self.temp(*id).unwrap();
                self.program.builder.build_store(temp, r);
            }
            LValue::Access(_, _) => todo!(),
            LValue::ReturnPointer => match &mut self.ret_ptr {
                ReturnPointer::Unit => panic!("Attempting to return a value on a Unit function"),
                ReturnPointer::Value(val) => *val = Some(r),
                ReturnPointer::OutParam(out_ptr) => {
                    let dest = *out_ptr;
                    self.build_memcpy(dest, r.into_pointer_value(), span)
                }
            },
        }
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
