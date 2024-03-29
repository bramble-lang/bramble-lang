//! Transforms the MIR representation into LLVM

use std::collections::{hash_map::Entry, HashMap, VecDeque};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::{CodeModel, InitializationConfig, RelocMode},
    types::{AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::*,
    AddressSpace, IntPredicate, OptimizationLevel,
};
use log::debug;

use crate::{
    compiler::{
        ast::Path,
        mir::{
            ir::*, DefId, FieldId, FunctionBuilder, MirBaseType, MirStructDef, MirTypeDef,
            ProgramBuilder, TransformerError, TransformerInternalError, TypeId,
        },
        CompilerDisplay, SourceMap, Span,
    },
    StringId, StringTable,
};

use super::llvmir::{get_ptr_alignment, LlvmIsAggregateType, LlvmToBasicTypeEnum};

/// Use the [`Generic`](AddressSpace::Generic) address space for all memory operations.
/// This is done because this seems to be the safest choice and because I cannot find
/// much documentation about what I should be using.
const ADDRESS_SPACE: AddressSpace = AddressSpace::Generic;

/// For functions that take an output parameter, the parameter is always placed as the
/// first parameter in the functions parameter list. We use the first parameter rather
/// than placing the Out Parameter as the last parameter, because if a function is
/// variadic it becomes impossible to deterministically determine which argument is
/// the output parameter. While Bramble does not support variadic functions, as of
/// this time, this makes it easier to reason about how the out parameter affects
/// the compilation of a program.
const OUT_PARAM_INDEX: u32 = 0;

/// Errors that are specific to building LLVM IR from Bramble MIR.
#[derive(Debug, Clone, Copy)]
pub enum LlvmBuilderError {
    CoerceVoidLocationIntoPointer,
    CoerceVoidLocationIntoFunction,
    CoerceFnLocationIntoPointer,
    CoercePtrLocationIntoFn,
    CoerceRetPtrIntoPtr,
    CoerceRetPtrIntoFn,
    CoerceValueIntoPointer,
    CoerceValueIntoFn,
    ReadInvalidLocation,
    InvalidArithmeticOperands,
    CannotConvertToBasicType,
    InvalidCastingOperation,
    InvalidOperand,
}

impl TransformerInternalError for LlvmBuilderError {}

/// Represents an LLVM value which represents an address somewhere in memory. This
/// includes [`pointers`](PointerValue) and [`function labels`](FunctionValue).
#[derive(PartialEq, Clone, Copy)]
pub enum Location<'ctx> {
    Pointer(PointerValue<'ctx>),
    Function(FunctionData<'ctx>),
    Argument(BasicValueEnum<'ctx>),
    ReturnPointer,
    Void,
}

impl<'ctx> Location<'ctx> {
    /// Will attempt to turn this value into a [`PointerValue`]. If this is a
    /// [`Location::Function`] variant then this will return an error.
    fn into_pointer(&self) -> Result<PointerValue<'ctx>, TransformerError> {
        match self {
            Location::Pointer(p) => Ok(*p),
            Location::Function(_) => Err(&LlvmBuilderError::CoerceFnLocationIntoPointer),
            Location::ReturnPointer => Err(&LlvmBuilderError::CoerceRetPtrIntoPtr),
            Location::Void => Err(&LlvmBuilderError::CoerceVoidLocationIntoPointer),
            Location::Argument(arg) => match arg {
                BasicValueEnum::PointerValue(ptr) => Ok(*ptr),
                _ => Err(&LlvmBuilderError::CoerceValueIntoPointer),
            },
        }
        .map_err(|e| TransformerError::Internal(e))
    }

    /// Will attempt to turn this value into a [`FunctionData`] value. If this is a
    /// [`Location::Pointer`] variant then this will return an error.
    fn into_function(&self) -> Result<FunctionData<'ctx>, TransformerError> {
        match self {
            Location::Pointer(_) => Err(&LlvmBuilderError::CoercePtrLocationIntoFn),
            Location::Function(f) => Ok(*f),
            Location::ReturnPointer => Err(&LlvmBuilderError::CoerceRetPtrIntoFn),
            Location::Void => Err(&LlvmBuilderError::CoerceVoidLocationIntoFunction),
            Location::Argument(_) => Err(&LlvmBuilderError::CoerceValueIntoFn),
        }
        .map_err(|e| TransformerError::Internal(e))
    }
}

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

    pub fn emit_object_code(&self, emit_asm: Option<&std::path::Path>, file: &std::path::Path) {
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
        if let Some(asm_file) = emit_asm {
            machine
                .write_to_file(self.module, inkwell::targets::FileType::Assembly, asm_file)
                .unwrap();
        }
        machine
            .write_to_file(self.module, inkwell::targets::FileType::Object, file)
            .unwrap();
    }

    pub fn emit_llvm_ir(&self, file: &std::path::Path) -> Result<(), inkwell::support::LLVMString> {
        self.module.print_to_file(file)
    }
}

/// Groups the data which describes an LLVM function together.
#[derive(PartialEq, Clone, Copy)]
pub struct FunctionData<'ctx> {
    /// A reference to the function within the LLVM module.
    function: FunctionValue<'ctx>,

    /// Describes how values are returned from the funciton to the caller.
    ret_method: ReturnMethod,

    /// This is true if the function has the special main function name.
    is_main: bool,
}

/// Specifies the method that will be used to pass the result of this
/// function back to its caller.
#[derive(PartialEq, Clone, Copy)]
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

    /// Defines the special name that is reserved for the main function
    main_name: StringId,
}

impl<'module, 'ctx> LlvmProgramBuilder<'module, 'ctx> {
    pub fn new(
        ctx: &'ctx Context,
        module: &'module Module<'ctx>,
        builder: &'module Builder<'ctx>,
        source_map: &'ctx SourceMap,
        table: &'ctx StringTable,
        main_name: StringId,
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
            main_name,
        }
    }

    /// Transforms this into the final [`LlvmProgram`] result, which can be used to
    /// actually generate the object code necessary for linking and final compilation.
    pub fn complete(mut self) -> LlvmProgram<'module, 'ctx> {
        match self.find_user_main().unwrap() {
            Some(user_main) => {
                let fv = user_main.function;
                self.construct_main(fv);
            }
            None => (),
        }

        LlvmProgram {
            module: self.module,
        }
    }

    /// Constructs the platform main function which will call the users defined `my_main`
    pub fn construct_main(&mut self, user_main: FunctionValue<'ctx>) {
        let main_type = self.context.i64_type().fn_type(&[], false);
        let main = self.module.add_function("main", main_type, None);
        let entry_bb = self.context.append_basic_block(main, "entry");
        self.builder.position_at_end(entry_bb);

        let status = self
            .builder
            .build_call(user_main, &[], "user_main")
            .try_as_basic_value()
            .left()
            .unwrap();
        self.builder.build_return(Some(&status));
    }

    fn find_user_main(&self) -> Result<Option<&FunctionData<'ctx>>, ()> {
        let mut mains = self
            .fn_table
            .iter()
            .filter(|(_, f)| f.is_main)
            .map(|(_, f)| f);
        match mains.next() {
            Some(m) => match mains.next() {
                None => Ok(Some(m)),
                Some(_) => Err(()),
            },
            None => Ok(None),
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
        self.ty_table
            .get(&id)
            .ok_or_else(|| TransformerError::TypeNotFound(id))
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
            AnyTypeEnum::FunctionType(_) => panic!("Cannot return a FunctionType"),
            AnyTypeEnum::StructType(s_ty) => {
                (ReturnMethod::OutParam, s_ty.ptr_type(ADDRESS_SPACE).into())
            }
            AnyTypeEnum::VectorType(_) => panic!("Cannot return a Vector Type"),
            AnyTypeEnum::VoidType(_) => (ReturnMethod::Return, self.context.void_type().into()),
        };
        Ok(method)
    }

    fn fn_type(
        &self,
        args: &[ArgDecl],
        is_variadic: bool,
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

                self.context.void_type().fn_type(&llvm_args, is_variadic)
            }
            ReturnMethod::Return => {
                // Convert list of arguments into a list of LLVM types
                let llvm_args = args
                    .iter()
                    .map(|arg| arg.into_arg_type(self))
                    .collect::<Result<Vec<_>, _>>()?;

                match llvm_ret_ty {
                    AnyTypeEnum::IntType(it) => it.fn_type(&llvm_args, is_variadic),
                    AnyTypeEnum::VoidType(vt) => vt.fn_type(&llvm_args, is_variadic),
                    AnyTypeEnum::FloatType(ft) => ft.fn_type(&llvm_args, is_variadic),
                    AnyTypeEnum::PointerType(pt) => pt.fn_type(&llvm_args, is_variadic),
                    AnyTypeEnum::FunctionType(_) => panic!("Cannot return a FunctionType"),
                    AnyTypeEnum::VectorType(_) => panic!("Cannot return a VectorType"),
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

    /// Returns true if this function has the name reserved for the user defined main function
    fn is_main_function(&self, path: &Path) -> bool {
        path.item().filter(|item| *item == self.main_name).is_some()
    }
}

impl<'p, 'module, 'ctx>
    ProgramBuilder<'p, Location<'ctx>, BasicValueEnum<'ctx>, LlvmFunctionBuilder<'p, 'module, 'ctx>>
    for LlvmProgramBuilder<'module, 'ctx>
{
    fn add_function(
        &mut self,
        func_id: DefId,
        canonical_path: &Path,
        args: &[ArgDecl],
        is_variadic: bool,
        ret_ty: TypeId,
    ) -> Result<(), TransformerError> {
        let name = self.to_label(canonical_path);

        debug!(
            "Adding function to Module: {} (var_args: {})",
            name, is_variadic
        );

        // Determine the channel for the return value
        // Set the return channel property for the function
        let (fn_type, ret_method) = self.fn_type(args, is_variadic, ret_ty)?;

        let function = self.module.add_function(&name, fn_type, None);

        // Add function to function table
        let function = FunctionData {
            ret_method,
            function,
            is_main: self.is_main_function(canonical_path),
        };

        match self.fn_table.insert(func_id, function) {
            Some(_) => Err(TransformerError::FunctionAlreadyDeclared),
            None => Ok(()),
        }
    }

    fn declare_struct(&mut self, id: TypeId, path: &Path) -> Result<(), TransformerError> {
        let label = self.to_label(path);
        let struct_decl = self.context.opaque_struct_type(&label);
        match self.ty_table.insert(id, struct_decl.into()) {
            Some(_) => Err(TransformerError::TypeAlreadyDefined),
            None => Ok(()),
        }
    }

    fn add_type(&mut self, id: TypeId, ty: &MirTypeDef) -> Result<(), TransformerError> {
        debug!("Adding a type to the Module");

        // Do not attempt to add the Null type to the LLVM IR table
        match ty {
            MirTypeDef::Base(MirBaseType::Null) => return Ok(()),
            _ => (),
        }

        // If type is already in the table then skip
        if self.ty_table.contains_key(&id) {
            // If this is a structure, then turn the declaration into a definition
            match ty {
                MirTypeDef::Structure { def, .. } => match def {
                    MirStructDef::Defined(fields) => {
                        // Get the structure declaration
                        let s = self.ty_table.get_mut(&id).unwrap().into_struct_type();

                        // Add fields to structure definition
                        let field_types: Vec<_> = fields
                            .iter()
                            .map(|f| {
                                self.get_type(f.ty)?.into_basic_type().map_err(|_| {
                                    TransformerError::Internal(
                                        &LlvmBuilderError::CannotConvertToBasicType,
                                    )
                                })
                            })
                            .collect::<Result<_, _>>()?;
                        s.set_body(&field_types, false);
                        Ok(())
                    }
                    MirStructDef::Declared => Err(TransformerError::StructUndefined),
                },
                MirTypeDef::Base(_) | MirTypeDef::Array { .. } | MirTypeDef::RawPointer { .. } => {
                    Err(TransformerError::TypeAlreadyDefined)
                }
            }
        } else {
            let llvm_ty = ty.into_basic_type_enum(self);
            self.ty_table.insert(id, llvm_ty);
            Ok(())
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
    ) -> AnyTypeEnum<'ctx> {
        match self {
            MirTypeDef::Base(base) => base.into_basic_type_enum(p.context),
            MirTypeDef::Array { ty, sz } => {
                let el_llvm_ty = p.get_type(*ty).unwrap();
                let len = *sz as u32;
                let bt = el_llvm_ty.into_basic_type().unwrap().as_basic_type_enum(); // I don't know why as_basic_type_enum has to be called but without it the array_type method doesn't work!
                bt.array_type(len).into()
            }
            MirTypeDef::RawPointer { target, .. } => {
                let ty = p.get_type(*target).unwrap().into_basic_type().unwrap();
                ty.ptr_type(ADDRESS_SPACE).into()
            }
            MirTypeDef::Structure {
                path,
                def: MirStructDef::Defined(fields),
            } => {
                // convert path into a label
                let label = p.to_label(path);
                // Check that structure is defined
                // Create custom type in LLVM
                let struct_ty = p.context.opaque_struct_type(&label);

                // add fields
                let fields: Vec<_> = fields
                    .iter()
                    .map(|f| {
                        p.get_type(f.ty)
                            .unwrap_or_else(|e| {
                                // This panics because by this time, every type that is referenced in this structure
                                // should already be added (or declared).  So, this means that a bug exists either in
                                // this module or in the MIR Traverser
                                panic!("Cannot find given Type ID {:?} in\n{:?}", e, p.ty_table)
                            })
                            .into_basic_type()
                            .expect("Cannot convert to a basic type")
                    })
                    .collect();

                struct_ty.set_body(&fields, false);
                struct_ty.into()
            }
            MirTypeDef::Structure { .. } => {
                panic!("Attempting to add a structure which has not been defined")
            }
        }
    }
}

impl MirBaseType {
    /// Convert into the corresponding LLVM type and then wrap that in an [`AnyTypeEnum`] variant.
    fn into_basic_type_enum<'ctx>(&self, context: &'ctx Context) -> AnyTypeEnum<'ctx> {
        match self {
            MirBaseType::U8 | MirBaseType::I8 => context.i8_type().into(),
            MirBaseType::U16 | MirBaseType::I16 => context.i16_type().into(),
            MirBaseType::U32 | MirBaseType::I32 => context.i32_type().into(),
            MirBaseType::U64 | MirBaseType::I64 => context.i64_type().into(),
            MirBaseType::F64 => context.f64_type().into(),
            MirBaseType::Bool => context.bool_type().into(),
            MirBaseType::Unit => context.void_type().into(),
            MirBaseType::StringLiteral => context
                .i8_type()
                .array_type(0)
                .ptr_type(AddressSpace::Generic)
                .into(),
            // TODO: Should Null this actually make it to MIR?
            MirBaseType::Null => panic!("Attempting to use Null as a type"),
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
    vars: HashMap<VarId, Location<'ctx>>,

    /// Mapping of [`TempIds`](TempId) to their LLVM value, this is used to look up
    /// variables after they have been allocated.
    temps: HashMap<TempId, Location<'ctx>>,

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
    OutParam(Location<'ctx>),
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
            blocks: HashMap::default(),
        }
    }

    fn get_out_param(f: &FunctionData<'ctx>) -> Option<Location<'ctx>> {
        match f.ret_method {
            ReturnMethod::OutParam => Some(Location::Pointer(
                f.function
                    .get_nth_param(OUT_PARAM_INDEX)
                    .unwrap()
                    .into_pointer_value(),
            )),
            ReturnMethod::Return => None,
        }
    }

    fn get_arg(&self, id: ArgId) -> Result<BasicValueEnum<'ctx>, TransformerError> {
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
            .get_nth_param(id.to_u32() + arg_offset)
            .ok_or(TransformerError::ArgNotFound)
    }

    fn arg_label(&self, ad: &ArgDecl) -> String {
        self.program.str_table.get(ad.name()).unwrap()
    }

    fn var_label(&self, vd: &VarDecl) -> String {
        let name = self.program.str_table.get(vd.name()).unwrap();
        let scope = vd.scope();
        format!("{}_{}", name, scope)
    }

    fn temp_label(&self, id: TempId) -> String {
        format!("_{}", id.index())
    }

    /// Convert the ID of a string to the name of the global variable that
    /// references that string
    fn create_stringpool_label(&self, id: StringId) -> String {
        format!(
            "str_{}_{}",
            self.program
                .module
                .get_name()
                .to_str()
                .expect("Expected a valid UTF string for the Module name"),
            id
        )
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

    fn build_ptr_compare(
        &self,
        op: IntPredicate,
        l: PointerValue<'ctx>,
        r: PointerValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let i64ty = self.program.context.i64_type();
        let li = self.program.builder.build_ptr_to_int(l, i64ty, "");
        let ri = self.program.builder.build_ptr_to_int(r, i64ty, "");

        let op = self
            .program
            .builder
            .build_int_compare(op, li, ri, "")
            .into();
        op
    }
}

impl<'p, 'module, 'ctx> FunctionBuilder<Location<'ctx>, BasicValueEnum<'ctx>>
    for LlvmFunctionBuilder<'p, 'module, 'ctx>
{
    fn create_bb(&mut self, id: BasicBlockId, bb: &BasicBlock) -> Result<(), TransformerError> {
        let llvm_bb = match self.blocks.entry(id) {
            Entry::Occupied(occ) => *occ.get(),
            Entry::Vacant(entry) => {
                let llvm_bb = self
                    .program
                    .context
                    .append_basic_block(self.function.function, &id.to_string());
                entry.insert(llvm_bb);
                llvm_bb
            }
        };

        // If this basic block ends with a CallFn then set the reentry BB to point to this same BB
        // LLVM IR does not treat function calls as terminators, so we should merge the block succeeding
        // a function call with the block making a function call when lowering into LLVM.
        match bb.get_term().map(|term| term.kind()) {
            Some(kind) => match kind {
                TerminatorKind::CallFn { reentry, .. } => {
                    self.blocks.insert(reentry.1, llvm_bb);
                }
                _ => (),
            },
            _ => panic!("Terminator must be set for BB before lowering to LLVM"),
        };
        Ok(())
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

    fn alloc_arg(&mut self, arg_id: ArgId, decl: &ArgDecl) -> Result<(), TransformerError> {
        let name = self.arg_label(decl);
        let arg_value = self.get_arg(arg_id)?;
        arg_value.set_name(&name);

        // Check if variable name already exists
        let var_id = decl.var_id().unwrap();
        match self.vars.entry(var_id) {
            // If it does -> return an error
            Entry::Occupied(_) => Err(TransformerError::VariableAlreadyAllocated),

            // If not, then allocate a pointer in the Builder
            Entry::Vacant(ve) => {
                // and add a mapping from VarID to the pointer in the local var table
                ve.insert(Location::Argument(arg_value));
                Ok(())
            }
        }
    }

    fn alloc_var(&mut self, id: VarId, decl: &VarDecl) -> Result<(), TransformerError> {
        let name = self.var_label(decl);

        // Check if variable name already exists
        match self.vars.entry(id) {
            // If it does -> return an error
            Entry::Occupied(occ) => match occ.get() {
                Location::Argument(_) => Ok(()),
                _ => Err(TransformerError::VariableAlreadyAllocated),
            },

            // If not, then allocate a pointer in the Builder
            Entry::Vacant(ve) => {
                // and add a mapping from VarID to the pointer in the local var table
                let ty = self.program.get_type(decl.ty())?;
                let ptr = self
                    .program
                    .builder
                    .build_alloca(ty.into_basic_type().unwrap(), &name);
                ve.insert(Location::Pointer(ptr));
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
                // and add a mapping from TempID to the pointer in the local var table
                let loc = if let Ok(ty) = self.program.get_type(vd.ty())?.into_basic_type() {
                    let ptr = self.program.builder.build_alloca(ty, &name);
                    Location::Pointer(ptr)
                } else {
                    Location::Void
                };
                ve.insert(loc);
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
    ) -> Result<(), TransformerError> {
        // Look up then_bb
        let then_bb = self
            .blocks
            .get(&then_bb)
            .ok_or(TransformerError::BasicBlockNotFound)?;
        // Look up else_bb
        let else_bb = self
            .blocks
            .get(&else_bb)
            .ok_or(TransformerError::BasicBlockNotFound)?;
        // Create conditional jump to then or else
        self.program
            .builder
            .build_conditional_branch(cond.into_int_value(), *then_bb, *else_bb);

        Ok(())
    }

    fn term_call_fn(
        &mut self,
        span: Span,
        target: Location<'ctx>,
        mut args: VecDeque<BasicValueEnum<'ctx>>,
        reentry: (Location<'ctx>, BasicBlockId),
    ) -> Result<(), TransformerError> {
        let f = target.into_function()?;
        match f.ret_method {
            ReturnMethod::OutParam => {
                // If the return method is to use an out parameter, then push the
                // return value location to the front of the argument list for the functoin
                let out = reentry.0.into_pointer()?.as_basic_value_enum();
                args.push_front(out);
            }
            ReturnMethod::Return => (),
        }

        // This is done to try and minimize the memory usage. The out pointer parameter must be prepended
        // to the vector of arguments, but the `inkwell` API takes a slice, which means the collection of
        // arguments must be contiguous in memory.
        let arg_slice = args.make_contiguous();
        let result =
            self.program
                .builder
                .build_call(target.into_function()?.function, arg_slice, "");

        // If the return method is to return with the LLVM Return operator, then store
        // that value into the temp location
        if f.ret_method == ReturnMethod::Return {
            match result.try_as_basic_value().left() {
                Some(r) => self.store(span, reentry.0, r),
                None => {
                    assert!(reentry.0 == Location::Void, "If function called is void, then the call site value location must be Void")
                }
            }
        }

        self.blocks
            .get(&reentry.1)
            .ok_or(TransformerError::BasicBlockNotFound)?;

        Ok(())
    }

    fn term_goto(&mut self, target: BasicBlockId) -> Result<(), TransformerError> {
        let target = self
            .blocks
            .get(&target)
            .ok_or(TransformerError::BasicBlockNotFound)?;
        self.program.builder.build_unconditional_branch(*target);

        Ok(())
    }

    fn load(&self, lv: Location<'ctx>) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match lv {
            Location::Pointer(ptr) => {
                if ptr.get_type().get_element_type().is_aggregate_type() {
                    Ok(ptr.into())
                } else {
                    Ok(self.program.builder.build_load(lv.into_pointer()?, ""))
                }
            }
            Location::Argument(arg) => Ok(arg),
            Location::Function(_) | Location::ReturnPointer | Location::Void => Err(
                TransformerError::Internal(&LlvmBuilderError::ReadInvalidLocation),
            ),
        }
    }

    fn store(&mut self, span: Span, l: Location<'ctx>, r: BasicValueEnum<'ctx>) {
        match l {
            Location::Pointer(ptr) => {
                // If we're dealing with aggregate values, then memcpy
                if ptr.get_type().get_element_type().is_aggregate_type() {
                    self.build_memcpy(ptr, r.into_pointer_value(), span)
                } else {
                    self.program.builder.build_store(ptr, r);
                }
            }
            Location::Function(_) => panic!("Cannot store in a function location"),
            Location::ReturnPointer => match &mut self.ret_ptr {
                ReturnPointer::Unit => panic!("Attempting to return a value on a Unit function"),
                ReturnPointer::Value(val) => *val = Some(r),
                ReturnPointer::OutParam(out_ptr) => {
                    let dest = out_ptr.into_pointer().unwrap();
                    self.build_memcpy(dest, r.into_pointer_value(), span)
                }
            },
            Location::Void => (),
            Location::Argument(_) => panic!("Cannot store to an argument"),
        }
    }

    fn static_loc(&self, id: DefId) -> Result<Location<'ctx>, TransformerError> {
        let f = self
            .program
            .fn_table
            .get(&id)
            .ok_or(TransformerError::FunctionNotFound)?;
        Ok(Location::Function(*f))
    }

    fn var(&self, v: VarId) -> Result<Location<'ctx>, TransformerError> {
        self.vars
            .get(&v)
            .copied()
            .ok_or(TransformerError::VarNotFound)
    }

    fn temp(&self, v: TempId) -> Result<Location<'ctx>, TransformerError> {
        self.temps
            .get(&v)
            .copied()
            .ok_or(TransformerError::TempNotFound)
    }

    fn array_access(
        &self,
        l: Location<'ctx>,
        idx: BasicValueEnum<'ctx>,
    ) -> std::result::Result<Location<'ctx>, TransformerError> {
        let ptr = l.into_pointer()?;

        // Convert the index into an IntValue.  This will panic if `idx` is not an integer but
        // the semantic analysis should prevent that from happening
        let llvm_index = idx.into_int_value();

        // Compute the GEP to the element in the array
        let outer_idx = self.program.context.i64_type().const_int(0, false);
        let el_ptr = unsafe {
            self.program
                .builder
                .build_gep(ptr, &[outer_idx, llvm_index], "")
        };

        Ok(Location::Pointer(el_ptr))
    }

    fn field_access(
        &self,
        l: Location<'ctx>,
        field: FieldId,
    ) -> Result<Location<'ctx>, TransformerError> {
        // Convert l to a pointer
        let ptr = l.into_pointer()?;

        // Build GEP to field
        let field_ptr = self
            .program
            .builder
            .build_struct_gep(ptr, field.to_u32(), "")
            .expect("Cannot construct GEP");
        Ok(Location::Pointer(field_ptr))
    }

    fn return_ptr(&self) -> Result<Location<'ctx>, TransformerError> {
        Ok(Location::ReturnPointer)
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

    fn const_null(&self) -> BasicValueEnum<'ctx> {
        let zero = self.program.context.i64_type().const_zero();
        self.program
            .builder
            .build_int_to_ptr(
                zero,
                self.program
                    .context
                    .i64_type()
                    .ptr_type(AddressSpace::Generic),
                "",
            )
            .into()
    }

    fn const_f64(&self, f: f64) -> BasicValueEnum<'ctx> {
        self.program.context.f64_type().const_float(f).into()
    }

    fn string_literal(&mut self, id: StringId) -> BasicValueEnum<'ctx> {
        let label = self.create_stringpool_label(id);
        match self.program.module.get_global(&label) {
            Some(g) => g.as_pointer_value().into(),
            None => {
                let s = self.program.str_table.get(id).unwrap();
                let escaped_s = super::llvmir::convert_esc_seq_to_ascii(&s).unwrap();
                let len_w_null = escaped_s.len() + 1;
                let g = self.program.module.add_global(
                    self.program.context.i8_type().array_type(len_w_null as u32),
                    None,
                    &self.create_stringpool_label(id),
                );
                g.set_initializer(
                    &self
                        .program
                        .context
                        .const_string(escaped_s.as_bytes(), true),
                );
                let ptr = g.as_pointer_value();
                ptr.into()
            }
        }
    }

    fn i_add(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                Ok(self.program.builder.build_int_add(l, r, "").into())
            }
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn i_sub(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                Ok(self.program.builder.build_int_sub(l, r, "").into())
            }
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn i_mul(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                Ok(self.program.builder.build_int_mul(l, r, "").into())
            }
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn si_div(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                // With the current design, the difference between signed and unsigned division is
                // a hardware difference and falls squarely within the field of the LLVM generator
                // module.  But this violates the precept that this module makes no decisions and only
                // transcribes exactly what it is given.  Ultimately, I need to capture the notion
                // of operators for each operand type in the language layer; especially when I get
                // to implementing FP values and operations.
                Ok(self.program.builder.build_int_signed_div(l, r, "").into())
            }
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn ui_div(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                // With the current design, the difference between signed and unsigned division is
                // a hardware difference and falls squarely within the field of the LLVM generator
                // module.  But this violates the precept that this module makes no decisions and only
                // transcribes exactly what it is given.  Ultimately, I need to capture the notion
                // of operators for each operand type in the language layer; especially when I get
                // to implementing FP values and operations.
                Ok(self.program.builder.build_int_unsigned_div(l, r, "").into())
            }
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn f_add(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
                Ok(self.program.builder.build_float_add(l, r, "").into())
            }
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn f_sub(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
                Ok(self.program.builder.build_float_sub(l, r, "").into())
            }
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn f_mul(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
                Ok(self.program.builder.build_float_mul(l, r, "").into())
            }
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn f_div(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
                Ok(self.program.builder.build_float_div(l, r, "").into())
            }
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn i_eq(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                .program
                .builder
                .build_int_compare(IntPredicate::EQ, l, r, "")
                .into()),
            (BasicValueEnum::PointerValue(l), BasicValueEnum::PointerValue(r)) => {
                let result = self.build_ptr_compare(IntPredicate::EQ, l, r).into();
                Ok(result)
            }
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn i_neq(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                .program
                .builder
                .build_int_compare(IntPredicate::NE, l, r, "")
                .into()),
            (BasicValueEnum::PointerValue(l), BasicValueEnum::PointerValue(r)) => {
                let result = self.build_ptr_compare(IntPredicate::NE, l, r).into();
                Ok(result)
            }
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn si_lt(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                .program
                .builder
                .build_int_compare(IntPredicate::SLT, l, r, "")
                .into()),
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn ui_lt(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                .program
                .builder
                .build_int_compare(IntPredicate::ULT, l, r, "")
                .into()),
            (BasicValueEnum::PointerValue(l), BasicValueEnum::PointerValue(r)) => {
                let result = self.build_ptr_compare(IntPredicate::ULT, l, r).into();
                Ok(result)
            }
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn si_lte(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                .program
                .builder
                .build_int_compare(IntPredicate::SLE, l, r, "")
                .into()),
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn ui_lte(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                .program
                .builder
                .build_int_compare(IntPredicate::ULE, l, r, "")
                .into()),
            (BasicValueEnum::PointerValue(l), BasicValueEnum::PointerValue(r)) => {
                let result = self.build_ptr_compare(IntPredicate::ULE, l, r).into();
                Ok(result)
            }
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn si_gt(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                .program
                .builder
                .build_int_compare(IntPredicate::SGT, l, r, "")
                .into()),
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn ui_gt(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                .program
                .builder
                .build_int_compare(IntPredicate::UGT, l, r, "")
                .into()),
            (BasicValueEnum::PointerValue(l), BasicValueEnum::PointerValue(r)) => {
                let result = self.build_ptr_compare(IntPredicate::UGT, l, r).into();
                Ok(result)
            }
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn si_gte(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                .program
                .builder
                .build_int_compare(IntPredicate::SGE, l, r, "")
                .into()),
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn ui_gte(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                .program
                .builder
                .build_int_compare(IntPredicate::UGE, l, r, "")
                .into()),
            (BasicValueEnum::PointerValue(l), BasicValueEnum::PointerValue(r)) => {
                let result = self.build_ptr_compare(IntPredicate::UGE, l, r).into();
                Ok(result)
            }
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn f_eq(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                .program
                .builder
                .build_float_compare(inkwell::FloatPredicate::OEQ, l, r, "")
                .into()),
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn f_neq(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                .program
                .builder
                .build_float_compare(inkwell::FloatPredicate::ONE, l, r, "")
                .into()),
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn f_lt(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                .program
                .builder
                .build_float_compare(inkwell::FloatPredicate::OLT, l, r, "")
                .into()),
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn f_lte(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                .program
                .builder
                .build_float_compare(inkwell::FloatPredicate::OLE, l, r, "")
                .into()),
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn f_gt(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                .program
                .builder
                .build_float_compare(inkwell::FloatPredicate::OGT, l, r, "")
                .into()),
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn f_gte(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                .program
                .builder
                .build_float_compare(inkwell::FloatPredicate::OGE, l, r, "")
                .into()),
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn i_neg(
        &self,
        a: BasicValueEnum<'ctx>,
    ) -> std::result::Result<BasicValueEnum<'ctx>, TransformerError> {
        match a {
            BasicValueEnum::IntValue(a) => Ok(self.program.builder.build_int_neg(a, "").into()),
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn f_neg(&self, a: BasicValueEnum<'ctx>) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match a {
            BasicValueEnum::FloatValue(a) => Ok(self.program.builder.build_float_neg(a, "").into()),
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn i_not(&self, a: BasicValueEnum<'ctx>) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match a {
            BasicValueEnum::IntValue(a) => Ok(self.program.builder.build_not(a, "").into()),
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn i_and(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                Ok(self.program.builder.build_and(l, r, "").into())
            }
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn i_or(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match (a, b) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                Ok(self.program.builder.build_or(l, r, "").into())
            }
            _ => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidArithmeticOperands,
            )),
        }
    }

    fn cast(
        &self,
        l: BasicValueEnum<'ctx>,
        l_signed: bool,
        l_sz: u64,
        target: TypeId,
        target_signed: bool,
        target_sz: u64,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        let target_ty = self.program.get_type(target)?.into_basic_type().unwrap();

        let op = match (l, target_ty) {
            (BasicValueEnum::IntValue(iv), BasicTypeEnum::IntType(tty)) => {
                // if upcasting
                if l_sz < target_sz {
                    match (l_signed, target_signed) {
                        (false, false) | (false, true) => {
                            self.program.builder.build_int_z_extend(iv, tty, "")
                        }
                        (true, false) | (true, true) => {
                            self.program.builder.build_int_s_extend(iv, tty, "")
                        }
                    }
                // else if downcasting
                } else {
                    // trancate
                    self.program.builder.build_int_truncate(iv, tty, "")
                }
                .into()
            }
            (BasicValueEnum::IntValue(iv), BasicTypeEnum::FloatType(tty)) => {
                // if source is signed
                if l_signed {
                    self.program.builder.build_signed_int_to_float(iv, tty, "")
                // otherwise
                } else {
                    self.program
                        .builder
                        .build_unsigned_int_to_float(iv, tty, "")
                }
                .into()
            }
            // float to int
            (BasicValueEnum::FloatValue(fv), BasicTypeEnum::IntType(tty)) => {
                // if target is signed
                if target_signed {
                    self.program.builder.build_float_to_signed_int(fv, tty, "")
                } else {
                    self.program
                        .builder
                        .build_float_to_unsigned_int(fv, tty, "")
                }
                .into()
            }
            (BasicValueEnum::FloatValue(fv), BasicTypeEnum::FloatType(tty)) => {
                // if upcasting
                if l_sz < target_sz {
                    self.program.builder.build_float_ext(fv, tty, "")
                // otherwise
                } else {
                    self.program.builder.build_float_trunc(fv, tty, "")
                }
                .into()
            }
            // pointer to int
            (BasicValueEnum::PointerValue(pv), BasicTypeEnum::IntType(tty)) => {
                self.program.builder.build_ptr_to_int(pv, tty, "").into()
            }
            // int to pointer
            (BasicValueEnum::IntValue(iv), BasicTypeEnum::PointerType(tty)) => {
                self.program.builder.build_int_to_ptr(iv, tty, "").into()
            }
            // pointer to pointer
            (BasicValueEnum::PointerValue(pv), BasicTypeEnum::PointerType(tty)) => {
                self.program.builder.build_bitcast(pv, tty, "")
            }
            _ => {
                return Err(TransformerError::Internal(
                    &LlvmBuilderError::InvalidCastingOperation,
                ))
            }
        };

        Ok(op)
    }

    fn size_of(&self, ty: TypeId) -> BasicValueEnum<'ctx> {
        let llvm_ty = self.program.get_type(ty).unwrap();
        llvm_ty
            .size_of()
            .expect("Could not compute size from given type")
            .into()
    }

    fn address_of(&self, a: Location<'ctx>) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        match a {
            Location::Pointer(ptr) => Ok(ptr.into()),
            Location::Argument(_)
            | Location::ReturnPointer
            | Location::Void
            | Location::Function(_) => Err(TransformerError::Internal(
                &LlvmBuilderError::InvalidOperand,
            )),
        }
    }

    fn deref(&self, a: Location<'ctx>) -> Result<Location<'ctx>, TransformerError> {
        match a {
            Location::Pointer(ptr) => {
                if ptr.get_type().get_element_type().is_aggregate_type() {
                    Ok(Location::Pointer(ptr))
                } else {
                    Ok(Location::Pointer(
                        self.program
                            .builder
                            .build_load(ptr, "")
                            .into_pointer_value(),
                    ))
                }
            }
            Location::Argument(ptr) => Ok(Location::Pointer(ptr.into_pointer_value())),
            Location::Function(_) | Location::ReturnPointer | Location::Void => Err(
                TransformerError::Internal(&LlvmBuilderError::InvalidOperand),
            ),
        }
    }

    fn pointer_offset(
        &self,
        a: BasicValueEnum<'ctx>,
        o: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, TransformerError> {
        let ptr = a.into_pointer_value();
        let offset = o.into_int_value();

        let result = unsafe { self.program.builder.build_gep(ptr, &[offset], "").into() };
        Ok(result)
    }
}
