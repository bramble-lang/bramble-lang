#![allow(unused_imports, unused_variables)]

/// The compiler traverses the Bramble AST and constructs and constructs
/// an LLVM Module through LLVM IR.

/// This uses the LLVM C API to interface with LLVM and construct the
/// Module. Resulting IR can then be fed into the LLVM Compiler to compile
/// into native assembly or into a JIT.
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    convert::TryFrom,
    error::Error,
    rc::Rc,
};

use inkwell::{
    builder::Builder,
    context,
    execution_engine::{ExecutionEngine, JitFunction},
    module::Module,
    passes::PassManager,
    targets::{CodeModel, InitializationConfig, RelocMode, Target},
    types::*,
    values::*,
    AddressSpace, FloatPredicate, IntPredicate, OptimizationLevel,
};

use crate::{
    compiler::{
        ast::{Element, Parameter, StructDef},
        diagnostics::{Event, EventId, EventStack, Logger, View, Writable},
        import::{Import, ImportRoutineDef, ImportStructDef},
        parser::{ParserContext, ParserError},
        source::SourceIr,
        CompilerError, SourceMap, Span,
    },
    result::Result,
    StringId, StringTable,
};

use crate::{
    compiler::{
        ast::{Context, Node, Path, Type},
        semantics::semanticnode::SemanticContext,
    },
    project::manifest::Manifest,
};

use super::ast;

use super::{scopestack::RegisterLookup, stringpool::StringPool};

const MEM_ALIGNMENT: u64 = 8;

/// A LLVM IR generator which can be used to generate all the code
/// for a single LLVM Module.
pub struct IrGen<'ctx> {
    context: &'ctx context::Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    imports: &'ctx [Import],
    string_pool: StringPool<'ctx>,
    registers: RegisterLookup<'ctx>,
    struct_table: HashMap<String, ast::StructDef<SemanticContext>>,
    fn_use_out_param: HashSet<String>,
    string_table: &'ctx StringTable,
    source_map: &'ctx SourceMap,
    logger: &'ctx Logger<'ctx>,
    event_stack: EventStack,
}

impl<'ctx> IrGen<'ctx> {
    pub fn new(
        ctx: &'ctx context::Context,
        module: &str,
        imports: &'ctx [Import],
        source_map: &'ctx SourceMap,
        string_table: &'ctx StringTable,
        logger: &'ctx Logger,
    ) -> IrGen<'ctx> {
        IrGen {
            context: ctx,
            module: ctx.create_module(module),
            builder: ctx.create_builder(),
            imports,
            string_pool: StringPool::new(string_table),
            registers: RegisterLookup::new(),
            struct_table: HashMap::new(),
            fn_use_out_param: HashSet::new(),
            source_map,
            string_table,
            logger,
            event_stack: EventStack::new(),
        }
    }

    /// Print the LLVM IR to stderr
    pub fn print_err(&self) {
        self.module.print_to_stderr();
    }

    /// Print the LLVM IR to the given file
    pub fn emit_llvm_ir(&self, path: &std::path::Path) {
        self.module.print_to_file(path).unwrap()
    }

    /// Compile the LLVM IR into an object file for the target platform
    pub fn emit_object_code(&self, path: &std::path::Path, emit_asm: bool) -> Result<()> {
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
            .ok_or("Could not create a target machine for compilation")?;
        let data = machine.get_target_data();

        // Configure the module
        self.module.set_data_layout(&data.get_data_layout());
        self.module.set_triple(&triple);

        // If emit asm is true, then also write the assembly for the target machine to a file
        if emit_asm {
            let asm_path = path.with_extension("s");
            machine
                .write_to_file(
                    &self.module,
                    inkwell::targets::FileType::Assembly,
                    &asm_path,
                )
                .map_err(|e| e.to_string())?
        }

        // Emit object file
        machine
            .write_to_file(&self.module, inkwell::targets::FileType::Object, path)
            .map_err(|e| e.to_string())
    }

    /// Take the given Bramble AST to compile it to LLVM IR and add it to the LLVM module.
    ///
    /// All user input is expected to be fully validated and correct by the time it reaches
    /// the compiler phase (via syntactic and semantic analysis).  Therefore, if anything
    /// goes wrong during compilation, it is assumed to be the result of a critical bug in
    /// the compiler itself and not an issue with the input Bramble code. This means that any
    /// error at this stage is unrecoverable; since its a bug in the compiler itself it cannot
    /// be trusted. So, if any unexpected state is encountered or any error happens this module
    /// will panic at that point in code and crash the compiler.
    pub fn ingest(
        &mut self,
        m: &'ctx ast::Module<SemanticContext>,
        user_main: StringId,
    ) -> Result<()> {
        self.compile_string_pool(m);
        self.add_imports();

        self.add_mod_items(m);

        if let Some(main_path) = Self::find_distinct_user_main(m, user_main)? {
            self.configure_user_main(&main_path)
        }

        match m.to_llvm_ir(self) {
            None => (),
            Some(_) => panic!("Expected None when compiling a Module"),
        };

        Ok(())
    }

    fn find_distinct_user_main(
        m: &'ctx ast::Module<SemanticContext>,
        user_main: StringId,
    ) -> Result<Option<Path>> {
        let mut matches = vec![];
        let base_path = Path::new();
        Self::find_user_main(m, user_main, base_path, &mut matches);
        if matches.len() == 1 {
            Ok(Some(matches[0].clone()))
        } else if matches.is_empty() {
            Ok(None)
        } else {
            Err(format!(
                "Found multiple {} functions in the project",
                user_main,
            ))
        }
    }

    fn find_user_main(
        module: &'ctx ast::Module<SemanticContext>,
        user_main: StringId,
        mut path: Path,
        matches: &mut Vec<Path>,
    ) {
        path.push(Element::Id(
            module.name().expect("Modules must have a name."),
        ));

        // Search through functions for "my_main"
        let functions = module.get_functions();
        for f in functions {
            match f.name() {
                Some(name) if name == user_main => {
                    let mut path = path.clone();
                    path.push(Element::Id(name));
                    matches.push(path);
                }
                _ => (),
            }
        }

        // If not found, recurse through every module in this module
        let modules = module.get_modules();
        for m in modules {
            Self::find_user_main(m, user_main, path.clone(), matches);
        }
    }

    /// Creates `main` entry point which will be called by the OS to start the Bramble
    /// application. This main will initialize platform level values and state, then
    /// call the user defined main `my_main`.
    fn configure_user_main(&self, path: &Path) {
        let main_type = self.context.i64_type().fn_type(&[], false);
        let main = self.module.add_function("main", main_type, None);
        let entry_bb = self.context.append_basic_block(main, "entry");
        self.builder.position_at_end(entry_bb);

        let user_main_name = path.to_label(self.source_map, self.string_table);
        let user_main = self
            .module
            .get_function(&user_main_name)
            .unwrap_or_else(|| panic!("Could not find {}", user_main_name));

        let status = self
            .builder
            .build_call(user_main, &[], "user_main")
            .try_as_basic_value()
            .left()
            .unwrap();
        self.builder.build_return(Some(&status));
    }

    /// Add the list of external function declarations to the function table
    /// in the LLVM module
    fn add_imports(&mut self) {
        // Add struct definitions first in case there are any function declarations that depend
        // upon these types
        for manifest in self.imports {
            // Add imported structures to the LLVM Module
            for sd in &manifest.structs {
                self.add_import_struct_def(sd)
            }
        }

        // Add all function definitions that are imported from other projects
        for manifest in self.imports {
            // Add imported functions to the LLVM Module
            for rd in &manifest.funcs {
                self.add_fn_decl(
                    &rd.path().to_label(self.source_map, self.string_table),
                    &rd.params()
                        .iter()
                        .map(|(_, ty)| ty.clone())
                        .collect::<Vec<_>>(),
                    false,
                    rd.ty(),
                    Span::zero(),
                );
            }
        }
    }

    /// Take the given AST and add declarations for every function to the
    /// LLVM module. This is required so that the FunctionValue can be looked
    /// up when generating code for function calls.
    fn add_mod_items(&mut self, m: &'ctx ast::Module<SemanticContext>) {
        for s in m.get_structs() {
            if let ast::Item::Struct(sd) = s {
                self.add_struct_def(sd);
            } else {
                panic!("Expected a struct but got {}", s)
            }
        }

        for f in m.get_functions() {
            if let ast::Item::Routine(rd) = f {
                self.add_fn_def_decl(rd);
            }
        }

        for ex in m.get_externs() {
            if let ast::Item::Extern(ex) = ex {
                self.add_extern_fn_decl(ex);
            }
        }

        for m in m.get_modules() {
            self.add_mod_items(m);
        }
    }

    /// Takes a RoutineDef and adds its declaration to the
    /// LLVM Module. This function declaration can then be
    /// looked up through `self.module` for function calls
    /// and to add the definition to the function when
    /// compiling the AST to LLVM.
    fn add_fn_def_decl(&mut self, rd: &'ctx ast::RoutineDef<SemanticContext>) {
        let params: Vec<_> = rd.get_params().iter().map(|p| p.ty.clone()).collect();
        self.add_fn_decl(
            &rd.context
                .canonical_path()
                .to_label(self.source_map, self.string_table),
            &params,
            false,
            &rd.ret_ty,
            rd.span(),
        )
    }

    fn add_extern_fn_decl(&mut self, ex: &'ctx ast::Extern<SemanticContext>) {
        // Declare external function
        let params: Vec<_> = ex.get_params().iter().map(|p| p.ty.clone()).collect();
        let label = &ex
            .context()
            .canonical_path()
            .to_label(self.source_map, self.string_table);
        self.add_fn_decl(
            label,
            &params,
            ex.has_varargs,
            ex.get_return_type(),
            ex.span(),
        );

        let llvm_fn_decl = self.module.get_function(label).unwrap();
        self.record_terminal(ex.span(), &llvm_fn_decl);
    }

    /// Takes a tuple describing the signature of an function (internal or external) to the
    /// LLVM Module. This function declaration can then be
    /// looked up through `self.module` for function calls
    /// and to add the definition to the function when
    /// compiling the AST to LLVM.
    fn add_fn_decl(
        &mut self,
        name: &str,
        params: &[ast::Type],
        has_var_arg: bool,
        ret_ty: &ast::Type,
        span: Span,
    ) {
        let mut llvm_params = vec![];

        // If the return type is a structure, then update the function to use
        // a return parameter and make the function a void
        let llvm_ty = match ret_ty {
            ast::Type::Custom(_) | ast::Type::Array(..) => {
                self.fn_use_out_param.insert(name.into());

                let ptr_ty = ret_ty
                    .to_llvm_ir(self)
                    .map_err(|e| format!("S{}: {}", span, e))
                    .unwrap()
                    .into_basic_type()
                    .unwrap()
                    .ptr_type(AddressSpace::Generic)
                    .into();
                llvm_params.push(ptr_ty);

                ast::Type::Unit.to_llvm_ir(self)
            }
            _ => ret_ty.to_llvm_ir(self),
        }
        .map_err(|e| format!("S{}: {}", span, e))
        .unwrap();

        for p in params {
            let ty_llvm = p
                .to_llvm_ir(self)
                .map_err(|e| format!("S{}: {}", span, e))
                .unwrap()
                .into_basic_type();
            match ty_llvm {
                Ok(ty_llvm) if ty_llvm.is_aggregate_type() => {
                    llvm_params.push(ty_llvm.ptr_type(AddressSpace::Generic).into())
                }
                Ok(ty_llvm) => llvm_params.push(ty_llvm),
                Err(msg) => panic!("Failed to convert parameter type to LLVM: {}", msg),
            }
        }

        let fn_type = match llvm_ty {
            AnyTypeEnum::IntType(ity) => ity.fn_type(&llvm_params, has_var_arg),
            AnyTypeEnum::FloatType(fty) => fty.fn_type(&llvm_params, has_var_arg),
            AnyTypeEnum::PointerType(pty) => pty.fn_type(&llvm_params, has_var_arg),
            AnyTypeEnum::VoidType(vty) => vty.fn_type(&llvm_params, has_var_arg),
            _ => panic!("Unexpected type: {:?}", llvm_ty),
        };
        self.module.add_function(name, fn_type, None);
    }

    /// Add a struct definition to the LLVM context and module.
    fn add_struct_def(&mut self, sd: &'ctx ast::StructDef<SemanticContext>) {
        self.struct_table.insert(
            sd.context()
                .canonical_path()
                .to_label(self.source_map, self.string_table),
            sd.clone(),
        );
        let name = sd
            .context()
            .canonical_path()
            .to_label(self.source_map, self.string_table);

        // Add structure name to LLVM context before defining the fields (to allow for self referencing)
        let struct_ty = self.context.opaque_struct_type(&name);

        let fields_llvm: Vec<BasicTypeEnum<'ctx>> = sd
            .get_fields()
            .iter()
            .filter_map(|f| {
                // TODO: what's going on here?  Should this fail if I cannot convert to a basic type?
                f.ty.to_llvm_ir(self)
                    .map_err(|e| format!("S{}: {}", f.span(), e))
                    .unwrap()
                    .into_basic_type()
                    .ok()
            })
            .collect();
        struct_ty.set_body(&fields_llvm, false);
        self.record_terminal(sd.span(), &struct_ty);
    }

    /// Add a struct definition to the LLVM context and module.
    fn add_import_struct_def(&mut self, sd: &'ctx ImportStructDef) {
        let name = sd.path().to_label(self.source_map, self.string_table);
        let struct_ty = self.context.opaque_struct_type(&name);

        let fields_llvm: Vec<BasicTypeEnum<'ctx>> = sd
            .fields()
            .iter()
            .filter_map(|(field_name, field_ty)| {
                // TODO: what's going on here?  Should this fail if I cannot convert to a basic type?
                match field_ty {
                    ast::Type::Custom(_) => field_ty.to_llvm_ir(self),
                    _ => field_ty.to_llvm_ir(self),
                }
                .map_err(|e| format!("L{}: {}", 0, e))
                .unwrap()
                .into_basic_type()
                .ok()
            })
            .collect();

        self.struct_table.insert(name, sd.into());
        struct_ty.set_body(&fields_llvm, false);
    }

    /// Add all string literals to the data section of the assemby output
    fn compile_string_pool(&mut self, m: &ast::Module<SemanticContext>) {
        self.string_pool.extract_from_module(m);

        for (s, id) in self.string_pool.pool.iter() {
            let escaped_s = convert_esc_seq_to_ascii(s).unwrap();
            let len_w_null = escaped_s.len() + 1;
            let g = self.module.add_global(
                self.context.i8_type().array_type(len_w_null as u32),
                None,
                &self.get_stringpool_label(*id),
            );
            g.set_initializer(&self.context.const_string(escaped_s.as_bytes(), true));
        }
    }

    fn build_memcpy(&self, dest: PointerValue<'ctx>, src: PointerValue<'ctx>, span: Span) {
        let dest_align = get_ptr_alignment(dest);
        let src_align = get_ptr_alignment(src);
        let mc = self
            .builder
            .build_memcpy(
                dest,
                dest_align,
                src,
                src_align,
                dest.get_type().get_element_type().size_of().unwrap(),
            )
            .unwrap();
        self.record_terminal(span, &mc);
    }

    /// If the LLVM builder cursor is currently within a function, this will
    /// return that function.  Otherwise it will return `None`.
    fn get_current_fn(&self) -> Option<FunctionValue> {
        self.builder
            .get_insert_block()
            .and_then(|bb| bb.get_parent())
    }

    /// Will look for `s` in the string pool, if found, it will return the
    /// name of the global variable that is bound to that string. Otherwise,
    /// it will return `None`
    fn get_str_var(&self, s: &str) -> Option<String> {
        self.string_pool
            .get(s)
            .map(|id| self.get_stringpool_label(*id))
    }

    /// Convert the ID of a string to the name of the global variable that
    /// references that string
    fn get_stringpool_label(&self, id: usize) -> String {
        format!(
            "str_{}_{}",
            self.module
                .get_name()
                .to_str()
                .expect("Expected a valid UTF string for the Module name"),
            id
        )
    }

    /// Creates and records an event which does have any children.
    fn record_terminal<IR: Writable>(&self, span: Span, ir: IR) {
        self.logger.write(Event::<_, ParserError>::new_with_result(
            "llvm",
            span,
            Ok(ir),
            self.event_stack.clone(),
        ))
    }

    /// Start a new event with no set Result. Events created after this poing and
    /// before this [`Event`] is dropped will be descendents of this [`Event`].
    fn new_event<'a, IR: Writable>(&self, span: Span) -> Event<'a, IR, ParserError> {
        Event::new("llvm", span, self.event_stack.clone())
    }

    /// Record the result of an event
    fn record<IR: Writable>(&self, evt: Event<IR, ParserError>, ir: IR) {
        let event = evt.with_msg(Ok(ir));
        self.logger.write(event)
    }
}

trait ToLlvmIr<'ctx> {
    type Value: inkwell::values::AnyValue<'ctx>;

    /// Compile a Language unit to LLVM and return the appropriate LLVM Value
    /// if it has one (Modules don't have LLVM Values so those will return None)
    fn to_llvm_ir(&self, llvm: &mut IrGen<'ctx>) -> Option<Self::Value>;

    /// If an expression is addressable then this will generate LLVM IR code
    /// which gets the address of the expression's location rather than the value.
    fn to_address(&self, llvm: &mut IrGen<'ctx>) -> Option<PointerValue<'ctx>> {
        None
    }
}

impl<'ctx> ToLlvmIr<'ctx> for ast::Module<SemanticContext> {
    type Value = FunctionValue<'ctx>;

    fn to_llvm_ir(&self, llvm: &mut IrGen<'ctx>) -> Option<Self::Value> {
        for m in self.get_modules() {
            m.to_llvm_ir(llvm);
        }

        for f in self.get_functions() {
            if let ast::Item::Routine(rdef) = f {
                let fn_val = rdef
                    .to_llvm_ir(llvm)
                    .expect("Expected Function Value from RoutineDef");
            }
        }

        for c in self.get_coroutines() {
            todo!("Coroutine support not yet implemented")
        }

        None
    }
}

impl<'ctx> ToLlvmIr<'ctx> for ast::RoutineDef<SemanticContext> {
    type Value = FunctionValue<'ctx>;

    fn to_llvm_ir(&self, llvm: &mut IrGen<'ctx>) -> Option<Self::Value> {
        let event = llvm.new_event(self.span());
        let fn_name = self
            .context
            .canonical_path()
            .to_label(llvm.source_map, llvm.string_table);
        let fn_value = llvm
            .module
            .get_function(&fn_name)
            .expect("Could not find function");
        let entry_bb = llvm.context.append_basic_block(fn_value, "entry");
        llvm.builder.position_at_end(entry_bb);

        llvm.registers.open_fn().unwrap();
        let llvm_params = fn_value.get_params();
        let num_params = llvm_params.len();

        // If the function returns a structure, then the first parameter will
        // be the return parameter.
        let start = if llvm.fn_use_out_param.contains(&fn_name) {
            let pname = ".out";
            llvm.registers.insert(pname, llvm_params[0].into()).unwrap();
            1
        } else {
            0
        };

        for pi in start..num_params {
            let param = &(*self.get_params())[pi - start];
            let pid = &param.name;
            let pname = llvm.string_table.get(*pid).unwrap();

            // move parameter into the stack
            let pptr = llvm
                .builder
                .build_alloca(llvm_params[pi].get_type(), &pname);
            llvm.record_terminal(param.span(), &pptr);
            let st = llvm.builder.build_store(pptr, llvm_params[pi]);
            llvm.record_terminal(param.span(), &st);
            llvm.registers.insert(&pname, pptr.into()).unwrap();
        }

        // Compile the body to LLVM
        for stm in &self.body {
            let value = stm.to_llvm_ir(llvm);
        }

        llvm.registers.close_fn().unwrap();
        llvm.record(event, &fn_value);

        Some(fn_value)
    }
}

impl<'ctx> ToLlvmIr<'ctx> for ast::Statement<SemanticContext> {
    type Value = AnyValueEnum<'ctx>;

    fn to_llvm_ir(&self, llvm: &mut IrGen<'ctx>) -> Option<Self::Value> {
        match self {
            ast::Statement::Return(ret) => ret.to_llvm_ir(llvm).map(|i| i.into()),
            ast::Statement::Expression(exp) => exp.to_llvm_ir(llvm).map(|v| v.into()),
            ast::Statement::Bind(bind) => bind.to_llvm_ir(llvm).map(|i| i.into()),
            ast::Statement::Mutate(mutate) => mutate.to_llvm_ir(llvm).map(|i| i.into()),
            ast::Statement::YieldReturn(_) => todo!("Coroutines not yet implemented: {}", self),
        }
    }
}

impl<'ctx> ToLlvmIr<'ctx> for ast::Bind<SemanticContext> {
    type Value = PointerValue<'ctx>;

    fn to_llvm_ir(&self, llvm: &mut IrGen<'ctx>) -> Option<Self::Value> {
        let event = llvm.new_event(self.span());
        let sid = self.get_id();
        let name = llvm.string_table.get(sid).unwrap();

        match self
            .get_type()
            .to_llvm_ir(llvm)
            .map_err(|e| format!("S{}: {}", self.span(), e))
            .unwrap()
            .into_basic_type()
        {
            Ok(ty) if ty.is_aggregate_type() => {
                let rhs = self.get_rhs().to_llvm_ir(llvm).unwrap();
                let rhs_ptr = rhs.into_pointer_value();

                let alloca_event = llvm.new_event(self.span());
                let dest = llvm.builder.build_alloca(ty, &name);

                llvm.build_memcpy(dest, rhs_ptr, self.span());

                llvm.registers.insert(&name, dest.into()).unwrap();
                llvm.record(alloca_event, &dest);
                Some(dest)
            }
            Ok(ty) => {
                let store_event = llvm.new_event(self.span());
                let alloca_event = llvm.new_event(self.span());
                let ptr = llvm.builder.build_alloca(ty, &name);

                let rhs = self.get_rhs().to_llvm_ir(llvm).unwrap();
                let st = llvm.builder.build_store(ptr, rhs);

                llvm.registers.insert(&name, ptr.into()).unwrap();
                llvm.record(alloca_event, &ptr);
                llvm.record(store_event, &st);
                Some(ptr)
            }
            Err(msg) => panic!("Failed to convert to basic type: {}", msg),
        }
        .view(|x| llvm.record(event, x))
    }
}

impl<'ctx> ToLlvmIr<'ctx> for ast::Mutate<SemanticContext> {
    type Value = PointerValue<'ctx>;

    fn to_llvm_ir(&self, llvm: &mut IrGen<'ctx>) -> Option<Self::Value> {
        let event = llvm.new_event(self.span());

        let rhs = self.get_rhs().to_llvm_ir(llvm).unwrap();
        let lhs_ptr = self.get_lhs().to_address(llvm).unwrap();

        let st = llvm.builder.build_store(lhs_ptr, rhs);
        llvm.record(event, &st);

        Some(lhs_ptr)
    }
}

impl<'ctx> ToLlvmIr<'ctx> for ast::Return<SemanticContext> {
    type Value = InstructionValue<'ctx>;

    fn to_llvm_ir(&self, llvm: &mut IrGen<'ctx>) -> Option<Self::Value> {
        let event = llvm.new_event(self.span());
        Some(match self.get_value() {
            None => llvm.builder.build_return(None),
            Some(val) => {
                match val.get_type() {
                    // Instead of type use the table that indicates the out parameter was added
                    // TODO: I think that this can be linked to the `llvm.fn_out_params` table. I do it with Return
                    ast::Type::Custom(_) | ast::Type::Array(..) => {
                        let out = llvm.registers.get(".out").unwrap().into_pointer_value();
                        let src_ptr = val.to_llvm_ir(llvm).unwrap().into_pointer_value();
                        llvm.build_memcpy(out, src_ptr, self.span());

                        // Use the return parameter as a ptr to memory to store the struct and copy it there
                        llvm.builder.build_return(None)
                    }
                    _ => {
                        let val = val
                            .to_llvm_ir(llvm)
                            .expect("Return expression did not compile to an LLVM value");
                        llvm.builder.build_return(Some(&val))
                    }
                }
            }
        })
        .view(|ir| llvm.record(event, ir))
    }
}

impl<'ctx> ToLlvmIr<'ctx> for ast::Expression<SemanticContext> {
    type Value = BasicValueEnum<'ctx>;

    fn to_address(&self, llvm: &mut IrGen<'ctx>) -> Option<PointerValue<'ctx>> {
        // If this value is not addressable then do _not_ return a pointer
        // The semantic analyzer should prevent this situation from happening but just in case
        if !self.context().is_addressable() {
            return None;
        }

        match self {
            ast::Expression::Identifier(_, id) => {
                let name = llvm.string_table.get(*id).unwrap();

                let ptr = llvm.registers.get(&name).unwrap().into_pointer_value();
                Some(ptr)
            }
            ast::Expression::ArrayAt {
                context: ctx,
                array,
                index,
            } => {
                // evalute the array to get the ptr to the array
                // Check the array type, if it's not a pointer then get the GEP
                let llvm_array_ptr = match array.to_llvm_ir(llvm) {
                    Some(a) if a.is_pointer_value() => a.into_pointer_value(),
                    Some(a) => panic!("Unexpected type for array: {:?}", a),
                    None => panic!("Could not convert type {} to LLVM type", array),
                };

                // evaluate the index to get the index value
                let llvm_index = index.to_llvm_ir(llvm).unwrap().into_int_value();

                // Compute the GEP
                let outer_idx = llvm.context.i64_type().const_int(0, false);
                let el_ptr = unsafe {
                    llvm.builder
                        .build_gep(llvm_array_ptr, &[outer_idx, llvm_index], "")
                };
                Some(el_ptr)
            }
            ast::Expression::MemberAccess(ctx, val, field) => {
                let event = llvm.new_event(self.span());
                let sdef = llvm
                    .struct_table
                    .get(
                        &val.get_type()
                            .get_path()
                            .unwrap()
                            .to_label(llvm.source_map, llvm.string_table),
                    )
                    .unwrap();

                let field_idx = sdef.get_field_idx(*field).unwrap();

                let val_llvm = val.to_llvm_ir(llvm).unwrap().into_pointer_value();
                let field_ptr = llvm
                    .builder
                    .build_struct_gep(val_llvm, field_idx as u32, "")
                    .unwrap();
                llvm.record(event, &field_ptr);

                Some(field_ptr)
            }
            ast::Expression::UnaryOp(ctx, ast::UnaryOperator::DerefRawPointer, operand) => {
                let r = operand.to_llvm_ir(llvm).expect("Expected a value");
                let ptr = r.into_pointer_value();
                Some(ptr)
            }
            _ => None,
        }
    }

    fn to_llvm_ir(&self, llvm: &mut IrGen<'ctx>) -> Option<Self::Value> {
        match self {
            ast::Expression::Null(_) => {
                let zero = llvm.context.i64_type().const_zero();
                Some(
                    llvm.builder
                        .build_int_to_ptr(
                            zero,
                            llvm.context.i64_type().ptr_type(AddressSpace::Generic),
                            "",
                        )
                        .into(),
                )
            }
            ast::Expression::U8(_, i) => {
                let u8t = llvm.context.i8_type();
                Some(u8t.const_int(*i as u64, false).into())
                    .view(|ir| llvm.record_terminal(self.span(), ir))
                // TODO: Is it correct to NOT sign extend for unsigned ints?
            }
            ast::Expression::U16(_, i) => {
                let u16t = llvm.context.i16_type();
                Some(u16t.const_int(*i as u64, false).into())
                    .view(|ir| llvm.record_terminal(self.span(), ir)) // TODO: Is it correct to NOT sign extend for unsigned ints?
            }
            ast::Expression::U32(_, i) => {
                let u32t = llvm.context.i32_type();
                Some(u32t.const_int(*i as u64, false).into())
                    .view(|ir| llvm.record_terminal(self.span(), ir)) // TODO: Is it correct to NOT sign extend for unsigned ints?
            }
            ast::Expression::U64(_, i) => {
                let u64t = llvm.context.i64_type();
                Some(u64t.const_int(*i as u64, false).into())
                    .view(|ir| llvm.record_terminal(self.span(), ir)) // TODO: Is it correct to NOT sign extend for unsigned ints?
            }
            ast::Expression::I8(_, i) => {
                let i8t = llvm.context.i8_type();
                Some(i8t.const_int(*i as u64, true).into())
                    .view(|ir| llvm.record_terminal(self.span(), ir))
            }
            ast::Expression::I16(_, i) => {
                let i16t = llvm.context.i16_type();
                Some(i16t.const_int(*i as u64, true).into())
                    .view(|ir| llvm.record_terminal(self.span(), ir))
            }
            ast::Expression::I32(_, i) => {
                let i32t = llvm.context.i32_type();
                Some(i32t.const_int(*i as u64, true).into())
                    .view(|ir| llvm.record_terminal(self.span(), ir))
            }
            ast::Expression::I64(_, i) => {
                let i64t = llvm.context.i64_type();
                Some(i64t.const_int(*i as u64, true).into())
                    .view(|ir| llvm.record_terminal(self.span(), ir))
            }
            ast::Expression::F64(_, f) => {
                let i64t = llvm.context.f64_type();
                Some(i64t.const_float(*f as f64).into())
                    .view(|ir| llvm.record_terminal(self.span(), ir))
            }
            ast::Expression::Boolean(_, b) => {
                let bt = llvm.context.bool_type();
                let event = llvm.new_event::<&BasicValueEnum>(self.span());
                Some(bt.const_int(*b as u64, true).into()).view(|ir| llvm.record(event, ir))
            }
            ast::Expression::StringLiteral(_, s) => {
                let val = llvm.string_table.get(*s).unwrap();
                let str_id = llvm.get_str_var(&val).unwrap();
                let val = llvm.module.get_global(&str_id).unwrap();
                let val_ptr = val.as_pointer_value();
                let bitcast = llvm.builder.build_bitcast(
                    val_ptr,
                    llvm.context
                        .i8_type()
                        .array_type(0)
                        .ptr_type(AddressSpace::Generic),
                    "",
                );
                Some(bitcast).view(|ir| llvm.record_terminal(self.span(), ir))
            }
            ast::Expression::SizeOf(_, ty) => {
                let llvm_ty = ty.to_llvm_ir(llvm).unwrap();
                let sz = llvm_ty.size_of().unwrap();
                Some(sz.into()).view(|ir| llvm.record_terminal(self.span(), ir))
            }
            ast::Expression::Identifier(_, id) => {
                let name = llvm.string_table.get(*id).unwrap();
                let ptr = llvm.registers.get(&name).unwrap().into_pointer_value();
                if ptr.get_type().get_element_type().is_aggregate_type() {
                    Some(ptr.into())
                } else {
                    let val = llvm.builder.build_load(ptr, &name);
                    Some(val).view(|ir| llvm.record_terminal(self.span(), ir))
                }
            }
            ast::Expression::UnaryOp(_, op, exp) => Some(op.to_llvm_ir(llvm, exp, self.span())),
            ast::Expression::BinaryOp(_, op, l, r) => Some(op.to_llvm_ir(llvm, l, r, self.span())),
            ast::Expression::RoutineCall(meta, call, name, params) => call
                .to_llvm_ir(llvm, name, params, self.get_type(), self.span())
                .map_err(|e| format!("S{}: {}", self.span(), e))
                .unwrap(),
            ast::Expression::ExpressionBlock(meta, stmts, exp) => {
                llvm.registers.open_local().unwrap();
                for stmt in stmts {
                    stmt.to_llvm_ir(llvm);
                }
                let val = exp.as_ref().map(|e| e.to_llvm_ir(llvm)).flatten();
                llvm.registers.close_local().unwrap();
                val
            }
            ast::Expression::If {
                cond,
                if_arm: then_arm,
                else_arm,
                ..
            } => {
                let event = llvm.new_event(self.span());
                let cond_val = cond.to_llvm_ir(llvm).unwrap().into_int_value();
                let current_fn = llvm.get_current_fn().unwrap();
                let then_bb = llvm.context.append_basic_block(current_fn, "then");
                let else_bb = llvm.context.insert_basic_block_after(then_bb, "else");
                let merge_bb = llvm.context.insert_basic_block_after(else_bb, "merge");
                llvm.record(
                    event,
                    &llvm
                        .builder
                        .build_conditional_branch(cond_val, then_bb, else_bb),
                );

                let event = llvm.new_event(self.span());
                llvm.builder.position_at_end(then_bb);
                let then_arm_val = then_arm.to_llvm_ir(llvm);
                let then_bb = llvm.builder.get_insert_block().unwrap(); // The builders position may change after compiling the then block
                let br = llvm.builder.build_unconditional_branch(merge_bb);
                llvm.record(event, &br);

                let event = llvm.new_event(self.span());
                llvm.builder.position_at_end(else_bb);
                let else_arm_val = else_arm.as_ref().map(|ea| ea.to_llvm_ir(llvm)).flatten();
                let else_bb = llvm.builder.get_insert_block().unwrap(); // The builders position may changing after compiling the else block
                let br = llvm.builder.build_unconditional_branch(merge_bb);
                llvm.record(event, &br);

                llvm.builder.position_at_end(merge_bb);

                match (then_arm_val, else_arm_val) {
                    (Some(then_arm_val), Some(else_arm_val)) => {
                        // create phi to unify the branches
                        let phi = llvm.builder.build_phi(then_arm_val.get_type(), "phi");
                        phi.add_incoming(&[(&then_arm_val, then_bb), (&else_arm_val, else_bb)]);
                        Some(phi.as_basic_value())
                    }
                    (None, None) => None,
                    _ => panic!(
                        "Mismatching arms on if expression: {:?}, {:?}",
                        then_arm_val, else_arm_val
                    ),
                }
            }
            ast::Expression::While { cond, body, .. } => {
                let current_fn = llvm.get_current_fn().unwrap();

                // Construct the three components of the while loop
                // 1. The top of the loop that checks the condition
                // 2. The expression that is executed if the condition is true
                // 3. The exit point for the loop that is jumped to when the condition is false
                let loop_bb = llvm.context.append_basic_block(current_fn, "while_cond");
                let body_bb = llvm.context.append_basic_block(current_fn, "while_body");
                let after_bb = llvm.context.append_basic_block(current_fn, "while_end");

                let event = llvm.new_event(self.span());
                // Emit the logic for checking the while condition
                let br = llvm.builder.build_unconditional_branch(loop_bb);
                llvm.record(event, &br);

                let event = llvm.new_event(self.span());
                llvm.builder.position_at_end(loop_bb);
                // Test the condition and determine if the loop should be terminated
                let cond_val = cond.to_llvm_ir(llvm).unwrap().into_int_value();
                let br = llvm
                    .builder
                    .build_conditional_branch(cond_val, body_bb, after_bb);
                llvm.record(event, &br);

                // Emit the code that will evaluate the loop body
                let event = llvm.new_event(self.span());
                llvm.builder.position_at_end(body_bb);
                body.to_llvm_ir(llvm); // The result of the body is not used for anything so ignore it
                let br = llvm.builder.build_unconditional_branch(loop_bb);
                llvm.record(event, &br);

                // Position the LLVM Builder cursor to be immediately after the loop
                llvm.builder.position_at_end(after_bb);

                None
            }
            ast::Expression::MemberAccess(_, val, field) => {
                let event = llvm.new_event(self.span());
                let sdef = llvm
                    .struct_table
                    .get(
                        &val.get_type()
                            .get_path()
                            .unwrap()
                            .to_label(llvm.source_map, llvm.string_table),
                    )
                    .unwrap();

                let field_idx = sdef.get_field_idx(*field).unwrap();

                let field_idx_llvm = llvm.context.i64_type().const_int(field_idx as u64, true);

                let val_llvm = val.to_llvm_ir(llvm).unwrap().into_pointer_value();
                let field_ptr = llvm
                    .builder
                    .build_struct_gep(val_llvm, field_idx as u32, "")
                    .unwrap();
                llvm.record(event, &field_ptr);

                // check if the field_ptr element type is an aggregate, if so, return the ptr
                let el_ty = field_ptr.get_type().get_element_type();
                if el_ty.is_aggregate_type() {
                    Some(field_ptr.into())
                } else {
                    let event = llvm.new_event(self.span());
                    let field_val = llvm.builder.build_load(field_ptr, "");
                    Some(field_val).view(|ir| llvm.record(event, ir))
                }
            }
            ast::Expression::StructExpression(_, name, fields) => {
                let event = llvm.new_event(self.span());
                let sname = self
                    .context()
                    .ty()
                    .get_path()
                    .unwrap()
                    .to_label(llvm.source_map, llvm.string_table);
                let sdef = llvm
                    .struct_table
                    .get(&sname)
                    .unwrap_or_else(|| panic!("Cannot find {} in {:?}", sname, llvm.struct_table));
                let sdef_llvm = llvm.module.get_struct_type(&sname).unwrap();
                let s_ptr = llvm.builder.build_alloca(sdef_llvm, "");
                llvm.record(event, &s_ptr);

                // convert field names to field indexes (order of fields in expression may not
                // be the same as in the defintion)
                let idx_fields: Vec<(usize, &ast::Expression<SemanticContext>)> = fields
                    .iter()
                    .map(|(n, v)| (sdef.get_field_idx(*n).unwrap(), v))
                    .collect();

                for (f_idx, e) in idx_fields {
                    let event = llvm.new_event(e.span());
                    let val = e.to_llvm_ir(llvm).unwrap();
                    let fld_ptr = llvm
                        .builder
                        .build_struct_gep(s_ptr, f_idx as u32, "")
                        .unwrap();
                    llvm.record(event, &fld_ptr);

                    let el_ty = fld_ptr.get_type().get_element_type();
                    if el_ty.is_aggregate_type() {
                        // TODO: should I do this for all pointer values?
                        let val_ptr = val.into_pointer_value();
                        llvm.build_memcpy(fld_ptr, val_ptr, self.span());
                    } else {
                        let event = llvm.new_event(self.span());
                        let st = llvm.builder.build_store(fld_ptr, val);
                        llvm.record(event, &st);
                    }
                }
                Some(s_ptr.into())
            }
            ast::Expression::ArrayExpression(meta, elements, len) => {
                let event = llvm.new_event(self.span());
                let a_ty = meta.ty();
                let a_llvm_ty = a_ty
                    .to_llvm_ir(llvm)
                    .map_err(|e| format!("S{}: {}", self.span(), e))
                    .unwrap()
                    .into_basic_type()
                    .unwrap();
                let alloca_event = llvm.new_event(self.span());
                let a_ptr = llvm.builder.build_alloca(a_llvm_ty, "");
                llvm.record(alloca_event, &a_ptr);

                // Compute the results for each element of the array value
                let elements_llvm: Vec<_> = elements
                    .iter()
                    .map(|e| (e.to_llvm_ir(llvm).unwrap(), e.span()))
                    .collect();

                // Move those results into the elements of the array
                let mut idx = 0;
                let outer_idx = llvm.context.i64_type().const_int(0, false);
                for (e, e_span) in elements_llvm {
                    let event = llvm.new_event(e_span);
                    let llvm_idx = llvm.context.i64_type().const_int(idx, false);
                    let el_ptr =
                        unsafe { llvm.builder.build_gep(a_ptr, &[outer_idx, llvm_idx], "") };
                    llvm.record(event, &el_ptr);

                    let el_ty = el_ptr.get_type().get_element_type();

                    if el_ty.is_aggregate_type() {
                        llvm.build_memcpy(el_ptr, e.into_pointer_value(), self.span());
                    } else {
                        let event = llvm.new_event(e_span);
                        let st = llvm.builder.build_store(el_ptr, e);
                        llvm.record(event, &st);
                    }
                    idx += 1;
                }

                // The arch value of this expression is the ptr to the array
                Some(a_ptr.into()).view(|ir| llvm.record(event, ir))
            }
            ast::Expression::ArrayAt {
                context: meta,
                array,
                index,
            } => {
                let event = llvm.new_event(self.span());
                // evalute the array to get the ptr to the array
                // Check the array type, if it's not a pointer then get the GEP
                let llvm_array_ptr = match array.to_llvm_ir(llvm) {
                    Some(a) if a.is_pointer_value() => a.into_pointer_value(),
                    Some(a) => panic!("Unexpected type for array: {:?}", a),
                    None => panic!("Could not convert type {} to LLVM type", array),
                };

                // evaluate the index to get the index value
                let llvm_index = index.to_llvm_ir(llvm).unwrap().into_int_value();

                // Compute the GEP
                let outer_idx = llvm.context.i64_type().const_int(0, false);
                let el_ptr = unsafe {
                    llvm.builder
                        .build_gep(llvm_array_ptr, &[outer_idx, llvm_index], "")
                };
                llvm.record(event, &el_ptr);

                // Load the value pointed to by GEP and return that
                let el_ty = el_ptr.get_type().get_element_type();
                let el_val = if el_ty.is_aggregate_type() {
                    el_ptr.into()
                } else {
                    let event = llvm.new_event(self.span());
                    let ld = llvm.builder.build_load(el_ptr, "");
                    llvm.record(event, &ld);
                    ld
                };

                Some(el_val)
            }
            ast::Expression::TypeCast(_, src, target_ty) => {
                Some(self.type_cast(llvm, src, target_ty))
            }
            ast::Expression::CustomType(..) => {
                panic!("CustomType nodes should be resolved and removed before the compiler stage.")
            }
            ast::Expression::Path(..) => {
                panic!("Path nodes should be resolved and removed before the compiler stage.")
            }
            ast::Expression::IdentifierDeclare(..) => {
                panic!("IdentifierDelcare nodes should be resolved and removed before the compiler stage")
            }
            ast::Expression::Yield(..) => panic!("Yield is not yet implemented for LLVM"),
        }
    }
}

impl ast::Expression<SemanticContext> {
    /// Construct the LLVM IR instructions needed for casting one primitive type
    /// to another primitive type.  This will panic if the requested cast operation
    /// is illegal.
    fn type_cast<'ctx>(
        &self,
        llvm: &mut IrGen<'ctx>,
        src: &ast::Expression<SemanticContext>,
        target_ty: &Type,
    ) -> BasicValueEnum<'ctx> {
        let event = llvm.new_event(self.span());
        let target_ty_llvm = target_ty.to_llvm_ir(llvm).unwrap();
        let src_llvm = src.to_llvm_ir(llvm).unwrap();
        let src_signed = src.get_type().is_signed();
        let src_width = src.get_type().bit_width();
        let target_signed = target_ty.is_signed();
        let target_width = target_ty.bit_width();
        let op = match (src_llvm, target_ty_llvm) {
            (BasicValueEnum::IntValue(iv), AnyTypeEnum::IntType(tty)) => {
                // if upcasting
                if src_width < target_width {
                    match (src_signed, target_signed) {
                        (false, false) | (false, true) => {
                            llvm.builder.build_int_z_extend(iv, tty, "")
                        }
                        (true, false) | (true, true) => {
                            llvm.builder.build_int_s_extend(iv, tty, "")
                        }
                    }
                // else if downcasting
                } else {
                    // trancate
                    llvm.builder.build_int_truncate(iv, tty, "")
                }
                .into()
            }
            // int to float
            (BasicValueEnum::IntValue(iv), AnyTypeEnum::FloatType(tty)) => {
                // if source is signed
                if src_signed {
                    llvm.builder.build_signed_int_to_float(iv, tty, "")
                // otherwise
                } else {
                    llvm.builder.build_unsigned_int_to_float(iv, tty, "")
                }
                .into()
            }
            // float to int
            (BasicValueEnum::FloatValue(fv), AnyTypeEnum::IntType(tty)) => {
                // if target is signed
                let sty = src.get_type();
                if target_signed {
                    llvm.builder.build_float_to_signed_int(fv, tty, "")
                } else {
                    llvm.builder.build_float_to_unsigned_int(fv, tty, "")
                }
                .into()
            }
            // float to float
            (BasicValueEnum::FloatValue(fv), AnyTypeEnum::FloatType(tty)) => {
                // if upcasting
                if src_width < target_width {
                    llvm.builder.build_float_ext(fv, tty, "")
                // otherwise
                } else {
                    llvm.builder.build_float_trunc(fv, tty, "")
                }
                .into()
            }
            // pointer to int
            (BasicValueEnum::PointerValue(pv), AnyTypeEnum::IntType(tty)) => {
                llvm.builder.build_ptr_to_int(pv, tty, "").into()
            }
            // int to pointer
            (BasicValueEnum::IntValue(iv), AnyTypeEnum::PointerType(tty)) => {
                llvm.builder.build_int_to_ptr(iv, tty, "").into()
            }
            // pointer to pointer
            (BasicValueEnum::PointerValue(pv), AnyTypeEnum::PointerType(tty)) => {
                llvm.builder.build_bitcast(pv, tty, "")
            }
            _ => panic!("Illegal casting operation"),
        };

        llvm.record(event, &op);
        op
    }
}

impl ast::UnaryOperator {
    fn to_llvm_ir<'ctx>(
        &self,
        llvm: &mut IrGen<'ctx>,
        right: &ast::Expression<SemanticContext>,
        span: Span,
    ) -> BasicValueEnum<'ctx> {
        let event = llvm.new_event(span);
        let is_float = right.get_type().is_float();
        let op = match (self, is_float) {
            (ast::UnaryOperator::Negate, false) => {
                let r = right.to_llvm_ir(llvm).expect("Expected a value");
                let rv = r.into_int_value();
                llvm.builder.build_int_neg(rv, "").into()
            }
            (ast::UnaryOperator::Negate, true) => {
                let r = right.to_llvm_ir(llvm).expect("Expected a value");
                let rv = r.into_float_value();
                llvm.builder.build_float_neg(rv, "").into()
            }
            (ast::UnaryOperator::Not, false) => {
                let r = right.to_llvm_ir(llvm).expect("Expected a value");
                let rv = r.into_int_value();
                llvm.builder.build_not(rv, "").into()
            }
            (ast::UnaryOperator::AddressConst, false) | (ast::UnaryOperator::AddressMut, false) => {
                // get pointer to identifier
                let ptr = right.to_address(llvm).unwrap();
                ptr.into()
            }
            (ast::UnaryOperator::DerefRawPointer, false) => {
                let r = right.to_llvm_ir(llvm).expect("Expected a value");
                let ptr = r.into_pointer_value();
                if ptr.get_type().get_element_type().is_aggregate_type() {
                    ptr.into()
                } else {
                    llvm.builder.build_load(ptr, "")
                }
            }
            _ => panic!("Invalid operator"),
        };

        llvm.record(event, &op);

        op
    }
}

impl ast::BinaryOperator {
    fn to_llvm_ir<'ctx>(
        &self,
        llvm: &mut IrGen<'ctx>,
        left: &ast::Expression<SemanticContext>,
        right: &ast::Expression<SemanticContext>,
        span: Span,
    ) -> BasicValueEnum<'ctx> {
        let event = llvm.new_event(span);
        let is_float = left.get_type().is_float();
        let is_pointer = left.get_type().is_raw_pointer() || right.get_type().is_raw_pointer();
        let l = left.to_llvm_ir(llvm).expect("Expected a value");
        let r = right.to_llvm_ir(llvm).expect("Expected a value");
        let op = if is_float {
            // With the current design, the difference between float and integer arithmetic is
            // a hardware difference and falls squarely within the field of the LLVM generator
            // module.  But this violates the precept that this module makes no decisions and only
            // transcribes exactly what it is given.  Ultimately, I need to capture the notion
            // of operators for each operand type in the language layer.
            let lf = l.into_float_value();
            let rf = r.into_float_value();
            match self {
                ast::BinaryOperator::Add => llvm.builder.build_float_add(lf, rf, "").into(),
                ast::BinaryOperator::Sub => llvm.builder.build_float_sub(lf, rf, "").into(),
                ast::BinaryOperator::Mul => llvm.builder.build_float_mul(lf, rf, "").into(),
                ast::BinaryOperator::Div => llvm.builder.build_float_div(lf, rf, "").into(),
                ast::BinaryOperator::Ls => llvm
                    .builder
                    .build_float_compare(FloatPredicate::OLT, lf, rf, "")
                    .into(),
                ast::BinaryOperator::LsEq => llvm
                    .builder
                    .build_float_compare(FloatPredicate::OLE, lf, rf, "")
                    .into(),
                ast::BinaryOperator::Gr => llvm
                    .builder
                    .build_float_compare(FloatPredicate::OGT, lf, rf, "")
                    .into(),
                ast::BinaryOperator::GrEq => llvm
                    .builder
                    .build_float_compare(FloatPredicate::OGE, lf, rf, "")
                    .into(),
                _ => panic!("Attempting to apply invalid operators to floats"),
            }
        } else if is_pointer {
            let lp = l.into_pointer_value();
            if self == &ast::BinaryOperator::RawPointerOffset {
                let offset = r.into_int_value();
                unsafe { llvm.builder.build_gep(lp, &[offset], "").into() }
            } else {
                let rp = r.into_pointer_value();
                use ast::BinaryOperator::*;
                match self {
                    Eq | NEq | Ls | LsEq | Gr | GrEq => {
                        // figure out diff
                        let li = llvm
                            .builder
                            .build_ptr_to_int(lp, llvm.context.i64_type(), "");
                        let ri = llvm
                            .builder
                            .build_ptr_to_int(rp, llvm.context.i64_type(), "");
                        let predicate = match self {
                            Eq => IntPredicate::EQ,
                            NEq => IntPredicate::NE,
                            Ls => IntPredicate::ULT,
                            LsEq => IntPredicate::ULE,
                            Gr => IntPredicate::UGT,
                            GrEq => IntPredicate::UGE,
                            _ => panic!(),
                        };
                        llvm.builder.build_int_compare(predicate, li, ri, "").into()
                    }
                    _ => panic!(),
                }
            }
        } else {
            let lv = l.into_int_value();
            let rv = r.into_int_value();
            match self {
                ast::BinaryOperator::Add => llvm.builder.build_int_add(lv, rv, "").into(),
                ast::BinaryOperator::Sub => llvm.builder.build_int_sub(lv, rv, "").into(),
                ast::BinaryOperator::Mul => llvm.builder.build_int_mul(lv, rv, "").into(),
                ast::BinaryOperator::Div => {
                    // With the current design, the difference between signed and unsigned division is
                    // a hardware difference and falls squarely within the field of the LLVM generator
                    // module.  But this violates the precept that this module makes no decisions and only
                    // transcribes exactly what it is given.  Ultimately, I need to capture the notion
                    // of operators for each operand type in the language layer; especially when I get
                    // to implementing FP values and operations.
                    if left.get_type().is_unsigned_int() {
                        llvm.builder.build_int_unsigned_div(lv, rv, "")
                    } else {
                        llvm.builder.build_int_signed_div(lv, rv, "")
                    }
                    .into()
                }
                ast::BinaryOperator::BAnd => llvm.builder.build_and(lv, rv, "").into(),
                ast::BinaryOperator::BOr => llvm.builder.build_or(lv, rv, "").into(),
                ast::BinaryOperator::Eq => llvm
                    .builder
                    .build_int_compare(IntPredicate::EQ, lv, rv, "")
                    .into(),
                ast::BinaryOperator::NEq => llvm
                    .builder
                    .build_int_compare(IntPredicate::NE, lv, rv, "")
                    .into(),
                ast::BinaryOperator::Ls => llvm
                    .builder
                    .build_int_compare(IntPredicate::SLT, lv, rv, "")
                    .into(),
                ast::BinaryOperator::LsEq => llvm
                    .builder
                    .build_int_compare(IntPredicate::SLE, lv, rv, "")
                    .into(),
                ast::BinaryOperator::Gr => llvm
                    .builder
                    .build_int_compare(IntPredicate::SGT, lv, rv, "")
                    .into(),
                ast::BinaryOperator::GrEq => llvm
                    .builder
                    .build_int_compare(IntPredicate::SGE, lv, rv, "")
                    .into(),
                ast::BinaryOperator::RawPointerOffset => {
                    panic!("Should be impossible to reach this arm")
                }
            }
        };

        llvm.record(event, &op);

        op
    }
}

impl ast::RoutineCall {
    fn to_label<'ctx>(&self, llvm: &IrGen<'ctx>, target: &ast::Path) -> String {
        match self {
            ast::RoutineCall::Function | ast::RoutineCall::CoroutineInit => {
                target.to_label(llvm.source_map, llvm.string_table)
            }
            ast::RoutineCall::Extern => llvm
                .string_table
                .get(target.item().expect("Extern call must have a target path"))
                .unwrap(),
        }
    }

    /// Determines if the given ReturnType should be converted to an out
    /// parameter (This will be the case for structs).  If the return value
    /// should be returned via out param, then this will return a Some value
    /// with the out param pointer.  If not, this will return None.
    fn to_out_param<'ctx>(
        llvm: &mut IrGen<'ctx>,
        target: &str,
        ret_ty: &Type,
        span: Span,
    ) -> Result<Option<PointerValue<'ctx>>> {
        if llvm.fn_use_out_param.contains(target) {
            let out_ty = ret_ty.to_llvm_ir(llvm)?.into_basic_type().unwrap();

            if !out_ty.is_aggregate_type() {
                panic!("Expected an aggregate type but got {}. Out parameters should only be used with LLVM Aggregate Types (arrays, structs).", ret_ty);
            }

            let ptr = llvm
                .builder
                .build_alloca(out_ty, &format!("_out_{}", target));
            llvm.record_terminal(span, &ptr);
            Ok(Some(ptr))
        } else {
            Ok(None)
        }
    }

    fn to_llvm_ir<'ctx>(
        &self,
        llvm: &mut IrGen<'ctx>,
        target: &ast::Path,
        params: &Vec<ast::Expression<SemanticContext>>,
        ret_ty: &ast::Type,
        span: Span,
    ) -> Result<Option<BasicValueEnum<'ctx>>> {
        match self {
            ast::RoutineCall::Function | ast::RoutineCall::Extern => {
                let event = llvm.new_event(span);
                // Check if the function returns a struct, if it does then create a local struct
                // and pass that as the first parameter
                let fn_name = self.to_label(llvm, target);
                let mut llvm_params: Vec<BasicValueEnum<'ctx>> = Vec::new();

                let out_param = Self::to_out_param(llvm, &fn_name, ret_ty, span)?;

                // If this will use an out param to return the result then
                // add it to the list of parameters for this function.
                if let Some(out_ptr) = out_param {
                    llvm_params.push(out_ptr.into())
                }

                for p in params {
                    let p_llvm = p.to_llvm_ir(llvm).unwrap();
                    llvm_params.push(p_llvm);
                }

                let call = llvm
                    .module
                    .get_function(&fn_name)
                    .unwrap_or_else(|| panic!("Could not find function {}", fn_name));
                let result = llvm.builder.build_call(call, &llvm_params, "result");
                llvm.record(event, &result);
                match out_param {
                    Some(ptr) => Ok(Some(ptr.into())),
                    None => Ok(result.try_as_basic_value().left()),
                }
            }
            ast::RoutineCall::CoroutineInit => todo!("Not yet implemented"),
        }
    }
}

impl ast::Type {
    fn to_llvm_ir<'ctx>(&self, llvm: &IrGen<'ctx>) -> Result<AnyTypeEnum<'ctx>> {
        let llvm_ty = match self {
            ast::Type::Null => panic!("No variable should ever be given the Null type"),
            ast::Type::U8 | ast::Type::I8 => llvm.context.i8_type().into(),
            ast::Type::U16 | ast::Type::I16 => llvm.context.i16_type().into(),
            ast::Type::U32 | ast::Type::I32 => llvm.context.i32_type().into(),
            ast::Type::U64 | ast::Type::I64 => llvm.context.i64_type().into(),
            ast::Type::F64 => llvm.context.f64_type().into(),
            ast::Type::Bool => llvm.context.bool_type().into(),
            ast::Type::Unit => llvm.context.void_type().into(),
            ast::Type::StringLiteral => llvm
                .context
                .i8_type()
                .array_type(0)
                .ptr_type(AddressSpace::Generic)
                .into(),
            ast::Type::Custom(name) => {
                let label = name.to_label(llvm.source_map, llvm.string_table);
                llvm.module
                    .get_struct_type(&label)
                    .unwrap_or_else(|| panic!("Could not find struct {}", label))
                    .into()
            }
            ast::Type::RawPointer(_, ty) => {
                let target_ty = ty.to_llvm_ir(llvm)?;
                let bty = target_ty.into_basic_type().unwrap();
                bty.ptr_type(AddressSpace::Generic).into()
            }
            ast::Type::Array(a, len) => {
                let el_ty = a.to_llvm_ir(llvm)?;
                let len = *len as u32;
                el_ty.into_basic_type().unwrap().array_type(len).into()
            }
            ast::Type::StructDef(_)
            | ast::Type::FunctionDef(_, _)
            | ast::Type::CoroutineDef(_, _)
            | ast::Type::Coroutine(_)
            | ast::Type::ExternDecl(..)
            | ast::Type::Unknown => return Err(format!("Can't convert type to LLVM: {}", self)),
        };
        Ok(llvm_ty)
    }
}

/// Convert any escape senquences to their ascii codes
pub fn convert_esc_seq_to_ascii(s: &str) -> Result<String> {
    let mut is_escape = false;
    let mut escaped_str = String::new();
    for c in s.chars() {
        if c == '\\' {
            is_escape = true;
        } else if !is_escape {
            escaped_str.push(c);
        } else {
            is_escape = false;

            // process escaped character
            match c {
                '\\' => escaped_str.push('\\'),
                'n' => escaped_str.push('\n'),
                'r' => escaped_str.push('\r'),
                't' => escaped_str.push('\t'),
                '0' => escaped_str.push('\0'),
                '"' => escaped_str.push('\"'),
                '\'' => escaped_str.push('\''),
                _ => return Err(format!("Unknown escape sequence \\{}", c)),
            }
        }
    }

    Ok(escaped_str)
}

pub fn get_ptr_alignment(ptr: PointerValue) -> u32 {
    ptr.get_type()
        .get_alignment()
        .get_zero_extended_constant()
        .unwrap_or(MEM_ALIGNMENT) as u32
}

/// Defines helper functions for interacting with LLVM types
pub trait LlvmIsAggregateType {
    /// Returns `true` if the type is an array or a struct
    fn is_aggregate_type(&self) -> bool;
}

impl<'ctx> LlvmIsAggregateType for AnyTypeEnum<'ctx> {
    fn is_aggregate_type(&self) -> bool {
        self.is_array_type() || self.is_struct_type()
    }
}

impl<'ctx> LlvmIsAggregateType for BasicTypeEnum<'ctx> {
    fn is_aggregate_type(&self) -> bool {
        self.is_array_type() || self.is_struct_type()
    }
}

pub trait LlvmToBasicTypeEnum<'ctx> {
    fn into_basic_type(self) -> Result<BasicTypeEnum<'ctx>>;
}

impl<'ctx> LlvmToBasicTypeEnum<'ctx> for AnyTypeEnum<'ctx> {
    fn into_basic_type(self) -> Result<BasicTypeEnum<'ctx>> {
        match self {
            AnyTypeEnum::StructType(st_ty) => Ok(st_ty.into()),
            AnyTypeEnum::IntType(i_ty) => Ok(i_ty.into()),
            AnyTypeEnum::FloatType(f_ty) => Ok(f_ty.into()),
            AnyTypeEnum::PointerType(ptr_ty) => Ok(ptr_ty.into()),
            AnyTypeEnum::ArrayType(a_ty) => Ok(a_ty.into()),
            AnyTypeEnum::VoidType(_) => Err("Cannot convert void to basic type".into()),
            AnyTypeEnum::FunctionType(_) => Err("Cannot convert void to basic type".into()),
            AnyTypeEnum::VectorType(_) => {
                todo!("Not implemented")
            }
        }
    }
}
