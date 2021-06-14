#![allow(unused_imports, unused_variables)]

/// The compiler traverses the Braid AST and constructs and constructs
/// an LLVM Module through LLVM IR.

/// This uses the LLVM C API to interface with LLVM and construct the
/// Module. Resulting IR can then be fed into the LLVM Compiler to compile
/// into native assembly or into a JIT.
use std::{
    collections::{HashMap, HashSet},
    convert::TryFrom,
    error::Error,
};

use ast::Expression;
use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    module::Module,
    passes::PassManager,
    targets::{CodeModel, InitializationConfig, RelocMode, Target},
    types::*,
    values::*,
    AddressSpace, IntPredicate, OptimizationLevel,
};

use braid_lang::result::Result;

use crate::{
    ast,
    ast::{BinaryOperator, Extern, Node, Parameter, RoutineDef, StructDef},
    semantics::semanticnode::SemanticAnnotations,
};

use super::{scopestack::RegisterLookup, stringpool::StringPool};

const MEM_ALIGNMENT: u64 = 8;

/// A LLVM IR generator which can be used to generate all the code
/// for a single LLVM Module.
pub struct IrGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    externs: &'ctx Vec<(ast::Path, Vec<ast::Type>, ast::Type)>,
    string_pool: StringPool,
    registers: RegisterLookup<'ctx>,
    struct_table: HashMap<String, &'ctx StructDef<SemanticAnnotations>>,
    fn_use_out_param: HashSet<String>,
}

impl<'ctx> IrGen<'ctx> {
    pub fn new(
        ctx: &'ctx Context,
        module: &str,
        externs: &'ctx Vec<(ast::Path, Vec<ast::Type>, ast::Type)>,
    ) -> IrGen<'ctx> {
        IrGen {
            context: ctx,
            module: ctx.create_module(module),
            builder: ctx.create_builder(),
            externs,
            string_pool: StringPool::new(),
            registers: RegisterLookup::new(),
            struct_table: HashMap::new(),
            fn_use_out_param: HashSet::new(),
        }
    }

    /// Print the LLVM IR to stderr
    pub fn print_err(&self) {
        self.module.print_to_stderr();
    }

    /// Print the LLVM IR to the given file
    pub fn print(&self, path: &std::path::Path) {
        self.module.print_to_file(path).unwrap()
    }

    /// Compile the LLVM IR into an object file for the target platform
    pub fn emit_object_code(&self, path: &std::path::Path) -> Result<()> {
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

        // Emit object file
        machine
            .write_to_file(&self.module, inkwell::targets::FileType::Object, path)
            .map_err(|e| e.to_string())
    }

    /// Take the given Braid AST to compile it to LLVM IR and add it to the LLVM module.
    ///
    /// All user input is expected to be fully validated and correct by the time it reaches
    /// the compiler phase (via syntactic and semantic analysis).  Therefore, if anything
    /// goes wrong during compilation, it is assumed to be the result of a critical bug in
    /// the compiler itself and not an issue with the input Braid code. This means that any
    /// error at this stage is unrecoverable; since its a bug in the compiler itself it cannot
    /// be trusted. So, if any unexpected state is encountered or any error happens this module
    /// will panic at that point in code and crash the compiler.
    pub fn ingest(&mut self, m: &'ctx ast::Module<SemanticAnnotations>) {
        self.compile_string_pool(m);
        self.add_externs();
        self.add_mod_items(m);
        self.create_main();
        match m.to_llvm_ir(self) {
            None => (),
            Some(_) => panic!("Expected None when compiling a Module"),
        };
    }

    /// Creates `main` entry point which will be called by the OS to start the Braid
    /// application. This main will initialize platform level values and state, then
    /// call the user defined main `my_main`.
    fn create_main(&self) {
        let main_type = self.context.i64_type().fn_type(&[], false);
        let main = self.module.add_function("main", main_type, None);
        let entry_bb = self.context.append_basic_block(main, "entry");
        self.builder.position_at_end(entry_bb);
        let user_main = self.module.get_function("root_my_main").unwrap();
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
    fn add_externs(&mut self) {
        for (path, params, ty) in self.externs {
            self.add_fn_decl(&path.to_label(), params, ty)
        }
    }

    /// Take the given AST and add declarations for every function to the
    /// LLVM module. This is required so that the FunctionValue can be looked
    /// up when generating code for function calls.
    fn add_mod_items(&mut self, m: &'ctx ast::Module<SemanticAnnotations>) {
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
    fn add_fn_def_decl(&mut self, rd: &'ctx RoutineDef<SemanticAnnotations>) {
        let params = rd.get_params().iter().map(|p| p.ty.clone()).collect();
        self.add_fn_decl(
            &rd.annotations.get_canonical_path().to_label(),
            &params,
            &rd.ty,
        )
    }

    fn add_extern_fn_decl(&mut self, ex: &'ctx Extern<SemanticAnnotations>) {
        // Delcare external function
        let params = ex.get_params().iter().map(|p| p.ty.clone()).collect();
        self.add_fn_decl(
            &ex.annotation().get_canonical_path().to_label(),
            &params,
            ex.get_return_type(),
        );
    }

    /// Takes a tuple describing the signature of an function (internal or external) to the
    /// LLVM Module. This function declaration can then be
    /// looked up through `self.module` for function calls
    /// and to add the definition to the function when
    /// compiling the AST to LLVM.
    fn add_fn_decl(&mut self, name: &str, params: &Vec<ast::Type>, ret_ty: &ast::Type) {
        let mut llvm_params = vec![];

        // If the return type is a structure, then update the function to use
        // a return parameter and make the function a void
        let llvm_ty = match ret_ty {
            ast::Type::Custom(_) | ast::Type::Array(..) => {
                self.fn_use_out_param.insert(name.into());

                let ptr_ty = ret_ty
                    .to_llvm_ir(self)
                    .into_basic_type()
                    .unwrap()
                    .ptr_type(AddressSpace::Generic)
                    .into();
                llvm_params.push(ptr_ty);

                ast::Type::Unit.to_llvm_ir(self)
            }
            _ => ret_ty.to_llvm_ir(self),
        };

        for p in params {
            let ty_llvm = p.to_llvm_ir(self).into_basic_type();
            match ty_llvm {
                Ok(ty_llvm) if ty_llvm.is_aggregate_type() => {
                    llvm_params.push(ty_llvm.ptr_type(AddressSpace::Generic).into())
                }
                Ok(ty_llvm) => llvm_params.push(ty_llvm),
                Err(msg) => panic!("Failed to convert parameter type to LLVM: {}", msg),
            }
        }
        let fn_type = match llvm_ty {
            AnyTypeEnum::IntType(ity) => ity.fn_type(&llvm_params, false),
            AnyTypeEnum::PointerType(pty) => pty.fn_type(&llvm_params, false),
            AnyTypeEnum::VoidType(vty) => vty.fn_type(&llvm_params, false),
            _ => panic!("Unexpected type: {:?}", llvm_ty),
        };
        self.module.add_function(name, fn_type, None);
    }

    /// Add a struct definition to the LLVM context and module.
    fn add_struct_def(&mut self, sd: &'ctx StructDef<SemanticAnnotations>) {
        self.struct_table
            .insert(sd.annotation().get_canonical_path().to_label(), sd);
        let name = sd.annotation().get_canonical_path().to_label();
        let fields_llvm: Vec<BasicTypeEnum<'ctx>> = sd
            .get_fields()
            .iter()
            .filter_map(|f| {
                // TODO: what's going on here?  Should this fail if I cannot convert to a basic type?
                match f.ty {
                    ast::Type::Custom(_) => f.ty.to_llvm_ir(self).into_basic_type(),
                    _ => f.ty.to_llvm_ir(self).into_basic_type(),
                }
                .ok()
            })
            .collect();
        let struct_ty = self.context.opaque_struct_type(&name);
        struct_ty.set_body(&fields_llvm, false);
    }

    /// Add all string literals to the data section of the assemby output
    fn compile_string_pool(&mut self, m: &ast::Module<SemanticAnnotations>) {
        self.string_pool.extract_from_module(m);

        for (s, id) in self.string_pool.pool.iter() {
            let escaped_s = convert_esc_seq_to_ascii(s).unwrap();
            let len_w_null = escaped_s.len() + 1;
            let g = self.module.add_global(
                self.context.i8_type().array_type(len_w_null as u32),
                None,
                &Self::id_to_str_pool_var(*id),
            );
            g.set_initializer(&self.context.const_string(escaped_s.as_bytes(), true));
        }
    }

    fn build_memcpy(&self, dest: PointerValue<'ctx>, src: PointerValue<'ctx>) {
        let dest_align = get_ptr_alignment(dest);
        let src_align = get_ptr_alignment(src);
        self.builder
            .build_memcpy(
                dest,
                dest_align,
                src,
                src_align,
                dest.get_type().get_element_type().size_of().unwrap(),
            )
            .unwrap();
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
            .map(|id| Self::id_to_str_pool_var(*id))
    }

    /// Convert the ID of a string to the name of the global variable that
    /// references that string
    fn id_to_str_pool_var(id: usize) -> String {
        format!("str_{}", id)
    }
}

trait ToLlvmIr<'ctx> {
    type Value: inkwell::values::AnyValue<'ctx>;

    /// Compile a Language unit to LLVM and return the appropriate LLVM Value
    /// if it has one (Modules don't have LLVM Values so those will return None)
    fn to_llvm_ir(&self, llvm: &mut IrGen<'ctx>) -> Option<Self::Value>;
}

impl<'ctx> ToLlvmIr<'ctx> for ast::Module<SemanticAnnotations> {
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

impl<'ctx> ToLlvmIr<'ctx> for ast::RoutineDef<SemanticAnnotations> {
    type Value = FunctionValue<'ctx>;

    fn to_llvm_ir(&self, llvm: &mut IrGen<'ctx>) -> Option<Self::Value> {
        let fn_name = self.annotations.get_canonical_path().to_label();
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
            let pname = &(*self.get_params())[pi - start].name;

            // move parameter into the stack
            let pptr = llvm.builder.build_alloca(llvm_params[pi].get_type(), pname);
            llvm.builder.build_store(pptr, llvm_params[pi]);
            llvm.registers.insert(pname, pptr.into()).unwrap();
        }

        // Compile the body to LLVM
        for stm in &self.body {
            let value = stm.to_llvm_ir(llvm);
        }

        llvm.registers.close_fn().unwrap();

        Some(fn_value)
    }
}

impl<'ctx> ToLlvmIr<'ctx> for ast::Statement<SemanticAnnotations> {
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

impl<'ctx> ToLlvmIr<'ctx> for ast::Bind<SemanticAnnotations> {
    type Value = PointerValue<'ctx>;

    fn to_llvm_ir(&self, llvm: &mut IrGen<'ctx>) -> Option<Self::Value> {
        let rhs = self.get_rhs().to_llvm_ir(llvm).unwrap();

        match self.get_type().to_llvm_ir(llvm).into_basic_type() {
            Ok(ty) if ty.is_aggregate_type() => {
                let ptr = llvm.builder.build_alloca(ty, self.get_id());
                let rhs_ptr = rhs.into_pointer_value();
                llvm.build_memcpy(ptr, rhs_ptr);

                llvm.registers.insert(self.get_id(), ptr.into()).unwrap();
                Some(ptr)
            }
            Ok(ty) => {
                let ptr = llvm.builder.build_alloca(ty, self.get_id());
                llvm.builder.build_store(ptr, rhs);
                llvm.registers.insert(self.get_id(), ptr.into()).unwrap();
                Some(ptr)
            }
            Err(msg) => panic!("Failed to convert to basic type: {}", msg),
        }
    }
}

impl<'ctx> ToLlvmIr<'ctx> for ast::Mutate<SemanticAnnotations> {
    type Value = PointerValue<'ctx>;

    fn to_llvm_ir(&self, llvm: &mut IrGen<'ctx>) -> Option<Self::Value> {
        let rhs = self.get_rhs().to_llvm_ir(llvm).unwrap();
        let v_ptr = llvm
            .registers
            .get(self.get_id())
            .unwrap()
            .into_pointer_value();
        llvm.builder.build_store(v_ptr, rhs);
        Some(v_ptr)
    }
}

impl<'ctx> ToLlvmIr<'ctx> for ast::Return<SemanticAnnotations> {
    type Value = InstructionValue<'ctx>;

    fn to_llvm_ir(&self, llvm: &mut IrGen<'ctx>) -> Option<Self::Value> {
        Some(match self.get_value() {
            None => llvm.builder.build_return(None),
            Some(val) => {
                match val.get_type() {
                    // Instead of type use the table that indicates the out parameter was added
                    // TODO: I think that this can be linked to the `llvm.fn_out_params` table. I do it with Return
                    ast::Type::Custom(_) | ast::Type::Array(..) => {
                        let out = llvm.registers.get(".out").unwrap().into_pointer_value();
                        let src_ptr = val.to_llvm_ir(llvm).unwrap().into_pointer_value();
                        llvm.build_memcpy(out, src_ptr);

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
    }
}

impl<'ctx> ToLlvmIr<'ctx> for ast::Expression<SemanticAnnotations> {
    type Value = BasicValueEnum<'ctx>;

    fn to_llvm_ir(&self, llvm: &mut IrGen<'ctx>) -> Option<Self::Value> {
        match self {
            ast::Expression::U8(_, i) => {
                let u8t = llvm.context.i8_type();
                Some(u8t.const_int(*i as u64, false).into()) // TODO: Is it correct to NOT sign extend for unsigned ints?
            }
            ast::Expression::U16(_, i) => {
                let u16t = llvm.context.i16_type();
                Some(u16t.const_int(*i as u64, false).into()) // TODO: Is it correct to NOT sign extend for unsigned ints?
            }
            ast::Expression::U32(_, i) => {
                let u32t = llvm.context.i32_type();
                Some(u32t.const_int(*i as u64, false).into()) // TODO: Is it correct to NOT sign extend for unsigned ints?
            }
            ast::Expression::U64(_, i) => {
                let u64t = llvm.context.i64_type();
                Some(u64t.const_int(*i as u64, false).into()) // TODO: Is it correct to NOT sign extend for unsigned ints?
            }
            ast::Expression::Integer8(_, i) => {
                let i8t = llvm.context.i8_type();
                Some(i8t.const_int(*i as u64, true).into())
            }
            ast::Expression::Integer16(_, i) => {
                let i16t = llvm.context.i16_type();
                Some(i16t.const_int(*i as u64, true).into())
            }
            ast::Expression::Integer32(_, i) => {
                let i32t = llvm.context.i32_type();
                Some(i32t.const_int(*i as u64, true).into())
            }
            ast::Expression::Integer64(_, i) => {
                let i64t = llvm.context.i64_type();
                Some(i64t.const_int(*i as u64, true).into())
            }
            ast::Expression::Boolean(_, b) => {
                let bt = llvm.context.bool_type();
                Some(bt.const_int(*b as u64, true).into())
            }
            ast::Expression::StringLiteral(_, s) => {
                let str_id = llvm.get_str_var(s).unwrap();
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
                Some(bitcast.into())
            }
            ast::Expression::Identifier(_, id) => {
                let ptr = llvm.registers.get(id).unwrap().into_pointer_value();
                if ptr.get_type().get_element_type().is_aggregate_type() {
                    Some(ptr.into())
                } else {
                    let val = llvm.builder.build_load(ptr, id);
                    Some(val)
                }
            }
            ast::Expression::UnaryOp(_, op, exp) => {
                let v = exp.to_llvm_ir(llvm).expect("Expected a value");
                Some(op.to_llvm_ir(llvm, v))
            }
            ast::Expression::BinaryOp(_, op, l, r) => {
                let left = l.to_llvm_ir(llvm).expect("Expected a value");
                let right = r.to_llvm_ir(llvm).expect("Expected a value");

                // With the current design, the difference between signed and unsigned division is
                // a hardware difference and falls squarely within the field of the LLVM generator
                // module.  But this violates the precept that this module makes no decisions and only
                // transcribes exactly what it is given.  Ultimately, I need to capture the notion
                // of operators for each operand type in the language layer; especially when I get
                // to implementing FP values and operations.
                if *op == BinaryOperator::Div && l.get_type().is_unsigned_int() {
                    let lv = left.into_int_value();
                    let rv = right.into_int_value();
                    Some(llvm.builder.build_int_unsigned_div(lv, rv, "").into())
                } else {
                    Some(op.to_llvm_ir(llvm, left, right))
                }
            }
            ast::Expression::RoutineCall(_, call, name, params) => {
                call.to_llvm_ir(llvm, name, params, self.get_type())
            }
            ast::Expression::ExpressionBlock(_, stmts, exp) => {
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
                let cond_val = cond.to_llvm_ir(llvm).unwrap().into_int_value();
                let current_fn = llvm.get_current_fn().unwrap();
                let then_bb = llvm.context.append_basic_block(current_fn, "then");
                let else_bb = llvm.context.insert_basic_block_after(then_bb, "else");
                let merge_bb = llvm.context.insert_basic_block_after(else_bb, "merge");
                llvm.builder
                    .build_conditional_branch(cond_val, then_bb, else_bb);

                llvm.builder.position_at_end(then_bb);
                let then_arm_val = then_arm.to_llvm_ir(llvm);
                let then_bb = llvm.builder.get_insert_block().unwrap(); // The builders position may change after compiling the then block
                llvm.builder.build_unconditional_branch(merge_bb);

                llvm.builder.position_at_end(else_bb);
                let else_arm_val = else_arm.as_ref().map(|ea| ea.to_llvm_ir(llvm)).flatten();
                let else_bb = llvm.builder.get_insert_block().unwrap(); // The builders position may changing after compiling the else block
                llvm.builder.build_unconditional_branch(merge_bb);

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

                // Emit the logic for checking the while condition
                llvm.builder.build_unconditional_branch(loop_bb);
                llvm.builder.position_at_end(loop_bb);
                // Test the condition and determine if the loop should be terminated
                let cond_val = cond.to_llvm_ir(llvm).unwrap().into_int_value();
                llvm.builder
                    .build_conditional_branch(cond_val, body_bb, after_bb);

                // Emit the code that will evaluate the loop body
                llvm.builder.position_at_end(body_bb);
                body.to_llvm_ir(llvm); // The result of the body is not used for anything so ignore it
                llvm.builder.build_unconditional_branch(loop_bb);

                // Position the LLVM Builder cursor to be immediately after the loop
                llvm.builder.position_at_end(after_bb);

                None
            }
            ast::Expression::MemberAccess(_, val, field) => {
                let sdef = llvm
                    .struct_table
                    .get(&val.get_type().get_path().unwrap().to_label())
                    .unwrap();
                let field_idx = sdef.get_field_idx(field).unwrap();

                let field_idx_llvm = llvm.context.i64_type().const_int(field_idx as u64, true);

                let val_llvm = val.to_llvm_ir(llvm).unwrap().into_pointer_value();
                let field_ptr = llvm
                    .builder
                    .build_struct_gep(val_llvm, field_idx as u32, "")
                    .unwrap();

                // check if the field_ptr element type is an aggregate, if so, return the ptr
                let el_ty = field_ptr.get_type().get_element_type();
                if el_ty.is_aggregate_type() {
                    Some(field_ptr.into())
                } else {
                    let field_val = llvm.builder.build_load(field_ptr, "");
                    Some(field_val)
                }
            }
            ast::Expression::StructExpression(_, name, fields) => {
                let sname = self.annotation().ty().get_path().unwrap().to_label();
                let sdef = llvm
                    .struct_table
                    .get(&sname)
                    .expect(&format!("Cannot find {} in {:?}", sname, llvm.struct_table));
                let sdef_llvm = llvm.module.get_struct_type(&sname).unwrap();
                let s_ptr = llvm.builder.build_alloca(sdef_llvm, "");

                // convert field names to field indexes (order of fields in expression may not
                // be the same as in the defintion)
                let idx_fields: Vec<(usize, &ast::Expression<SemanticAnnotations>)> = fields
                    .iter()
                    .map(|(n, v)| (sdef.get_field_idx(n).unwrap(), v))
                    .collect();

                for (f_idx, e) in idx_fields {
                    let val = e.to_llvm_ir(llvm).unwrap();
                    let fld_ptr = llvm
                        .builder
                        .build_struct_gep(s_ptr, f_idx as u32, "")
                        .unwrap();

                    let el_ty = fld_ptr.get_type().get_element_type();
                    if el_ty.is_aggregate_type() {
                        // TODO: should I do this for all pointer values?
                        let val_ptr = val.into_pointer_value();
                        llvm.build_memcpy(fld_ptr, val_ptr);
                    } else {
                        llvm.builder.build_store(fld_ptr, val);
                    }
                }
                Some(s_ptr.into())
            }
            ast::Expression::ArrayValue(meta, elements, len) => {
                let a_ty = meta.ty();
                let a_llvm_ty = a_ty.to_llvm_ir(llvm).into_basic_type().unwrap();
                let a_ptr = llvm.builder.build_alloca(a_llvm_ty, "");

                // Compute the results for each element of the array value
                let elements_llvm: Vec<_> = elements
                    .iter()
                    .map(|e| e.to_llvm_ir(llvm).unwrap())
                    .collect();

                // Move those results into the elements of the array
                let mut idx = 0;
                let outer_idx = llvm.context.i64_type().const_int(0, false);
                for e in elements_llvm {
                    let llvm_idx = llvm.context.i64_type().const_int(idx, false);
                    let el_ptr =
                        unsafe { llvm.builder.build_gep(a_ptr, &[outer_idx, llvm_idx], "") };
                    let el_ty = el_ptr.get_type().get_element_type();

                    if el_ty.is_aggregate_type() {
                        llvm.build_memcpy(el_ptr, e.into_pointer_value());
                    } else {
                        llvm.builder.build_store(el_ptr, e);
                    }
                    idx += 1;
                }

                // The arch value of this expression is the ptr to the array
                Some(a_ptr.into())
            }
            ast::Expression::ArrayAt {
                annotation: meta,
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

                // Load the value pointed to by GEP and return that
                let el_ty = el_ptr.get_type().get_element_type();
                let el_val = if el_ty.is_aggregate_type() {
                    el_ptr.into()
                } else {
                    llvm.builder.build_load(el_ptr, "").into()
                };

                Some(el_val)
            }
            ast::Expression::CustomType(..) => panic!("CustomType should not be passed to LLVM"),
            ast::Expression::Path(..) => panic!("Path should not be passed to LLVM"),
            ast::Expression::IdentifierDeclare(..) => {
                panic!("IdentifierDelcare nodes should not be passed to LLVM")
            }
            ast::Expression::Yield(..) => panic!("Yield is not yet implemented for LLVM"),
        }
    }
}

impl ast::UnaryOperator {
    fn to_llvm_ir<'ctx>(
        &self,
        llvm: &IrGen<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let rv = right.into_int_value();
        match self {
            ast::UnaryOperator::Minus => llvm.builder.build_int_neg(rv, "").into(),
            ast::UnaryOperator::Not => llvm.builder.build_not(rv, "").into(),
        }
    }
}

impl ast::BinaryOperator {
    fn to_llvm_ir<'ctx>(
        &self,
        llvm: &IrGen<'ctx>,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let lv = left.into_int_value();
        let rv = right.into_int_value();
        match self {
            ast::BinaryOperator::Add => llvm.builder.build_int_add(lv, rv, ""),
            ast::BinaryOperator::Sub => llvm.builder.build_int_sub(lv, rv, ""),
            ast::BinaryOperator::Mul => llvm.builder.build_int_mul(lv, rv, ""),
            ast::BinaryOperator::Div => llvm.builder.build_int_signed_div(lv, rv, ""),
            ast::BinaryOperator::BAnd => llvm.builder.build_and(lv, rv, ""),
            ast::BinaryOperator::BOr => llvm.builder.build_or(lv, rv, ""),
            ast::BinaryOperator::Eq => llvm.builder.build_int_compare(IntPredicate::EQ, lv, rv, ""),
            ast::BinaryOperator::NEq => {
                llvm.builder.build_int_compare(IntPredicate::NE, lv, rv, "")
            }
            ast::BinaryOperator::Ls => {
                llvm.builder
                    .build_int_compare(IntPredicate::SLT, lv, rv, "")
            }
            ast::BinaryOperator::LsEq => {
                llvm.builder
                    .build_int_compare(IntPredicate::SLE, lv, rv, "")
            }
            ast::BinaryOperator::Gr => {
                llvm.builder
                    .build_int_compare(IntPredicate::SGT, lv, rv, "")
            }
            ast::BinaryOperator::GrEq => {
                llvm.builder
                    .build_int_compare(IntPredicate::SGE, lv, rv, "")
            }
        }
        .into()
    }
}

impl ast::RoutineCall {
    fn to_llvm_ir<'ctx>(
        &self,
        llvm: &mut IrGen<'ctx>,
        name: &ast::Path,
        params: &Vec<ast::Expression<SemanticAnnotations>>,
        ret_ty: &ast::Type,
    ) -> Option<BasicValueEnum<'ctx>> {
        match self {
            ast::RoutineCall::Function => {
                // Check if the function returns a struct, if it does then create a local struct
                // and pass that as the first parameter
                let fn_name = name.to_label();
                let mut llvm_params: Vec<BasicValueEnum<'ctx>> = Vec::new();

                let out_param = if llvm.fn_use_out_param.contains(&fn_name) {
                    let out_ty = ret_ty.to_llvm_ir(llvm).into_basic_type().unwrap();
                    if !out_ty.is_aggregate_type() {
                        panic!("Expected an aggregate type but got {}", ret_ty);
                    }

                    let ptr = llvm
                        .builder
                        .build_alloca(out_ty, &format!("_out_{}", fn_name));
                    llvm_params.push(ptr.into());
                    Some(ptr)
                } else {
                    None
                };

                for p in params {
                    let p_llvm = p.to_llvm_ir(llvm).unwrap();
                    llvm_params.push(p_llvm);
                }

                let call = llvm
                    .module
                    .get_function(&fn_name)
                    .expect(&format!("Could not find function {}", fn_name));
                let result = llvm.builder.build_call(call, &llvm_params, "result");
                match out_param {
                    Some(ptr) => Some(ptr.into()),
                    None => result.try_as_basic_value().left(),
                }
            }
            ast::RoutineCall::CoroutineInit => todo!("Not yet implemented"),
        }
    }
}

impl ast::Type {
    fn to_llvm_ir<'ctx>(&self, llvm: &IrGen<'ctx>) -> AnyTypeEnum<'ctx> {
        match self {
            ast::Type::I8 | ast::Type::U8 => llvm.context.i8_type().into(),
            ast::Type::I16 | ast::Type::U16 => llvm.context.i16_type().into(),
            ast::Type::I32 | ast::Type::U32 => llvm.context.i32_type().into(),
            ast::Type::I64 | ast::Type::U64 => llvm.context.i64_type().into(),
            ast::Type::Bool => llvm.context.bool_type().into(),
            ast::Type::Unit => llvm.context.void_type().into(),
            ast::Type::StringLiteral => llvm
                .context
                .i8_type()
                .array_type(0)
                .ptr_type(AddressSpace::Generic)
                .into(),
            ast::Type::Custom(name) => llvm
                .module
                .get_struct_type(&name.to_label())
                .expect(&format!("Could not find struct {}", name))
                .into(),
            ast::Type::Array(a, len) => {
                let el_ty = a.to_llvm_ir(llvm);
                let len = *len as u32;
                el_ty.into_basic_type().unwrap().array_type(len).into()
            }
            ast::Type::StructDef(_)
            | ast::Type::FunctionDef(_, _)
            | ast::Type::CoroutineDef(_, _)
            | ast::Type::Coroutine(_)
            | ast::Type::Unknown => panic!("Can't convert type to LLVM: {}", self),
        }
    }
}

/// Convert any escape senquences to their ascii codes
fn convert_esc_seq_to_ascii(s: &str) -> Result<String> {
    let mut is_escape = false;
    let mut escaped_str = String::new();
    for c in s.chars() {
        if c == '\\' {
            is_escape = true;
        } else {
            if !is_escape {
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
    }

    Ok(escaped_str)
}

fn get_ptr_alignment<'ctx>(ptr: PointerValue<'ctx>) -> u32 {
    ptr.get_type()
        .get_alignment()
        .get_zero_extended_constant()
        .unwrap_or(MEM_ALIGNMENT) as u32
}

/// Defines helper functions for interacting with LLVM types
trait LlvmIsAggregateType {
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

trait LlvmToBasicTypeEnum<'ctx> {
    fn into_basic_type(self) -> Result<BasicTypeEnum<'ctx>>;
}

impl<'ctx> LlvmToBasicTypeEnum<'ctx> for AnyTypeEnum<'ctx> {
    fn into_basic_type(self) -> Result<BasicTypeEnum<'ctx>> {
        match self {
            AnyTypeEnum::StructType(st_ty) => Ok(st_ty.into()),
            AnyTypeEnum::IntType(i_ty) => Ok(i_ty.into()),
            AnyTypeEnum::PointerType(ptr_ty) => Ok(ptr_ty.into()),
            AnyTypeEnum::ArrayType(a_ty) => Ok(a_ty.into()),
            AnyTypeEnum::VoidType(_) => Err("Cannot convert void to basic type".into()),
            AnyTypeEnum::FunctionType(_) => Err("Cannot convert void to basic type".into()),
            AnyTypeEnum::FloatType(_) | AnyTypeEnum::VectorType(_) => {
                todo!("Not implemented")
            }
        }
    }
}
