#![allow(unused_imports, unused_variables)]

/// The compiler traverses the Braid AST and constructs and constructs
/// an LLVM Module through LLVM IR.

/// This uses the LLVM C API to interface with LLVM and construct the
/// Module. Resulting IR can then be fed into the LLVM Compiler to compile
/// into native assembly or into a JIT.
use std::{collections::HashMap, convert::TryFrom, error::Error};

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
    ast::{Node, Parameter, RoutineDef, StructDef},
    compiler::memory::stringpool::StringPool,
    semantics::semanticnode::SemanticAnnotations,
};

use super::scopestack::RegisterLookup;

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
    fn add_externs(&self) {
        for (path, params, ty) in self.externs {
            self.add_extern_decl(&path.to_label(), params, ty)
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
                self.add_fn_decl(rd);
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
    fn add_fn_decl(&self, rd: &'ctx RoutineDef<SemanticAnnotations>) {
        let ty = rd.ty.to_llvm(self);
        let mut params: Vec<BasicTypeEnum<'ctx>> = vec![];
        for p in rd.get_params() {
            // Pass structs around by reference
            let p_ty = anytype_to_basictype(p.ty.to_llvm(self));

            match p_ty {
                Some(p_ty) => params.push(p_ty),
                None => (),
            }
        }

        let fn_type = match ty {
            AnyTypeEnum::IntType(ity) => ity.fn_type(&params, false),
            AnyTypeEnum::PointerType(pty) => pty.fn_type(&params, false),
            AnyTypeEnum::VoidType(vty) => vty.fn_type(&params, false),
            _ => panic!("Unexpected type {:?}", ty),
        };
        let fn_name = rd.annotations.get_canonical_path().to_label();
        self.module.add_function(&fn_name, fn_type, None);
    }

    /// Takes a tuple describing the signature of an extern and adds its declaration to the
    /// LLVM Module. This function declaration can then be
    /// looked up through `self.module` for function calls
    /// and to add the definition to the function when
    /// compiling the AST to LLVM.
    fn add_extern_decl(&self, name: &str, params: &Vec<ast::Type>, ret_ty: &ast::Type) {
        let llvm_ty = ret_ty.to_llvm(self);
        let mut llvm_params = vec![];
        for p in params {
            let ty_llvm = anytype_to_basictype(p.to_llvm(self));
            match ty_llvm {
                Some(ty_llvm) => llvm_params.push(ty_llvm),
                None => (),
            }
        }
        let fn_type = match llvm_ty {
            AnyTypeEnum::IntType(ity) => ity.fn_type(&llvm_params, false),
            AnyTypeEnum::VoidType(vty) => vty.fn_type(&llvm_params, false),
            _ => panic!("Unexpected type: {:?}", llvm_ty),
        };
        self.module.add_function(name, fn_type, None);
    }

    /// Add a struct definition to the LLVM context
    fn add_struct_def(&mut self, sd: &'ctx StructDef<SemanticAnnotations>) {
        self.struct_table
            .insert(sd.annotation().get_canonical_path().to_label(), sd);
        let name = sd.annotation().get_canonical_path().to_label();
        let fields_llvm: Vec<BasicTypeEnum<'ctx>> = sd
            .get_fields()
            .iter()
            .filter_map(|f| anytype_to_basictype(f.ty.to_llvm(self)))
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
        let fn_value = llvm
            .module
            .get_function(&self.annotations.get_canonical_path().to_label())
            .expect("Could not find function");
        let entry_bb = llvm.context.append_basic_block(fn_value, "entry");
        llvm.builder.position_at_end(entry_bb);

        llvm.registers.open_fn().unwrap();
        let llvm_params = fn_value.get_params();
        let num_params = self.get_params().len();
        for pi in 0..num_params {
            let pname = &(*self.get_params())[pi].name;

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

        match anytype_to_basictype(self.get_type().to_llvm(llvm)) {
            Some(ty) => {
                let ptr = llvm.builder.build_alloca(ty, self.get_id());
                llvm.builder.build_store(ptr, rhs);
                llvm.registers.insert(self.get_id(), ptr.into()).unwrap();
                Some(ptr)
            }
            None => None,
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
                let val = val
                    .to_llvm_ir(llvm)
                    .expect("Return expression did not compile to an LLVM value");
                llvm.builder.build_return(Some(&val))
            }
        })
    }
}

impl<'ctx> ToLlvmIr<'ctx> for ast::Expression<SemanticAnnotations> {
    type Value = BasicValueEnum<'ctx>;

    fn to_llvm_ir(&self, llvm: &mut IrGen<'ctx>) -> Option<Self::Value> {
        match self {
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
                let val = llvm.builder.build_load(ptr, id);
                Some(val)
            }
            ast::Expression::UnaryOp(_, op, exp) => {
                let v = exp.to_llvm_ir(llvm).expect("Expected a value");
                Some(op.to_llvm(llvm, v))
            }
            ast::Expression::BinaryOp(_, op, l, r) => {
                let left = l.to_llvm_ir(llvm).expect("Expected a value");
                let right = r.to_llvm_ir(llvm).expect("Expected a value");
                Some(op.to_llvm(llvm, left, right))
            }
            ast::Expression::RoutineCall(_, call, name, params) => {
                let llvm_params: Vec<BasicValueEnum<'ctx>> =
                    params.iter().map(|p| p.to_llvm_ir(llvm).unwrap()).collect();
                let call = llvm.module.get_function(&name.to_label()).unwrap();
                let result = llvm.builder.build_call(call, &llvm_params, "result");
                result.try_as_basic_value().left()
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
                let current_fn = llvm
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
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
                let field_val = llvm.builder.build_load(field_ptr, "");
                Some(field_val)
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
                    llvm.builder.build_store(fld_ptr, val);
                }
                Some(s_ptr.into())
            }
            _ => todo!("{} not implemented yet", self),
            /*
            ast::Expression::CustomType(_, _) => {}
            ast::Expression::Path(_, _) => {}
            ast::Expression::MemberAccess(_, _, _) => {}
            ast::Expression::IdentifierDeclare(_, _, _) => {}
            ast::Expression::StructExpression(_, _, _) => {}
            ast::Expression::Yield(_, _) => {}
            */
        }
    }
}

impl ast::UnaryOperator {
    fn to_llvm<'ctx>(
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
    fn to_llvm<'ctx>(
        &self,
        llvm: &IrGen<'ctx>,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let lv = left.into_int_value();
        let rv = right.into_int_value();

        match self {
            ast::BinaryOperator::Add => llvm.builder.build_int_add(lv, rv, "").into(),
            ast::BinaryOperator::Sub => llvm.builder.build_int_sub(lv, rv, "").into(),
            ast::BinaryOperator::Mul => llvm.builder.build_int_mul(lv, rv, "").into(),
            ast::BinaryOperator::Div => llvm.builder.build_int_signed_div(lv, rv, "").into(),
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
        }
    }
}

impl ast::Type {
    fn to_llvm<'ctx>(&self, llvm: &IrGen<'ctx>) -> AnyTypeEnum<'ctx> {
        match self {
            ast::Type::I32 => llvm.context.i32_type().into(),
            ast::Type::I64 => llvm.context.i64_type().into(),
            ast::Type::Bool => llvm.context.bool_type().into(),
            //ast::Type::Unit => llvm.context.custom_width_int_type(1).into(), // TODOC: Hacky place holder, need to get the right solution in here before merging
            ast::Type::Unit => llvm.context.void_type().into(), // TODOC: Hacky place holder, need to get the right solution in here before merging
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
                .ptr_type(AddressSpace::Generic)
                .into(),
            _ => panic!("Can't convert type to LLVM: {}", self),
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
                    _ => return Err(format!("Unknown escape sequence \\{}", c)),
                }
            }
        }
    }

    Ok(escaped_str)
}

fn anytype_to_basictype<'ctx>(any_ty: AnyTypeEnum<'ctx>) -> Option<BasicTypeEnum<'ctx>> {
    match any_ty {
        AnyTypeEnum::StructType(st_ty) => Some(st_ty.into()),
        AnyTypeEnum::IntType(i_ty) => Some(i_ty.into()),
        AnyTypeEnum::PointerType(ptr_ty) => Some(ptr_ty.into()),
        AnyTypeEnum::VoidType(_) => None,
        AnyTypeEnum::ArrayType(_)
        | AnyTypeEnum::FloatType(_)
        | AnyTypeEnum::FunctionType(_)
        | AnyTypeEnum::VectorType(_) => todo!("Not implemented"),
    }
}
