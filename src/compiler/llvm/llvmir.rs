#![allow(unused_imports, unused_variables)]

/// The compiler traverses the Braid AST and constructs and constructs
/// an LLVM Module through LLVM IR.

/// This uses the LLVM C API to interface with LLVM and construct the
/// Module. Resulting IR can then be fed into the LLVM Compiler to compile
/// into native assembly or into a JIT.
use std::{collections::HashMap, error::Error};

use inkwell::{
    builder::Builder,
    types::FunctionType,
    values::{
        AnyValueEnum, BasicValueEnum, FunctionValue, InstructionValue, IntValue, PointerValue,
    },
    AddressSpace, IntPredicate,
};
use inkwell::{context::Context, values::AnyValue};
use inkwell::{
    execution_engine::{ExecutionEngine, JitFunction},
    types::AnyType,
};
use inkwell::{module::Module, types::AnyTypeEnum};
use inkwell::{
    targets::{InitializationConfig, Target},
    types::IntType,
};
use inkwell::{types::BasicTypeEnum, OptimizationLevel};

use crate::{
    ast::{Annotation, RoutineDef},
    compiler::memory::{scope::CompilerAnnotation, stringpool::StringPool},
    semantics::semanticnode::SemanticAnnotations,
};
use braid_lang::result::Result;

use super::scopestack::RegisterLookup;

/// A LLVM IR generator which can be used to generate all the code
/// for a single LLVM Module.
pub struct IrGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    externs: &'ctx Vec<(crate::ast::Path, Vec<crate::ast::Type>, crate::ast::Type)>,
    string_pool: StringPool,
    registers: RegisterLookup<'ctx>,
    failed: bool,
}

impl<'ctx> IrGen<'ctx> {
    pub fn new(
        ctx: &'ctx Context,
        module: &str,
        externs: &'ctx Vec<(crate::ast::Path, Vec<crate::ast::Type>, crate::ast::Type)>,
    ) -> IrGen<'ctx> {
        IrGen {
            context: ctx,
            module: ctx.create_module(module),
            builder: ctx.create_builder(),
            externs,
            string_pool: StringPool::new(),
            registers: RegisterLookup::new(),
            failed: false,
        }
    }

    pub fn print_err(&self) -> Result<()> {
        if self.failed {
            Err("Cannot print IR: LLVM compilation failed".into())
        } else {
            self.module.print_to_stderr();
            Ok(())
        }
    }

    pub fn print(&self, path: &std::path::Path) -> Result<()> {
        if self.failed {
            Err("Cannot print IR: LLVM compilation failed".into())
        } else {
            self.module
                .print_to_file(path)
                .or_else(|e| Err(e.to_string()))
        }
    }

    pub fn compile(&mut self, m: &'ctx crate::ast::Module<SemanticAnnotations>) -> Result<()> {
        let mut internal = || {
            self.compile_string_pool(m);
            self.add_externs()?;
            self.construct_fn_decls(m)?;
            self.create_main()?;
            match m.to_llvm_ir(self) {
                None => Ok(()),
                Some(_) => Err("Expected None when compiling a Module".into()),
            }
        };
        match internal() {
            Ok(()) => Ok(()),
            Err(msg) => {
                self.failed = true;
                Err(msg)
            }
        }
    }

    fn create_main(&self) -> Result<()> {
        let main_type = self.context.i64_type().fn_type(&[], false);
        let main = self.module.add_function("main", main_type, None);
        let entry_bb = self.context.append_basic_block(main, "entry");
        self.builder.position_at_end(entry_bb);
        let user_main = self
            .module
            .get_function("root_my_main")
            .ok_or("No my_main function found")?;
        let status = self
            .builder
            .build_call(user_main, &[], "user_main")
            .try_as_basic_value()
            .left()
            .ok_or("Invalid return type associated with my_main function")?;
        self.builder.build_return(Some(&status));
        Ok(())
    }

    /// Add the list of external function declarations to the function table
    /// in the LLVM module
    fn add_externs(&self) -> Result<()> {
        for (path, params, ty) in self.externs {
            self.add_extern_decl(&path.to_label(), params, ty)?
        }
        Ok(())
    }

    /// Take the given AST and add declarations for every function to the
    /// LLVM module. This is required so that the FunctionValue can be looked
    /// up when generating code for function calls.
    fn construct_fn_decls(&self, m: &'ctx crate::ast::Module<SemanticAnnotations>) -> Result<()> {
        for f in m.get_functions() {
            if let crate::ast::Item::Routine(rd) = f {
                self.add_fn_decl(rd)?;
            }
        }

        for m in m.get_modules() {
            self.construct_fn_decls(m)?;
        }

        Ok(())
    }

    /// Takes a RoutineDef and adds its declaration to the
    /// LLVM Module. This function declaration can then be
    /// looked up through `self.module` for function calls
    /// and to add the definition to the function when
    /// compiling the AST to LLVM.
    fn add_fn_decl(&self, rd: &'ctx RoutineDef<SemanticAnnotations>) -> Result<()> {
        let mut params = vec![];
        for p in rd.get_params() {
            params.push(p.ty.to_llvm(self))
        }

        let ty = rd.ty.to_llvm(self);
        let fn_type = match ty {
            BasicTypeEnum::IntType(ity) => ity.fn_type(&params, false),
            BasicTypeEnum::PointerType(pty) => pty.fn_type(&params, false),
            _ => return Err(format!("Cannot create function for: {}", rd.ty)),
        };

        let fn_name = rd.annotations.get_canonical_path().to_label();
        self.module.add_function(&fn_name, fn_type, None);

        Ok(())
    }

    /// Takes a tuple describing the signature of an extern and adds its declaration to the
    /// LLVM Module. This function declaration can then be
    /// looked up through `self.module` for function calls
    /// and to add the definition to the function when
    /// compiling the AST to LLVM.
    fn add_extern_decl(
        &self,
        name: &str,
        params: &Vec<crate::ast::Type>,
        ret_ty: &crate::ast::Type,
    ) -> Result<()> {
        let mut llvm_params = vec![];
        for p in params {
            println!("{}", p);
            llvm_params.push(p.to_llvm(self))
        }

        let llvm_ty = ret_ty.to_llvm(self);
        let fn_type = match llvm_ty {
            BasicTypeEnum::IntType(ity) => ity.fn_type(&llvm_params, false),
            _ => return Err(format!("Cannot declare function of type: {}", ret_ty)),
        };
        self.module.add_function(name, fn_type, None);
        Ok(())
    }

    /// Add all string literals to the data section of the assemby output
    fn compile_string_pool(&mut self, m: &crate::ast::Module<SemanticAnnotations>) {
        self.string_pool.extract_from_module(m);

        for (s, id) in self.string_pool.pool.iter() {
            let len_w_null = s.len() + 1;
            let g = self.module.add_global(
                self.context.i8_type().array_type(len_w_null as u32),
                None,
                &Self::get_pooled_str_name(*id),
            );
            g.set_initializer(&self.context.const_string(s.as_bytes(), true));
        }
    }

    /// Given an ID number for a string in the string pool, return the name
    /// of the global variable associated with that string.
    fn get_pooled_str_name(id: usize) -> String {
        format!("str_{}", id)
    }

    /// Will look for `s` in the string pool, if found, it will return the
    /// name of the global variable that is bound to that string. Otherwise,
    /// it will return `None`
    fn get_str_id(&self, s: &str) -> Option<String> {
        self.string_pool
            .get(s)
            .map(|id| Self::get_pooled_str_name(*id))
    }
}

trait ToLlvmIr<'ctx> {
    type Value: inkwell::values::AnyValue<'ctx>;

    /// Compile a Language unit to LLVM and return the appropriate LLVM Value
    /// if it has one (Modules don't have LLVM Values so those will return None)
    fn to_llvm_ir(&self, llvm: &mut IrGen<'ctx>) -> Option<Self::Value>;
}

impl<'ctx> ToLlvmIr<'ctx> for crate::ast::Module<SemanticAnnotations> {
    type Value = FunctionValue<'ctx>;

    fn to_llvm_ir(&self, llvm: &mut IrGen<'ctx>) -> Option<Self::Value> {
        for m in self.get_modules() {
            m.to_llvm_ir(llvm);
        }
        for s in self.get_structs() {}
        for f in self.get_functions() {
            if let crate::ast::Item::Routine(rdef) = f {
                let fn_val = rdef
                    .to_llvm_ir(llvm)
                    .expect("Expected Function Value from RoutineDef");
            }
        }
        for c in self.get_coroutines() {}

        None
    }
}

impl<'ctx> ToLlvmIr<'ctx> for crate::ast::RoutineDef<SemanticAnnotations> {
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

impl<'ctx> ToLlvmIr<'ctx> for crate::ast::Statement<SemanticAnnotations> {
    type Value = AnyValueEnum<'ctx>;

    fn to_llvm_ir(&self, llvm: &mut IrGen<'ctx>) -> Option<Self::Value> {
        match self {
            crate::ast::Statement::Return(ret) => ret.to_llvm_ir(llvm).map(|i| i.into()),
            crate::ast::Statement::Expression(exp) => exp.to_llvm_ir(llvm).map(|v| v.into()),
            crate::ast::Statement::Bind(bind) => bind.to_llvm_ir(llvm).map(|i| i.into()),
            _ => None,
        }
    }
}

impl<'ctx> ToLlvmIr<'ctx> for crate::ast::Bind<SemanticAnnotations> {
    type Value = PointerValue<'ctx>;

    fn to_llvm_ir(&self, llvm: &mut IrGen<'ctx>) -> Option<Self::Value> {
        let ptr = llvm
            .builder
            .build_alloca(self.get_type().to_llvm(llvm), self.get_id());
        let rhs = self.get_rhs().to_llvm_ir(llvm).unwrap();
        llvm.builder.build_store(ptr, rhs);
        llvm.registers.insert(self.get_id(), ptr.into()).unwrap();
        Some(ptr)
    }
}

impl<'ctx> ToLlvmIr<'ctx> for crate::ast::Return<SemanticAnnotations> {
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

impl<'ctx> ToLlvmIr<'ctx> for crate::ast::Expression<SemanticAnnotations> {
    type Value = BasicValueEnum<'ctx>;

    fn to_llvm_ir(&self, llvm: &mut IrGen<'ctx>) -> Option<Self::Value> {
        match self {
            crate::ast::Expression::Integer32(_, i) => {
                let i32t = llvm.context.i32_type();
                Some(i32t.const_int(*i as u64, true).into())
            }
            crate::ast::Expression::Integer64(_, i) => {
                let i64t = llvm.context.i64_type();
                Some(i64t.const_int(*i as u64, true).into())
            }
            crate::ast::Expression::Boolean(_, b) => {
                let bt = llvm.context.bool_type();
                Some(bt.const_int(*b as u64, false).into())
            }
            crate::ast::Expression::StringLiteral(_, s) => {
                let str_id = llvm.get_str_id(s).unwrap();
                let val = llvm.module.get_global(&str_id).unwrap();
                let val_ptr = val.as_pointer_value();
                let bitcast = llvm.builder.build_bitcast(
                    val_ptr,
                    llvm.context
                        .i8_type()
                        .array_type(0)
                        .ptr_type(AddressSpace::Generic),
                    "sptr",
                );
                Some(bitcast.into())
            }
            crate::ast::Expression::Identifier(_, id) => {
                let ptr = llvm.registers.get(id).unwrap().into_pointer_value();
                let val = llvm.builder.build_load(ptr, id);
                Some(val)
            }
            crate::ast::Expression::UnaryOp(_, op, exp) => {
                let v = exp
                    .to_llvm_ir(llvm)
                    .expect("Expected a value")
                    .into_int_value();
                Some(match op {
                    crate::ast::UnaryOperator::Minus => llvm.builder.build_int_neg(v, "").into(),
                    crate::ast::UnaryOperator::Not => llvm.builder.build_not(v, "").into(),
                })
            }
            crate::ast::Expression::BinaryOp(_, op, l, r) => {
                let lv = l
                    .to_llvm_ir(llvm)
                    .expect("Expected a value")
                    .into_int_value();
                let rv = r
                    .to_llvm_ir(llvm)
                    .expect("Expected a value")
                    .into_int_value();
                Some(match op {
                    crate::ast::BinaryOperator::Add => {
                        llvm.builder.build_int_add(lv, rv, "").into()
                    }
                    crate::ast::BinaryOperator::Sub => {
                        llvm.builder.build_int_sub(lv, rv, "").into()
                    }
                    crate::ast::BinaryOperator::Mul => {
                        llvm.builder.build_int_mul(lv, rv, "").into()
                    }
                    crate::ast::BinaryOperator::Div => {
                        llvm.builder.build_int_signed_div(lv, rv, "").into()
                    }
                    crate::ast::BinaryOperator::BAnd => llvm.builder.build_and(lv, rv, "").into(),
                    crate::ast::BinaryOperator::BOr => llvm.builder.build_or(lv, rv, "").into(),
                    crate::ast::BinaryOperator::Eq => llvm
                        .builder
                        .build_int_compare(IntPredicate::EQ, lv, rv, "")
                        .into(),
                    crate::ast::BinaryOperator::NEq => llvm
                        .builder
                        .build_int_compare(IntPredicate::NE, lv, rv, "")
                        .into(),
                    crate::ast::BinaryOperator::Ls => llvm
                        .builder
                        .build_int_compare(IntPredicate::SLT, lv, rv, "")
                        .into(),
                    crate::ast::BinaryOperator::LsEq => llvm
                        .builder
                        .build_int_compare(IntPredicate::SLE, lv, rv, "")
                        .into(),
                    crate::ast::BinaryOperator::Gr => llvm
                        .builder
                        .build_int_compare(IntPredicate::SGT, lv, rv, "")
                        .into(),
                    crate::ast::BinaryOperator::GrEq => llvm
                        .builder
                        .build_int_compare(IntPredicate::SGE, lv, rv, "")
                        .into(),
                })
            }
            crate::ast::Expression::RoutineCall(_, call, name, params) => {
                let llvm_params: Vec<BasicValueEnum<'ctx>> =
                    params.iter().map(|p| p.to_llvm_ir(llvm).unwrap()).collect();
                let call = llvm.module.get_function(&name.to_label()).unwrap();
                let result = llvm.builder.build_call(call, &llvm_params, "result");
                let result_bv = result.try_as_basic_value().left().unwrap();
                Some(result_bv)
            }
            crate::ast::Expression::ExpressionBlock(_, stmts, exp) => {
                llvm.registers.open_local().unwrap();
                for stmt in stmts {
                    stmt.to_llvm_ir(llvm).unwrap();
                }
                let val = exp.as_ref().map(|e| e.to_llvm_ir(llvm)).flatten();
                llvm.registers.close_local().unwrap();
                val
            }
            crate::ast::Expression::If {
                cond,
                if_arm,
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
                let if_arm_val = if_arm.to_llvm_ir(llvm);
                llvm.builder.build_unconditional_branch(merge_bb);

                llvm.builder.position_at_end(else_bb);
                let else_arm_val = else_arm.as_ref().map(|ea| ea.to_llvm_ir(llvm).unwrap());
                llvm.builder.build_unconditional_branch(merge_bb);

                llvm.builder.position_at_end(merge_bb);

                match (if_arm_val, else_arm_val) {
                    (Some(if_arm_val), Some(else_arm_val)) => {
                        // create phi to unify the branches
                        let phi = llvm.builder.build_phi(if_arm_val.get_type(), "phi");
                        phi.add_incoming(&[(&if_arm_val, then_bb), (&else_arm_val, else_bb)]);
                        Some(phi.as_basic_value())
                    }
                    (None, None) => None,
                    _ => panic!(
                        "Mismatching arms on if expression: {:?}, {:?}",
                        if_arm_val, else_arm_val
                    ),
                }
            }
            _ => todo!("{} not implemented yet", self),
            /*
            crate::ast::Expression::CustomType(_, _) => {}
            crate::ast::Expression::Path(_, _) => {}
            crate::ast::Expression::MemberAccess(_, _, _) => {}
            crate::ast::Expression::IdentifierDeclare(_, _, _) => {}
            crate::ast::Expression::StructExpression(_, _, _) => {}
            crate::ast::Expression::Yield(_, _) => {}
            */
        }
    }
}

impl crate::ast::Type {
    fn to_llvm<'ctx>(&self, llvm: &IrGen<'ctx>) -> BasicTypeEnum<'ctx> {
        match self {
            crate::ast::Type::I32 => llvm.context.i32_type().into(),
            crate::ast::Type::I64 => llvm.context.i64_type().into(),
            crate::ast::Type::Bool => llvm.context.bool_type().into(),
            crate::ast::Type::Unit => llvm.context.custom_width_int_type(1).into(),
            crate::ast::Type::StringLiteral => llvm
                .context
                .i8_type()
                .array_type(0)
                .ptr_type(AddressSpace::Generic)
                .into(),
            _ => panic!("Can't convert type to LLVM: {}", self),
        }
    }
}
