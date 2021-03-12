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
    values::{AnyValueEnum, BasicValueEnum, FunctionValue, InstructionValue, IntValue},
    AddressSpace,
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
        }
    }

    pub fn print(&self, path: &std::path::Path) {
        self.module.print_to_stderr();
        self.module.print_to_file(path).unwrap()
    }

    pub fn compile(&mut self, m: &'ctx crate::ast::Module<SemanticAnnotations>) {
        self.compile_string_pool(m);
        self.add_externs();
        self.construct_fn_decls(m);
        match m.to_llvm_ir(self) {
            None => (),
            Some(_) => panic!("Expected None when compiling a Module"),
        }
    }

    /// Add the list of external function declarations to the function table
    /// in the LLVM module
    fn add_externs(&self) {
        for (path, params, ty) in self.externs {
            println!("{}", path);
            self.add_extern_decl(&path.to_label(), params, ty)
        }
    }

    /// Take the given AST and add declarations for every function to the
    /// LLVM module. This is required so that the FunctionValue can be looked
    /// up when generating code for function calls.
    fn construct_fn_decls(&self, m: &'ctx crate::ast::Module<SemanticAnnotations>) {
        for f in m.get_functions() {
            if let crate::ast::Item::Routine(rd) = f {
                self.add_fn_decl(rd);
            }
        }

        for m in m.get_modules() {
            self.construct_fn_decls(m);
        }
    }

    /// Takes a RoutineDef and adds its declaration to the
    /// LLVM Module. This function declaration can then be
    /// looked up through `self.module` for function calls
    /// and to add the definition to the function when
    /// compiling the AST to LLVM.
    fn add_fn_decl(&self, rd: &'ctx RoutineDef<SemanticAnnotations>) {
        let ty = self.type_to_llvm(&rd.ty);
        let mut params = vec![];
        for p in rd.get_params() {
            params.push(Self::anytype_to_basictype(&self.type_to_llvm(&p.ty)).unwrap())
        }

        let fn_type = match ty {
            AnyTypeEnum::IntType(ity) => ity.fn_type(&params, false),
            AnyTypeEnum::VoidType(vty) => vty.fn_type(&params, false),
            AnyTypeEnum::PointerType(pty) => pty.fn_type(&params, false),
            _ => panic!(),
        };
        let fn_name = rd.annotations.get_canonical_path().to_label();
        self.module.add_function(&fn_name, fn_type, None);
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
    ) {
        let llvm_ty = self.type_to_llvm(ret_ty);
        let mut llvm_params = vec![];
        for p in params {
            println!("{}", p);
            llvm_params.push(Self::anytype_to_basictype(&self.type_to_llvm(&p)).unwrap())
        }
        let fn_type = match llvm_ty {
            AnyTypeEnum::IntType(ity) => ity.fn_type(&llvm_params, false),
            AnyTypeEnum::VoidType(vty) => vty.fn_type(&llvm_params, false),
            _ => panic!(),
        };
        self.module.add_function(name, fn_type, None);
    }

    /// Add all string literals to the data section of the assemby output
    fn compile_string_pool(&mut self, m: &crate::ast::Module<SemanticAnnotations>) {
        self.string_pool.extract_from_module(m);

        for (s, id) in self.string_pool.pool.iter() {
            let len_w_null = s.len() + 1;
            let g = self.module.add_global(
                self.context.i8_type().array_type(len_w_null as u32),
                None,
                &format!("str_{}", id),
            );
            g.set_initializer(&self.context.const_string(s.as_bytes(), true));
        }
    }

    fn type_to_llvm(&self, ty: &crate::ast::Type) -> AnyTypeEnum<'ctx> {
        let ty = match ty {
            crate::ast::Type::I32 => self.context.i32_type().into(),
            crate::ast::Type::I64 => self.context.i64_type().into(),
            crate::ast::Type::Bool => self.context.bool_type().into(),
            crate::ast::Type::Unit => self.context.custom_width_int_type(1).into(),
            crate::ast::Type::StringLiteral => self
                .context
                .i8_type()
                .array_type(0)
                .ptr_type(AddressSpace::Generic)
                .into(),
            _ => panic!("Can't convert type to LLVM: {}", ty),
        };
        ty
    }

    fn anytype_to_basictype(ty: &AnyTypeEnum<'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        match ty {
            AnyTypeEnum::IntType(ity) => Some(ity.clone().into()),
            AnyTypeEnum::PointerType(pty) => Some(pty.clone().into()),
            _ => None,
        }
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
        llvm.registers.open_fn().unwrap();
        let llvm_params = fn_value.get_params();
        let num_params = self.get_params().len();
        for pi in 0..num_params {
            let pname = &(*self.get_params())[pi].name;
            llvm.registers
                .insert(pname, llvm_params[pi].into())
                .unwrap();
        }

        let entry_bb = llvm.context.append_basic_block(fn_value, "entry");
        llvm.builder.position_at_end(entry_bb);

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
            _ => None,
        }
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
            crate::ast::Expression::Integer64(_, i) => {
                let vt = llvm.context.void_type();
                let i64t = llvm.context.i64_type();
                Some(i64t.const_int(*i as u64, true).into())
            }
            crate::ast::Expression::Boolean(_, b) => {
                let bt = llvm.context.bool_type();
                Some(bt.const_int(*b as u64, false).into())
            }
            crate::ast::Expression::StringLiteral(_, s) => {
                let val = llvm.module.get_global("str_0").unwrap();
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
                let llvm_value = *llvm.registers.get(id).unwrap();
                Some(llvm_value)
            }
            crate::ast::Expression::UnaryOp(_, crate::ast::UnaryOperator::Minus, exp) => {
                let v = exp
                    .to_llvm_ir(llvm)
                    .expect("Expected a value")
                    .into_int_value();
                Some(llvm.builder.build_int_neg(v, "").into())
            }
            crate::ast::Expression::BinaryOp(_, crate::ast::BinaryOperator::Add, l, r) => {
                let lv = l
                    .to_llvm_ir(llvm)
                    .expect("Expected a value")
                    .into_int_value();
                let rv = r
                    .to_llvm_ir(llvm)
                    .expect("Expected a value")
                    .into_int_value();
                Some(llvm.builder.build_int_add(lv, rv, "").into())
            }
            crate::ast::Expression::RoutineCall(_, call, name, params) => {
                let llvm_params: Vec<BasicValueEnum<'ctx>> =
                    params.iter().map(|p| p.to_llvm_ir(llvm).unwrap()).collect();
                let call = llvm.module.get_function(&name.to_label()).unwrap();
                let result = llvm.builder.build_call(call, &llvm_params, "result");
                let result_bv = result.try_as_basic_value().left().unwrap();
                Some(result_bv)
            }
            _ => todo!("{} not implemented yet", self),
        }
    }
}
