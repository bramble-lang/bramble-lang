#![allow(unused_imports, unused_variables)]

/// The compiler traverses the Braid AST and constructs and constructs
/// an LLVM Module through LLVM IR.

/// This uses the LLVM C API to interface with LLVM and construct the
/// Module. Resulting IR can then be fed into the LLVM Compiler to compile
/// into native assembly or into a JIT.
use std::{collections::HashMap, error::Error};

use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::OptimizationLevel;
use inkwell::{builder::Builder, values::FunctionValue};
use inkwell::{context::Context, values::AnyValue};

use crate::ast::{Annotation, RoutineDef};

/// A LLVM IR generator which can be used to generate all the code
/// for a single LLVM Module.
pub struct IrGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    functions: HashMap<String, FunctionValue<'ctx>>,
}

impl<'ctx> IrGen<'ctx> {
    pub fn new(ctx: &'ctx Context, module: &str) -> IrGen<'ctx> {
        IrGen {
            context: ctx,
            module: ctx.create_module(module),
            builder: ctx.create_builder(),
            functions: HashMap::new(),
        }
    }

    /// Take the given AST
    fn construct_fn_decls<A>(&self, m: &crate::ast::Module<A>) {
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
    fn add_fn_decl<A>(&self, rd: &RoutineDef<A>) {
        let ty = self.context.void_type();
        let params = vec![];
        let fn_type = ty.fn_type(&params, false);
        self.module.add_function(rd.get_name(), fn_type, None);
    }
}

trait ToLlvmIr<'ctx> {
    type Value: inkwell::values::AnyValue<'ctx>;

    // Some things (Modules) don't have a value so amke this an option
    fn to_llvm_ir(&self, llvm: &IrGen<'ctx>) -> Option<Self::Value>; // I think this is a good place for an associated type, if I can specify it must implement the AnyValue trait
}

impl<'ctx, A> ToLlvmIr<'ctx> for crate::ast::Module<A> {
    /// DEFINITELY NOT WHAT i WANT, BUT JUST PUTTING HERE TO GET TO COMPILE
    type Value = FunctionValue<'ctx>;

    fn to_llvm_ir(&self, llvm: &IrGen<'ctx>) -> Option<Self::Value> {
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

impl<'ctx, A> ToLlvmIr<'ctx> for crate::ast::RoutineDef<A> {
    type Value = FunctionValue<'ctx>;

    fn to_llvm_ir(&self, llvm: &IrGen<'ctx>) -> Option<Self::Value> {
        let i64_type = llvm.context.i64_type();
        let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false);
        Some(llvm.module.add_function(&self.name, fn_type, None))
    }
}

/// Support method which generates the declaration for a routine
impl<A> crate::ast::RoutineDef<A> {
    fn get_decl<'ctx>(&self, context: &'ctx Context) -> FunctionValue {
        let unit_ty = context.void_type();
        todo!()
    }
}
