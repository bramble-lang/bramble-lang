#![allow(unused_imports, unused_variables)]

/// The compiler traverses the Braid AST and constructs and constructs
/// an LLVM Module through LLVM IR.

/// This uses the LLVM C API to interface with LLVM and construct the
/// Module. Resulting IR can then be fed into the LLVM Compiler to compile
/// into native assembly or into a JIT.
use std::{collections::HashMap, error::Error};

use inkwell::{builder::Builder, values::FunctionValue};
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

    pub fn print(&self) {
        self.module.print_to_stderr()
    }

    pub fn compile<A>(&self, m: &'ctx crate::ast::Module<A>) {
        self.construct_fn_decls(m);
        match m.to_llvm_ir(self) {
            None => (),
            Some(_) => panic!("Expected None when compiling a Module"),
        }
    }

    /// Take the given AST and add declarations for every function to the
    /// LLVM module. This is required so that the FunctionValue can be looked
    /// up when generating code for function calls.
    fn construct_fn_decls<A>(&self, m: &'ctx crate::ast::Module<A>) {
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
    fn add_fn_decl<A>(&self, rd: &'ctx RoutineDef<A>) {
        let ty = self.type_to_llvm(&rd.ty);
        let mut params = vec![];
        for p in rd.get_params() {
            params.push(self.type_to_llvm(&p.ty).into())
        }
        let fn_type = ty.fn_type(&params, false);
        self.module.add_function(rd.get_name(), fn_type, None);
    }

    fn type_to_llvm(&self, ty: &crate::ast::Type) -> IntType<'ctx> {
        let ty = match ty {
            crate::ast::Type::I64 => self.context.i64_type(),
            crate::ast::Type::Bool => self.context.bool_type(),
            _ => panic!("Can't convert type to LLVM: {}", ty),
        };
        ty
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
        let fn_value = llvm
            .module
            .get_function(&self.name)
            .expect("Could not find function");
        let entry_bb = llvm.context.append_basic_block(fn_value, "entry");
        llvm.builder.position_at_end(entry_bb);

        llvm.builder.build_return(None);

        Some(fn_value)
    }
}
