/**
   Translate Braid into LLVM IR.
*/
mod llvmir;
mod scopestack;
mod stringpool;

pub use llvmir::IrGen;
