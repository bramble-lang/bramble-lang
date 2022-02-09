mod import;
/**
   Translate Bramble into LLVM IR.
*/
mod llvmir;
mod scopestack;
mod stringpool;
mod writable;

use super::ast;
pub use llvmir::IrGen;
