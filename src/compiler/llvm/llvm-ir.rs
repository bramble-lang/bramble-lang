/**
The compiler traverses the Braid AST and constructs and constructs
an LLVM Module through LLVM IR.

This uses the LLVM C API to interface with LLVM and construct the
Module. Resulting IR can then be fed into the LLVM Compiler to compile
into native assembly or into a JIT.
*/

/// A LLVM IR generator which can be used to generate all the code
/// for a single LLVM Module.
struct IrGen<'ctx> {}
