/**
 * Tools which model the x86 assembly language to aid with writing the compiler logic that translates the AST to
 * x86 assembly code.
 * 
 * The x86 assembly instructions and architecture (registers, etc) are models using Enums.
 * 
 * The core of this module is the assembly macro which construtcs an x86 DSL that allows the user to write x86
 * assembly code directly in their Rust logic and the macro will check for both syntactic and semantic soundness.
 */

 pub mod assembly;