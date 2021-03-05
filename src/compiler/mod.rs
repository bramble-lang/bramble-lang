/**
 * The Compiler takes an AST that has been semantically analyzed and converts
 * it into x64 assembly language.
 *
 * Internally, the Compiler runs through a few precompiler steps which calculate
 * hardware architecture specific data for the program (e.g. assigning byte size
 * to primitive and custom types, computing the stack frame size, etc.). These
 * precompilation steps are handled in the `ast` submodule. The final output
 * of the precompilation stage is an `AST<Scope>`, which is an abstract syntax
 * tree that has all the architecture specific data for each node. This is the
 * last stage where a User error can occur: this stage will detect custom types
 * which have incomputable sizes and return an error to the User. After this
 * stage, the User input is considered correct and compilable and any faults
 * in the actual compilation can only come from a bug in the compiler itself.
 *
 * The Compiler type, which actually generates the x64 assembly, will only accept
 * an `AST<Scope>` as input; because, a tree of this type has been guaranteed to
 * be syntactically, semantically, and architecturally validated.  Once it reaches
 * this point, all information necessary to generate assembly code is already computed
 * and the input source code has been fully validated, so the compiler can translate
 * exactly what it is given.
 *
 * Because of this, if any error happens in the Compiler, we assume that the error
 * cannot be caused by User input; therefore, it must be a bug in the compiler
 * itself: whether the x64 translation, or the semantic layer allowed invalid code,
 * or something else. An error in this stage, then, is always considered critical
 * and unrecoverable, so the policy is to immeidately panic and return to the console
 * from exactly the point the error was first discovered. So that we are as close
 * to the origin as possible.
 *
 * Precompilation Tasks:
 * 1. Compute size of all variables, and custom types.
 * 2. Compute the stack frame size and assign locations within the stack frame for
 * all variables and function parameters.  This will also check for cycles in
 * struct definitions and structs which have infinite size.
 * 3. Build a global table of all custom defined types and compute the size of
 * all fields and assign relative offsets within the memory assigned to the struct
 * for those fields to reside.
 * 4. Construct a string pool of all string literals.
 */
pub mod compiler;

mod arch;
mod llvm;
mod memory;
mod x86;
