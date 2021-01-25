# User Input
## Problem
Be able to read input from the user.

## Functional Requirements
- Use Braid function semantics to make the call to read from the user
- Be able to define the function in assembly so that I can make syscalls etc.
- But provide the user with a Braid function to call
- Do not define the implementation of the IO function using the `assembly!` macro
within the compiler
- There must be a separate file containing the implementation of input that the compiler
imports for compilation.

## Design
### Changes to the layers
1. Lexer - requires no changes as no new tokens will be added
2. Parser - no changes, there is no syntax, the IO calls will use the existing function
and module path syntax.  For now, we will not solve importing modules, instead we will assume
that these modules are universal.
3. Semantics - Needs a list of the canonical function names its type (parameters and return
type) because this stage validates that any function that is called: exists and that the
parameters being passed match the functions type and then uses the return type of teh function
for further type checking.
4. Compiler:
    - Memory Layout - no changes: if the implementation is in assemby then all the memory layout
parameters would already be resolved.
    - Compiler - needs the assembly implementation of each function so that it can include it in
    the generated file.  OR, actually, can I just assembly the std assembly files separately then
    link them together? In which case, I think I will need the ASM names of the functions so that
    I can put `extern <fname>` at the top of the generated ASM file.
    - Compiler - I think this does function look up to get the list of parameters, but only during
    the compilation of the function definition (so that it moves the parameters into the stack
    frame)