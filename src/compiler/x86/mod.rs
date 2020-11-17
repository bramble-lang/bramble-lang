/*!
 # x86
 ## About
 This module provides a DSL that models the x86 assembly language and
 certain architecural aspects of how Braid code is translated to x86
 assembly.
 
 There are two core macros `assembly!` and `assembly2!`.  The difference
 is that `assembly2!` also takes a `FunctionInfo` parameter that provides
 scope information about the function being compiled.  This lets
 `assembly2` gracefully handle certain critical compilation requirements
 for the user.

 ## Example
 ```no_run
 assembly!{(code){
     mov %eax, %ebx;
     add %eax, %eax;
     push %eax;
 }}
 ```
 
 ## Assembly2 features:
 1. Unique local labels.  In x86 local labels are labels which begin with
    a `.`, they are local to a scope defined by the range of instructions 
    between two global labels (those without a `.` prefix). Local labels are
    used for compiling control structures (if's, loops, yields, etc). When
    any control structure is compiled it will dynmically generate the local
    labels it needs to function; to make sure that each control structure
    works its local labels must all be unique to it and no other control
    structure should use its labels. To provide that uniqueness, each time
    a local label is created it is given a monotonically increasing integer
    id postfix (.e.g `.loop_begin_0` and `.loop_end_0` and `.else_12`).
    
    `assembly2!` will handle generating that ID number and adding it as a post
    fix to the local label.  So instead of writing `^{format!("lbl_{}", id)}`
    you write `^lbl` and `assembly2!` will make sure that the output code is
    `.lbl_20`.
 
 2. Local labels must be unique within a macro scrope. The compiler will
    throw a runtime panic if the same local label is defined twice within a single
    `assembly2` scope..  Within a single macro scope (`asembly2!{(a,b){...macro scope...}}`)
    every local label which is defined must be unique.  Otherwise, `assembly2!`
    will not be able to properly tag each label with its ID and the x86 
    assembly output will be broken.

## Syntax
1. `%<register>` - The `%` prefix indicates that the next token will be a register.
2. `{expression}` - This is an expression which will be evaluated and whose result
    will be used in the generated x86 assembly. This can be used to dynamically
    generate the literals that will be output in the x86 code but it can also be
    used for such things as dynamically setting what registers will be used as
    an operand:

    ```no_run
        assembly!{(code){
            pop %{reg};   // where reg is of type Reg
        }};
    ```

3. `@label` - is a global label named `label`.
4. `@{"label"} - is a global label named `label`.  The braces indicate an expresion
    to be evaluated.
5. `^label` - is a local label and will be prefixed with a `.` in the generated
    x86.  It will also be given a monotonically increasing integer postfix to insure
    that it is unique within the scope of a function.
6. `^{"label"}` - is a local label named `label`. See previous for further details
    on local labels.
7. `{{iterable of instructions}}` - injects any iterable which contains `Inst`. This
    allows x86 code generated in helper functions to be easily injected into an
    assembly macro.
 */


 pub mod assembly;