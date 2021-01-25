# Design for how Core Libraries will be implemented
## Problem
I do not want to write Core Library functions directly in assembly. For example,
the Coroutine core functions are written in straight assembly and as I add
other functions (e.g. IO functions), I want to do those in Braid if possible.

## Solution
Read about how C implements functions like `printf` and so on to understand how
the system interfacing functions are implemented in C without just writing inline
assembly. Then add features to the Braid language that would let me do this.

At the very least, I want to move the x86 defined code into separate files rather
than being defined within the compiler: at least for large functions.

## Design

## Research
### C
Currently, reading through `glibc` to see how `printf` and other functions are implemented
and will note down what is being used to let C do the system calls and other things that
are needed for these functions to work.

From what I can tell, for the `sysdeps` it actually does break down into files of assembly
code.

#### glibc
##### Items of Interest when investigating `printf`
1. `__set_errno`
2. `LABEL` and `JUMP`: LABEL is a macro for building consistently named labels in the C
code and JUMP handles looking up the label to jump to.
3. `__find_specmb` and `__find_specwc`
4. `outstring`
5. `__getpagesize`


##### Investigating `printf`
Looking through the glibc source code in stdio/printf, there's a call to `__vfprintf_internal`.
There are also calls to `va_start` and `va_end`

In `vfprintf-internal.c` here's the definition of `vfprintf`:
```
      /* Get current character in format string.  */
      JUMP (*++f, step0_jumps);

      /* ' ' flag.  */
    LABEL (flag_space):
      space = 1;
      JUMP (*++f, step0_jumps);

      /* '+' flag.  */
    LABEL (flag_plus):
      showsign = 1;
      JUMP (*++f, step0_jumps);

      /* The '-' flag.  */
    LABEL (flag_minus):
      left = 1;
      pad = L_(' ');
      JUMP (*++f, step0_jumps);
```

What's `LABEL` and `JUMP` do?  Also within this function is a call to `outstring`, what does this do?

##### Investigating `putw`
###### Item of Interest
1. `_IO_fwrite` in `putw.c`


##### Investigating `__set_errno`
I found a definition of this function as a define in `` that references `glibc/stdlib/err.h`, in that
file there is a section that is blocked out by `#ifndef __ASSEMBLER__`: `__ASSEMBLER__` is defined as
1 when preprocessing assembly language code.

This is how `__set_errno` is defined `# define __set_errno(val) (errno = (val))`.  `errno` is defined
in `stdlib/err.h` as `# define errno (*__errno_location ())`, but only if `__ASSEMBLER__` is `0`.