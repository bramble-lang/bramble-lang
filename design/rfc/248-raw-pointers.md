# Design for Representing Addresses in Stet
## Problem
For any work beyond the most basic operations of a language, there needs to
be an ability to operate not on a variable but on the address to a variable.
This allows for efficient sharing of large or unknown blocks of data
between interfaces and it is essential for operating on data that is allocated
on the heap.  All heap operations are done through addresses.  In addition,
many interactions with the host OS require addresses to a location in memory
where large data structures can be shared. Finally, more advanced and safer
ways to interact with sharing memory locations require a representation of
the architecture concept of address and access to that address as a building
block: e.g. to build smart pointers we still have to have the concept of
a pointer to pack with the ref counter.

Currently, Stet does not have a concept of raw pointer or address which limits
the ability to use the heap, use the full set of C functions in libc, or the
create smart pointers or a garbage collector.

## Goal
The goal of this design proposal is to lay out the syntax and semantics of
raw pointers and address in the Stet language. Then define how these will
be representing in the compiler. The document will start by laying out the
requirements for raw pointers, then how they will be representing in the language,
then how they will be implemented in the compiler. There will be
a section describing a minimum set of tests to prove that the implementation
meets requirements. Finally, there will be the section describing how raw
pointers will be handled in the Insight platform and displayed in the Stet
Insight Viewer.

## Requirements
1. Immutability must be respected as far as it can possibly be guaranteed. It
is impossible to guarantee it is respected on the other side of an FFI but
within Stet, if a variable is aquired as immutable then it is a compiler error
for Stet to ever allow it to be mutated.
2. A pointer can only ever be given an address from a declared variable. For example,
you cannot set the address to a literal (e.g. `let ptr = 0xDEADBEEF` is illegal). I think
this may be unavoidable, but should be covered instead by casting which is a
feature I need to add in the future.
3. Addresses can only be taken from a variable.
4. Immutability is assumed.
5. A mutable variable can be referenced as immutable or mutable.
6. A mutable variable can only be referenced as immutable.
7. A variable of type raw pointer can be set to `null` that is the only literal
assignment allowed.
8. `null` is a keyword that can be assigned to or compared with _all_ ref types.
9. It is possible to get the address of any variable, no matter it's type.
10. Function pointers are not covered in this document
11. The design should discourage their use to only when necessary.

## Design
### Syntax
There are two parts to implementing raw pointers: the reference types, which
describe variables that hold addresses, and the operators used to to create
and use raw pointers.

#### Types
The type syntax is borrowed from Rust:
```
Raw Pointer = *(const|mut) <Type>
```
Where `Type` is any primitive or valid custom type, including raw pointers.
`const` and `mut` must be specified to help make raw pointers as explicit
as possible and to discourage their use by making them tedious.

#### Operators
There are two operators that are needed: one to get the reference and one
to dereference a pointer so that a user can access the target variable.

```
Reference = @[mut] <Identifier>
Dereference = *<Expression>
```

The Reference and Dereference operators are unary operators with precedence
higher than member access or array access operators.

The Reference operator can only be applied to identifiers, as those are the
only constructs in Stet that have physical locations in the computer. Literals,
for example, have a location in the machine code data section but are not
conceptually something that can be referenced.

The Dereference operator can be applied to the result of an expression. For
example, if a function returns a reference that can be immediately dereferenced
and used. Likewise, an if expression could resolve to a reference and that
can then be dereferenced.

### Semantics
#### Reference Operator
The reference operator can only be applied to an Identifier that is a variable
in the current scope. The type of the reference expression is `*const T` where 
`T` is the type of the variable.

The mutable reference operator `@mut` can only be applied to a variable that 
is also mutable.  The type of the mutable reference expression is `*mut T`.

`mut` and `const` are modifiers on the `*` reference type.

Implicit casting: a mutable reference can be cast to an immutable reference.
An immutable reference _cannot_ be case to a mutable reference.