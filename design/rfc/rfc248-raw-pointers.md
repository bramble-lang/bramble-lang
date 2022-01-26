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

## UX
Because raw pointers are one of the most dangerous elements to work with in
any programming language, their use should be both discouraged except in special
cases (features which require raw pointers, FFI uses, and performance) and it
should be immediately obvious to a reader that a raw pointer is being used.

So, any use of raw pointers should be obviously different from safer uses of
reference values (e.g. Rust borrows, smart pointers, garbage collected pointers, etc).
This leads to several design goals:
1. Raw pointer types should be as explicit as possible when written down.
1. Creating a raw pointer directly to a variable should be obviously different.
(e.g. Rust uses the [addr_of](https://doc.rust-lang.org/core/ptr/macro.addr_of.html)
macro to just get the address and bypass any safety checks).
1. Dereferences should also be obvious and explicit. Where dereferences of
smarter reference types might be implicitly injected by the compiler, for raw
pointers they must all be explicitly written.

Ultimately, the goal is to wrap all uses of raw pointers within an `unsafe`
expression block, which will allow the compiler to check for unintentional use
of raw pointers.

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
12. Raw pointers can be used on uninitialized memory.
13. When copied the address will be copied to the new location. This applies for
pointer fields in structures.
14. You can get the address of a field in a structure.
15. with Raw pointers, you can have internal references: that is a field in
a struct is a pointer to another field in the same struct.
16. Comparisons compare addresses
17. A function that returns the size of any type.
18. Be able to perform safe arithmetic on the pointer: for low level operations
(e.g. implementation of vectors or dynamic buffers) will need to be able to 
offset from the base pointer.

## Design
### Syntax
There are two parts to implementing raw pointers: the reference types, which
describe variables that hold addresses, and the operators used to to create
and use raw pointers.

#### Types
The type syntax is inspired from Rust:
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

`@` is chosen as the addressing
operator rather than the traditional `&` to make `&` available for future use
with safer reference/borrowing systems. In Rust `&` is used for both borrowing
and raw pointers (in Rust the `&` is cast to a raw pointer and a separate
macro must be used to explicitly get the address). For Stet, I want all interactions
with raw addresses/pointers to be as explicit and obvious as possible so the
addresses are gotten with `@`. Then `&` will be used for safe references which
will be what users are encouraged to use.

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

#### Assignment Semantics
A variable of type `*const T` can be assigned a value of type `*const T` or
a value of `*mut T`.

A variable of type `*mut T` can only be assigned a value of type `*mut T`.

```
let x: i64 := 5;
let mut y: i64 := 10;

let cptr_x: *const i64 := @x;  // Legal
let cptr_y: *const i64 := @y;  // Legal

let mptr_x: *mut i64 := @mut x; // Illegal, cannot get mutable address to an immutable variable
let mptr_y: *mut i64 := @mut y; // Legal
```

##### Ref Type Variable Mutability
A variable of reference type (immutable and immutable) can itself be annotated
as mutable: this means that the address stored in the variable can be mutated 
it has _no_ baring on if the location the address refers to can be mutated.

```
let x: i64 := 5;
let x2: i64 := 5;
let mut y: i64 := 10;
let mut y2: i64 := 10;

let mut cptr_x: *const := @x;
let mut cptr_y: *mut := @mut y;

mut cptr_x := @x2;     // Legal
mut cptr_y := @mut y2; // Legal 
```

#### Use in Expressions
Implicit casting: a mutable reference can be cast to an immutable reference.
An immutable reference _cannot_ be cast to a mutable reference.

Examples:
```
fn write_to(m: *mut i64) {...};
fn read_from(i: *const i64) {...};

let x: i64 := 5;
let mut y: i64 := 10;

let cptr_x: *const i64 := @x;  // Legal
let cptr_y: *const i64 := @y;  // Legal

let mptr_x: *mut i64 := @mut x; // Illegal, cannot get mutable address to an immutable variable
let mptr_y: *mut i64 := @mut y; // Legal

write_to(@mut y); // Legal
write_to(@y);     // Illegal, type mismatch requires *mut i64 but `@x` has type *const i64
write_to(@mut x); // Illegal, @mut cannot be applied to an immutable variable

read_from(cptr_x); // Legal
read_from(cptr_y); // Legal
read_from(mptr_y); // Legal
read_from(@x);     // Legal
read_from(@y);     // Legal
read_from(@mut y); // Legal
```

### Comparisons
Comparisons are done on the addresses stored in the reference variable.
Mutable and immutable references can be compared with each other.

#### Equality
- `==` - returns true if the LHS and RHS have the same address value, otherwise false.
- `!=` - returns true if the LHS and RHS have different address value, otherwise false.

#### Less/Greater

### Arithmetic
A key part of raw pointers is being able to use them to move through large
blocks of allocated memory: e.g. dynamic arrays and so on. On a semantic level
this mirrors the array access operation `[n]` with the key difference being that and offset
could be negative.

A similar, but visibly different operator is provided for getting offsets
from a given raw pointer (which addresses the need for pointer arithmetic).

```
ptr<i64> // The <> operator takes a signed 64 bit integer and returns a new address
```

The offset operator will get an offset address in terms of multiples of the size
of the underlying type. The address it will return computes as `ptr value + size_of(T) * offset`
where `T` is the underlying type of the ptr.

So for a pointer to a `i64`, the size of the underying type is `size_of(i64) = 8`
and the offset will move in steps of 8: `ptr<2> = ptr + 2 * 8` or
`ptr<-3> = ptr - 3 * 8`.

The `<>` characters were chosen to make the pointer offset operation visually
distinct from the array access operation while still sharing some resemblance
because logically they operate in identical manners with the array access being
more restrictive in its inputs (they must be unsigned).

Syntax:
```
PtrOffset = IDENTIFIER<EXPRESSION>
```

Semantics:
```
P = IDENTIFIER :- *(M = const|mut) T
O = EXPRESSION :- i64
----------------------
P<O> :- *M T    // The offset of a *const is *const and the offset of a *mut is *mut
```

Alternative syntax: Use this as a binary operator: `~`  e.g. `ptr ~ -5`.  I don't
like this because it's very subtle and with some fonts could be confused with `-`.

One reason to avoid using `<...>` is it's a pain to implement in the parser. 
Perhaps `$` or `#` as binary operators.  The semantic rules for typing stay the same
but it becomes easier to parse.

### Member Access on Values of Reference Type
If a reference points to a structure then to access members of the structure you
need to dereference the pointer first: `*ptr.field` or `(*ptr).field`. To dereference
a field in a structure that is a reference: `*(ptr.field_ptr).field` or 
`(*(ptr.field_ptr)).field`.

## Insight
The insight should focus on the physical nature of pointers and addresses, as they
represent the actual locations in memory and deal with interacting with those physical
realizations of the logical variables. So, when possible the visualization and Insight
should emphasize that link to the physical resource.

Recorded events:
When the `@` operator is applied to a variable then:
1. The lexer will emit an event for the `@` token
2. The parser will emit an event for generating a `@` unary node in the AST.
3. The type resolver will emit an event which includes a reference span to the definition
for whatever is the operand of `@`, because the declaration of that variable
is also the aquisition of the physical memory.
- If the operand is a field of a structure, then it will reference the declaration of
that structure variable.
- The reference type should probably be annotated to make it explicit that this
is refering to the same physical location and not that it is using a reference for
a compile time decision.
4. LLVM will emit an event with the `LLVM` get address instruction.

When the `*` deref instruction is applied to an expression:
1. The lexer will emit an event for the `*` token.
2. The parser will emit an event for generating the `*` unary node in the AST.
3. The type resolver will emit an event which includes a reference to
the definition of where the operand comes from.

### Viewer
Highlight the creation of raw pointers and the dereference of raw pointers in some
manner to emphasize that something dangers is being done.

In this case, the Cell of an `@` expression or a `*` expression will be will be colored
a light orange as a warning sign.

## Tests
### Integration Tests
1. Get Reference to each primitive type
2. Get Reference to array
3. Get Reference to structure
4. Get reference to field in structure
5. Get reference to element in array
6. Get reference to reference
7. Repeat all tests for mutable references
8. Test passing a reference to a function
9. Test passing a mutable reference to a function and function mutates value
10. Return reference from function
11. Test using malloc and free
12. Copy struture with reference field => both copies point to the same value
1. Test derefence of each of the Reference tests
2. Test mutable dereference of each of the reference tests
3. Reference to a mutable value, mutate the value, test that the reference 
reflects the mutation.
4. test initializiing a reference to null
5. Test comparing a reference to a null
6. Test comparing two references compares addresses
1. Get pointer to first element in the array, then use pointer arithmetic
to move to each element of the array.

## Implementation
### Types
The `const` keyword will be added to the token set and the lexer.

A new `Ref` variant will be added to the `Type` enumeration. The
`Ref` variant will have two fields. One field `is_mutable` boolean which is
false for immutable references and true for mutable references. And a boxed value
to the target type that the reference refers to.

The Parser will be updated to parse the ref type annotations according to this
grammatical rule:
```
RefType = *(const|mut) <Type>
```

`*const <Type>` will set `is_mutable` to false and the target type to `<Type>`.
`*mut <Type>` will set `is_mutable` to true and the target type to `<Type>`.

### @ Operator
`@` will be added to the Lexer and token set as `Ampersand`.

Two new variants will be added to the `Expression` enumeration: `AddressOf` and
`AddressOfMut`.

The parser will follow these rules:
```
AddressOf = @<Identifier>
AddressOfMut = @mut <Identifier>
```

Semantic rules will be added of the form:
1. For `AddressOf` the operand must be a variable.  It can be immutable or mutable.
2. For `AddressOfMut` the operand must be a mutable variable.

### * Operator
The `*` token already exists in the Lexer and token set.

A new unary rule will be added to the Parser:
```
Deref = *<Expression>
DeRefAssignment = mut *<Expression> := <Expression>
```

A new variant will be added to the `Expression` enum. This variant will have a single
child, which is the expression it is dereferencing. A new variant for mutations will
be added for mutating a reference.

Semantic Rules will be added of the form
1. For DeRefAssignment, the `<Expression>` on the LHS must be mutable reference.
2. For `*<Expression>` in the LHS, the type of `<Expression>` must be a Reference
type (mutable or immutable). If the type of `<Expression> :- *(const|mut) T` then
the type of `*<Expression> :- T`

### Syntactic Fuzzer Changes
1. Add type annotations for `*const T` and `*mut T`.  This should be able to use the
same logic as arrays.
1. Add expression generation for taking the reference of an identifier.
1. Add a failure test by generating a reference operator where the operand is not an identifier.
1. Add deref generation for expressions.
1. Add deref mutation generation for statement generators.