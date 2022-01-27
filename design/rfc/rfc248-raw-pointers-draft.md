# Design for Representing Addresses in Stet

> Note: I have used my rough understanding of how to state type rules in order
to express the semantic rules for raw pointers. There may be mistakes in my
statements due to lack of experience.

## Problem
Within the Stet language there is no representation of address based operations
or pointers. This severely limits both what can be built with Stet and
the FFIs that Stet can call. In order for Stet to grow, there needs to be
a representation of the most basic raw pointer concepts. These can then
be used to build not only heap based allocations, but smart pointers, or
garbage collectors, etc.

## Goal
In this document is the design for how raw pointers will be represented
in the Stet language.

## UX
Being danger, the use of raw pointers should be both discouraged except in special
cases (features which require raw pointers, FFI uses, and performance) and it
should be immediately obvious to a reader that a raw pointer is being used.
And the syntax of raw pointers should reflect this discouragement for writers
and make raw pointer use obvious for readers.

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

This document lays out the language features for supporting raw pointers
themselves. After this feature is implemented, then the next step is to
implement something like `unsafe` blocks in C# and Rust to further push
raw pointers to something that a user must "buy into" and to make them
obvious to a reader.

## Requirements
1. Immutability must be respected as far as it can possibly be guaranteed. It
is impossible to guarantee it is respected on the other side of an FFI but
within Stet, if a variable is aquired as immutable then it is a compiler error
for Stet to ever allow it to be mutated.
2. A raw pointer can be assigned either the address of a declared variable or
the `null` value.  It cannot be assigned an arbitrary integer.
3. `null` is a keyword that can be assigned to or compared with _all_ ref types.
4. It is possible to get the address of any variable, no matter it's type.
5. Function pointers are not covered in this document
6. The design should discourage their use to only when necessary.
7. Raw pointers can be used on uninitialized memory. (Currently, Stet does not
allow for uninitialized variables but that will come in the future.)
8. When a raw pointer is copied it's value will be copied and there will now be
at least two extant raw pointers to a location in memory.
9. You can get the address of a field in a structure.
10. with Raw pointers, you can have internal references: that is a field in
a struct is a pointer to another field in the same struct.
11. Comparisons compare addresses.  Raw pointers are equal if and only if they
point to the same location in memory or they are both `null`.
12. There is a way to compute offsets from a raw pointer.
13. There is a function for getting the size of a type. This is needed for custom
memory allocation, computing offsets, etc.
14. Comparison operators work on raw pointers. (`<, >, <=, >=, ==, !=`)

## Design
### Syntax
There are two parts to implementing raw pointers: the reference types, which
describe variables that hold addresses, and the operators used to to create
and use raw pointers.

#### Pointer Type
The type syntax is inspired from Rust:
```
Raw Pointer = *(const|mut) <Type>
```
Where `Type` is any primitive or valid custom type, including raw pointers.
`const` and `mut` must be specified to help make raw pointers as explicit
as possible and to discourage their use by making them tedious.

#### Reference Operators
There are two operators that are needed: one to get the reference and one
to dereference a pointer so that a user can access the target variable.

```
REFERENCE = @(const|mut) (IDENTIFIER[.FIELDNAME]*)
DEREFERENCE = ^EXPRESSION

MUTATE = mut LVALUE := EXPRESSION
LVALUE = [^]IDENTIFIER             // The formal labelling of the LVALUE is new to Stet
```

> Note: currently braid does not support mutations of fields in a struct. This proposal
optimistically hopes that that language change can be fit in here. The LVALUE in Stet
needs to be greatly matured so it can support derefs, fields, array elements, and 
parens to control order of operations.  The LVALUE presented here is still not complete
but fully realizing it is outside the scope of this RFC.

The Reference and Dereference operators are unary operators with precedence
higher than member access or array access operators.

The Reference operator can only be applied to identifiers or the fields of an
identifier that's a structure as those are the
only constructs in Stet that have physical locations in the computer. Literals,
for example, have a location in the machine code data section but are not
conceptually something that can be referenced. The reference operator explicitly
requires `const` and `mut` so that it mirrors the syntax of the raw pointer 
type names.

`@` is chosen as the addressing
operator rather than the traditional `&` to make `&` available for future use
with safer reference/borrowing systems. In `C/C++` the `&` is used to get
the address of a variable. In Rust `&` is used for both borrowing
and raw pointers (in Rust the `&` is cast to a raw pointer and a separate
macro must be used to explicitly get the address). For Stet, I want all interactions
with raw addresses/pointers to be as explicit and obvious as possible so the
addresses are gotten with `@`. Then `&` will be used for safe references which
will be what users are encouraged to use.

The `@` operator can only be applied to lvalues (identifier or a specific field).
This is because the raw pointer logically refers to a specific location in memory
and all locations in memory are refered to via identifiers. This mirrors the 
syntactic rules of C, and is carried into Stet both because it maps well to the
core philosophies of Stet and because it provides a very strict rule which makes
implementation easier.

The Dereference operator is `^`, which differs from other languages which use
`*` to dereference pointers. This was chosen to to make the dereferencing of
a raw pointer explicitly different from dereferencing safer references or
smart pointers. The `^` will probably be used for bitwise xor when that operator
is added to Stet but I believe it will be clear which operator is being used
by whether it is unary or binary.

Unlike the reference operator, the dereference operator can, syntactically, be
applied to any expression.  The requirement being that the expression resolve to
a reference type. This is because dereferencing is operating on a value (the
address) and the address is expected to have already come from a variable.

This makes using dereferenced identifiers legal as the left value in mutation
statement.

Both the reference and dereference operator symbols were chosen to be uniquely
applicable to raw pointers and distinct from future, safer, methods of handling
references in Stet (be those GC pointers, reference counters, or a lifetime system
a la Rust). The priority here is given to clarity for the reader of code rather
than efficiency for the writer.

#### Operator Precedence

1. `@` and `^` will have the same precedence as unary minus (`-`) and not (`!`)

### Semantics
#### Reference Operator
The reference operator can only be applied to an Identifier that is a variable
in the current scope. The type of the reference expression is `*const T` where 
`T` is the type of the variable.

The mutable reference operator `@mut` can only be applied to a variable that 
is also mutable.  The type of the mutable reference expression is `*mut T`.

`mut` and `const` are modifiers on the `*` reference type.

#### Deference Operator
```
T: Is a Type :- R: *(const|mut) T
-----------------------
:- ^R -> T
```

Mutating a dereference:
If the raw pointer is to a mutable location (`*mut T`) then the value of that
location can be mutated via the dereference operator;
```
let x: i64 := 1;
let mptr: *mut i64 := @mut x;
mut ^mptr := 5;
```

The type rules are:
```
mut ^R := V => T: Type :- R: *mut T, V: T
```

> Dereferencing `null` or a variable whose value is `null` is undefined behavior.

#### Assignment Semantics
A variable of type `*const T` can be assigned a value of type `*const T` or
a value of `*mut T`.

A variable of type `*mut T` can only be assigned a value of type `*mut T`.

```
let x: i64 := 5;
let mut y: i64 := 10;

let cptr_x: *const i64 := @const x;  // Legal
let cptr_y: *const i64 := @const y;  // Legal

let mptr_x: *mut i64 := @mut x; // Illegal, cannot get mutable address to an immutable variable
let mptr_y: *mut i64 := @mut y; // Legal

let mptr_x: *mut i64 := @const x; // Illegal, cannot get mutable address to an immutable variable
let mptr_y: *mut i64 := @const y; // Illegal, cannot cast a const location to a mutable pointer
```

##### Ref Type Variable Mutability
A variable of reference type (immutable and immutable) can itself be annotated
as mutable: this means that the address stored in the variable can be mutated 
it has _no_ bearing on if the location the address refers to can be mutated.

```
let x: i64 := 5;
let x2: i64 := 5;
let mut y: i64 := 10;
let mut y2: i64 := 10;

let mut cptr_x: *const := @const x;
let mut cptr_y: *mut := @mut y;

mut cptr_x := @const x2;     // Legal
mut cptr_y := @mut y2; // Legal 
```

#### Copying References
Copying: a mutable reference can be copied to a variable of type `*const` or
`*mut`, so long as the underlying type is the same. An immutable reference
can _only_ be copied toa  variable of type `*const`.

Examples:
```
fn write_to(m: *mut i64) {...};
fn read_from(i: *const i64) {...};

let x: i64 := 5;
let mut y: i64 := 10;

let cptr_x: *const i64 := @const x;       // Legal
let cptr_y: *const i64 := @const y;       // Legal
let cptr_x2: *const i64 := cptr_x;  // Legal
let cptr_y2: *const i64 := cptr_y;  // Legal

let mptr_y: *mut i64 := @mut y; // Legal
let mptr_x: *mut i64 := @mut x; // Illegal, cannot get mutable address to an immutable variable
let mptr_x: *mut i64 := cptr_x; // Illegal, cannot get mutable address to an immutable variable

write_to(@mut y); // Legal
write_to(@const y);     // Illegal, type mismatch requires *mut i64 but `@x` has type *const i64
write_to(@mut x); // Illegal, @mut cannot be applied to an immutable variable

read_from(cptr_x); // Legal
read_from(cptr_y); // Legal
read_from(mptr_y); // Legal
read_from(@const x);     // Legal
read_from(@const y);     // Legal
read_from(@mut y); // Legal
```

### Comparisons
Comparisons are done on the addresses stored in the reference variable.
Mutable and immutable references can be compared with each other.

#### Equality
- `==` - returns true if the LHS and RHS have the same address value, otherwise false.
- `!=` - returns true if the LHS and RHS have different address value, otherwise false.

#### Less Than/Greater Than
Comparison operators will operate on the numerical values of the addresses as if they
were integer types.

### Offsets
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
T: Type :- R: *(const|mut) T
----------------------
p: R, o: i64 :- p<o> -> R    // The offset of a *const is *const and the offset of a *mut is *mut
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

### size_of
The `size_of` function takes a type and returns the number of bytes allocated to store
a type in memory. This takes into account padding bytes that might be added for
memory alignment purposes.

Syntax:
```
SIZE_OF = size_of(TYPE)
```

Type:
```
:- size_of(T) -> u64
```

For simplicities sake, this is operating on the C notion of size where every type as a
size. For primitive types with the bitsize in the name, the size of the type shall remain
constant across all versions of Stet and all platforms and architectures.

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
5. Record if the pointer is pointing to a type which is memory aligned
6. When a pointer is bound to a Field in a struct, the node should display the
field name. Have a reference to the span for the field's definition.

When the `*` deref instruction is applied to an expression:
1. The lexer will emit an event for the `*` token.
2. The parser will emit an event for generating the `*` unary node in the AST.
3. The type resolver will emit an event which includes a reference to
the definition of where the operand comes from.

This is a bit outside the scope of this RFC, but because this is dealing with
language features that surface the physical nature of how variables are stored:
> It would be nice to record the memory layout information for all structs and
display that in the LLVM tab for the Viewer.
> It would also be nice to have a compiler flag to print out memory layout table
for all types after compilation.

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
13. Test derefence of each of the Reference tests
14. Test mutable dereference of each of the reference tests
15. Reference to a mutable value, mutate the value, test that the reference 
reflects the mutation.
16. test initializiing a reference to null
17. Test comparing a reference to a null
18. Test comparing two references compares addresses
19. Get pointer to first element in the array, then use pointer arithmetic
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

### ^ Operator
The `^` token already exists in the Lexer and token set.

A new unary rule will be added to the Parser:
```
Deref = ^EXPRESSION
DeRefAssignment = mut *EXPRESSION := EXPRESSION
```

A new variant will be added to the `Expression` enum. This variant will have a single
child, which is the expression it is dereferencing. A new variant for mutations will
be added for mutating a reference.

Semantic Rules will be added of the form
1. For DeRefAssignment, the `<Expression>` on the LHS must be mutable reference.
2. For `*<Expression>` in the LHS, the type of `<Expression>` must be a Reference
type (mutable or immutable). If the type of `<Expression> :- *(const|mut) T` then
the type of `*<Expression> :- T`

### size_of
The `size_of` function will be implemented using the LLVM API and computed as a static
value during compile-time.

https://stackoverflow.com/questions/14608250/how-can-i-find-the-size-of-a-type
https://llvm.org/doxygen/classllvm_1_1DataLayout.html#aa48b3b7e554b44f4e513d5dd8d9f9343


### Syntactic Fuzzer Changes
1. Add type annotations for `*const T` and `*mut T`.  This should be able to use the
same logic as arrays.
1. Add expression generation for taking the reference of an identifier.
1. Get a raw pointer to a field in a structure.
1. Add a failure test by generating a reference operator where the operand is not an identifier.
1. Add deref generation for expressions.
1. Add deref mutation generation for statement generators.
1. Compute raw pointer offsets
1. Mutate dereference to a field name