# Addressable Expressions
## Overview
Allow the Braid compiler to support more than just identifiers as the
LHS of mutation operations and the operand of `@(const|mut)` operations.
Specifically, be able to use array elements and structure fields so
that the following would be legal:
```
mut arr[0] := 5;
mut st.field := -1;
@const arr[0]
@const st.field
```

## Solution
(This is inspired by the language used in the Go Lang spec).

The expressions which can be mutated or referenced with `@` are expressions
which have places somewhere in memory (the stack or the heap). These
are called Addressable values. Without a place in memory, obviously, the
expression cannot have its value changed or its address taken. Expressions which
evaluate to a value are Value Expressions.  All addressable expressions are also
value expressions: in this case their value is read from their place in memory.

Whether an Addressable expression resolves to its address in memory or its value
is context dependent: specifically, the operation and what operand position the
expression takes. For example, the LHS of a mutation operation would resolve to
an address, but the RHS of the same mutation operation would evaluate to an
address.

I propose adding a new `is_addressable` flag to the `SemanticContext` of an
expression. This will be added before type resolution. During type resolution,
the compiler will not only check that the types are correct, but will also
check if an operand is addressable for the operations which require addressable
values.

In order to support mutation operations, an `is_mutable` flag will also have
be added to the `SemanticContext`.  Currently, this flag only exists in the
symbol table for variables, but this cannot support dereference expressions
or field access or array indexing expressions.  The `is_mutable` flag will
remain in the symbol table and a flag will be added to `SemanticContext` for
mutability of addressable expressions.

The mutability and addressability of an expression will be combined into a single
flag with 4 possible states: `None|Value|Addressable|AddressableMutable`. An expression
can only be mutable if it is addressable, so having two separate flags creates
risk of accidentally marking a value expression as mutable. `None` is used
both as a place holder for when an expression has not had it's Addressability
computed and for nodes which are not expressions (structure defintions, etc).

A new phase `ComputeAddressable` will traverse the AST and set the addressability
flag for every node in the AST, based upon a set of rules detailed in a following
section.

Then in LLVM Generation, a new method will be added to `Expression` called
`to_addr -> Option<T>`, which will return the address of an Addressable
Expression, rather than its stored value, if the expression is addressable
otherwise it will return None. This implementation is easily done by simply
returning the pointer and skipping the build load operation.

Why this design? This design allows for determining whether expressions can be used
for mutations and referencing in a way that is fully independent of the grammar rules
or AST design. Which means both: no major refactoring or changes to the AST _and_
no added bounds on how the grammar and AST could evolve into the future.

Why have this separate from type resolution? Type resolution is already large
and complex, the danger is that addressability rules checking would get lost
in all the type checking logic.  Making it easy to break and hard to fix in the
future. Having it as a separate step will make it more obvious that it occurs
and easier to maintain and grow. It also is logically not the same thing as
type checking.

### Addressable Rules
All rules here assume that the expressions are of valid types for the operators
(e.g. that in `^EXP`, `EXP` is a raw pointer type). The type checking can happen
after addressability is determined, because IDENTIFIERs are always addressable
and the result of dereferencing operation must be addressable.  Addressability
can also be done _after_ type checking, in which case it would be combined with
checking that the operands for place operators are addressable.

All expressions are `Value` expressions unless they satisify the Addressable
rules below. All other grammar rules are `None`.

```
X: IDENTIFIER
--------------
X :- Addressable


X: EXPRESSION :- Addressable
--------------
(X) :- Addressable


X: EXPRESSION :- Addressable, I: EXPRESSION
----------------
X[I] :- Addressable


X: EXPRESSION :- Addressable, F: IDENTIFIER
-----------------
X.F :- Addressable


X: EXPRESSION :- Addressable
----------------
@(const|mut) X :- Addressable


T: Type, X: *(const|mut) T
-----------------
^X :- Addressable


T: Type, X: *mut T
-----------------
^X :- Mutable


T: Type, X: *(const|mut) T
-----------------
X :- Addressable


X: EXPRESSION :- Addressable and Mutable, Y: Expression
----------------
mut X := Y :- Not Addressable
```

Array elements and structure fields are not de facto addressable. For example,
`foo().field`, the function `foo` returns a structure and we access the field
`field`. Within Braid, we logically describe this as a _value_ and not an addressable
expression so it cannot be used for `@` or mutations. (Even though, in the LLVM
IR we generate we actually allocate space in the caller stack frame for the returned
structure value, we still logically describe it as a value expression, this is 
because there is no label associated with it, so it would be hard for a user to
mentally or visually track how the value is being used or mutated).

### Implementation
#### Status
Add an enumeration that has the variants: None|Value|Addressable|AddressableMutable
Add a field for this enumeration to `SemanticContext` with default value `None`.

Can I update `canonize/foreach_mut` to support POST order execution (as a configurable
flag)? If so, then I think I can use that foreach for the traversal? Part of
this would be updating foreach to do the Insight event recording too.
> It might be easier to start out by adding this into type_resolver then move to its
own stage?

Update the grammar rules so that any expression can be used on teh LHS for mutation
and that any expression can be used as the operand for `@(const|mut)`.

## UX
The only impact on UX will be greater flexibility in what can be mutated and
referenced. So, this will move Braid towards being more intuitive in that regard.

## Insights
We should mark what expressions resolve to being addressable and what expressions
resolve to being only value expressions. This can be done by merging the events
generated by the addressability analysis with semantic node events via spans
tests.

A new compiler stage will be added to the trace output stage set: `Addressability`.
This stage will record the assignation of every span in the input source code with
whether it is `Addressable` or not. For each node in the AST that is checked, this
will emit an event with the result of the `Addressability` analysis.

## Tests
1. Test mutating an array element
1. Test mutating a structure field
1. Test referencing an array element
1. Test referencing a field
1. Test mutating a dereference (`mut ^a := 5`)
1. Test mutating an integer => fails
1. Test mutating `(x+5)` => fails
1. Test `(x)`
1. Test `(x)[5].field`, an array of structures
1. Test `(x.field)[5]`, field is an array
1. Test `^x.field`, field is a pointer
1. test `^x.field[2]`, field is an array of pointers
1. Test `^x[0]` array of pointers
1. Mix in combinations of `()`