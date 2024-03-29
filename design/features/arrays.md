# Feature: Arrays Allocation and Values
## Problem
A fixed set of randomly accessible data is one of the most basic types used in computer science
and is the foundation for almost all complex data structures.  It is also necessary for modelling
memory, files, and communication. Bramble currently lacks any type that covers this.

## Solution
Add an array type which will allow the user to create a densely packed block of memory which can
store a fixed number of values of a given type. This value can then be accessed via a numerical
index.

## Design
### Requirements
Allow the following actions:
1.   Index into the array to access a value
1.   Pass the array to a function
1.   Return the array from a function
1.   Be able to mutate an element in an array, if the array is mutable
1.   Be able to make an array immutable
1.   Have an array expression, that can be used to initialize an array.
1.   If an array is immutable but has not be assigned a value, then allow an element in the array to be assigned a value once. (maybe, but I think this could be skipped in favor of requiring array be initialized to an array expression.)
1.   Do not let an element in an array be used unless it has been assigned a value.
1.   Do not let an array be passed to a function unless every element has been assigned a value.
1.   Do not let an array be returned from a function unless every element as been assigned a value.

### Design
In my first design, arrays will be statically allocated on the stack with a fixed size set at
compile time.  Much like arrays in Rust. So the type of an array will consist of both the type of the
elements and the number of elements.

The array type will be written as `[<type>; <integer>]` with the outer `[]` representing the array and
the first field declaring the type of the element and the second field the number of elements
in the array.

```
type identifier: [<type>; <unsigned integer>];
value: [1, 2, 3, 4];
declaration: let a: [i32; 5] := [1, 2, 3, 4, 5];
function: fn foo(a: [i32; 5]) -> [i32; 10] {...}
access: a[2]
```

### Backend
This will use the LLVM array type on the backend of the compiler.

For passing arrays to a function, I will use a pointer to an array type, similar to Rust or C.

For returning an array from a function, I will transform the function to use an output parameter (similar
to what is done for structs).

## Research
### Rust
#### Passing an array to a function
The rust code:
```rust
pub fn pass(num: [i32; 5]) -> i32 {
    num[0] * num[1]
}
```

The LLVM output:
```llvm
define i32 @_ZN7example4pass17h6a5a7426692c20b1E([5 x i32]* noalias nocapture dereferenceable(20) %num) unnamed_addr #0 !dbg !6 {
start:
  %0 = getelementptr inbounds [5 x i32], [5 x i32]* %num, i64 0, i64 0, !dbg !10
  %_2 = load i32, i32* %0, align 4, !dbg !10
  %1 = getelementptr inbounds [5 x i32], [5 x i32]* %num, i64 0, i64 1, !dbg !11
  %_4 = load i32, i32* %1, align 4, !dbg !11
  %2 = call { i32, i1 } @llvm.smul.with.overflow.i32(i32 %_2, i32 %_4), !dbg !10
  %_6.0 = extractvalue { i32, i1 } %2, 0, !dbg !10
  %_6.1 = extractvalue { i32, i1 } %2, 1, !dbg !10
  %3 = call i1 @llvm.expect.i1(i1 %_6.1, i1 false), !dbg !10
  br i1 %3, label %panic, label %bb1, !dbg !10

bb1:                                              ; preds = %start
  ret i32 %_6.0, !dbg !12

panic:                                            ; preds = %start
  call void @_ZN4core9panicking5panic17h07405d6be4bce887E([0 x i8]* noalias nonnull readonly align 1 bitcast ([33 x i8]* @str.0 to [0 x i8]*), i64 33, %"std::panic::Location"* noalias readonly align 8 dereferenceable(24) bitcast (<{ i8*, [16 x i8] }>* @alloc5 to %"std::panic::Location"*)), !dbg !10
  unreachable, !dbg !10
}
```

#### Returning an array from a function
```rust
pub fn ret() -> [i32;5] {
    let a = [1, 2, 3, 4, 5];
    a
}
```

The LLVM:
```llvm
define void @_ZN7example3ret17h0366b3256c4cbda9E([5 x i32]* noalias nocapture sret dereferenceable(20) %a) unnamed_addr #0 !dbg !13 {
start:
  %0 = bitcast [5 x i32]* %a to i32*, !dbg !14
  store i32 1, i32* %0, align 4, !dbg !14
  %1 = getelementptr inbounds [5 x i32], [5 x i32]* %a, i32 0, i32 1, !dbg !14
  store i32 2, i32* %1, align 4, !dbg !14
  %2 = getelementptr inbounds [5 x i32], [5 x i32]* %a, i32 0, i32 2, !dbg !14
  store i32 3, i32* %2, align 4, !dbg !14
  %3 = getelementptr inbounds [5 x i32], [5 x i32]* %a, i32 0, i32 3, !dbg !14
  store i32 4, i32* %3, align 4, !dbg !14
  %4 = getelementptr inbounds [5 x i32], [5 x i32]* %a, i32 0, i32 4, !dbg !14
  store i32 5, i32* %4, align 4, !dbg !14
  ret void, !dbg !15
}
```
Interestingly, the Rust compiler moves the variable `a` up into the parameter list and uses that as
the name of the output parameter.

#### Using an array
Here's an example of a function which creates a two element array and then returns the first element:
```rust
pub fn ret() -> i32 {
    let a = [1, 2];

    a[0]
}
```

The LLVM:
```llvm
define i32 @_ZN7example3ret17h010117d9a2fb03b8E() unnamed_addr #0 !dbg !13 {
start:
  %a = alloca [2 x i32], align 4
  %0 = bitcast [2 x i32]* %a to i32*, !dbg !14
  store i32 1, i32* %0, align 4, !dbg !14
  %1 = getelementptr inbounds [2 x i32], [2 x i32]* %a, i32 0, i32 1, !dbg !14
  store i32 2, i32* %1, align 4, !dbg !14
  %2 = getelementptr inbounds [2 x i32], [2 x i32]* %a, i64 0, i64 0, !dbg !15
  %3 = load i32, i32* %2, align 4, !dbg !15
  ret i32 %3, !dbg !16
}
```
In this example, Rust uses the GEP to get pointers to individual fields and set the values.

##### Using an array with more complex elements
```rust
pub fn ret() -> i32 {
    let a = [inc(1), inc(2)];

    a[0]
}

pub fn inc(x: i32) -> i32 {
    x + 1
}
```
And it's LLVM (leaving out the `inc` function):
```llvm
define i32 @_ZN7example3ret17h010117d9a2fb03b8E() unnamed_addr #0 !dbg !13 {
start:
  %a = alloca [2 x i32], align 4
  %_2 = call i32 @_ZN7example3inc17h810941d63eecc67fE(i32 1), !dbg !14
  br label %bb1, !dbg !14

bb1:                                              ; preds = %start
  %_3 = call i32 @_ZN7example3inc17h810941d63eecc67fE(i32 2), !dbg !15
  br label %bb2, !dbg !15

bb2:                                              ; preds = %bb1
  %0 = bitcast [2 x i32]* %a to i32*, !dbg !16
  store i32 %_2, i32* %0, align 4, !dbg !16
  %1 = getelementptr inbounds [2 x i32], [2 x i32]* %a, i32 0, i32 1, !dbg !16
  store i32 %_3, i32* %1, align 4, !dbg !16
  %2 = getelementptr inbounds [2 x i32], [2 x i32]* %a, i64 0, i64 0, !dbg !17
  %3 = load i32, i32* %2, align 4, !dbg !17
  ret i32 %3, !dbg !18
}
```

### Breakdown of how Rust works
Rust uses `alloca` to allocate space on the stack for the array. Then to use the array, it casts the array pointer (returned by `alloca`) to a pointer to the element type (in the preceding example it goes from `[2 x i32]*` to `i32*`).  This creates a pointer to the first element in the array. To access the first element of the array, Rust just uses that pointer directly.  To Access other elements of the array, Rust uses the GEP on the pointer to the first element.

To read elements from the array, Rust uses a similar strategy, but using the `load` instruction to read data from where the GEP is pointing. One difference, is that in my example, Rust does not use the first element pointer to read the first element of the array, it uses the GEP to get a pointer to the first element and then calls `load` on that pointer.  I'm guessing this is because the first element pointer is created in the compilation of the `bind` expression and is therefore inaccessible to the `read element at` expression. (Will LLVM optimization catch this?)

## How Bramble will compile:
Bramble will start out by just copying the way that Rust compiles these expressions:

### Array Value
1. Allocate space on the stack with `alloca`
1. <s>`bitcast` the array pointer to a pointer to the first element in the array</s>.  This understanding is wrong.  Rust bitcasts the array ptr to assign the first element but ONLY the first element. For all other elements it uses GEP on the array pointer to get the element
1. For each element in the array, compile the element to LLVM
1. For each LLVM Value, use `getelementptr` on the _array_ pointer to point to the array at a given index
1. `store` the value to that location
1. Resolve the LLVM value of the array value to be the pointer returned by `alloca`.  This can then be copied into the binding variable.

### Binding
1. Allocate space on the stack with `alloca`
1. Evaluate the RHS of the bind expression
1. Do I memcpy the result of the RHS into the newly allocated space on the stack or do I use a pointer?
1. I copy the pointer to the space to the variable name.
1. In `let a = [1, 2, 3]`, the variable `a` is a pointer to the first element in the array.

### The get an element operation `let x := a[0]`
1. Given an array pointer (the LLVM result that comes from computing an array value)
1. and an index
1. Compute the GEP for the array pointer at that index
1. Load that value into a register
1. That is the result of a get operation
1. The semantic rule for the index operator is that the LHS must be an array type and the resolved type is the element type of the LHS.
1. Another semantic rule: the index type must resolve to be an i64 type

### The set an element operation `mut a[0] := 5`
1. Given a mutation statement with a LHS that is an indexed array (`a[0]`)
1. Compute the RHS expression of the mutation statement
1. Compute the GEP of the array element at the given index
1. Store the result of the RHS expression into the GEP

### Mutate the array itself `mut a := [1, 2]`
1. What about `mut a = [1, 2]`?
1. Evalutation each element expression in the LHS
1. Use the RHS as the array pointer
1. Compute the GEP for each element in the array
1. Store the result of the corresponding LHS element

### Passing to a function
1. In parameters, evaluate the array type to an LLVM pointer
1. Make all array usage use the pointer to an array and GEP. So that it doesn't matter if an array was defined locally or if it was passed in as a parameter.

### Returning from a function
1. Update the signature of the function: insert a new parameter that is a pointer to an array (the same type as the return type) and make the return type void
1. At the call site, `alloca` space for the returned array on the stack then pass the pointer to the array as the out parameter for the function.
1. In the function, at the point of return, copy the array value to the location pointed to by the out parameter.
1. This is why Rust has compile time array sizes: if the size of an array returned by a function was dynamically set within the function, then Rust would not know how much space to allocate for the array at the call site.  (How does Clang deal with this?)

## FAQ
1. What about an empty array value `[]` what type does that resolve to? What's the difference between an array with no values and the unit?
2. In the line `let a = [1, 2, 3]` what is `a`? `a` is a pointer to the space in memory where the array begings.  The pointer to the first element in the array.  In the case of LLVM, `a` would be the value returned by `alloca`.
3. Do arrays embed information about their size? No, because arrays have fixed compile time size, they do not need to embed information about their size into the array. This lets the array also be a core model for how contiguous memory works in the hardware.  If we want to do bounds checking on indices, then that can be computed at compile time because the size for an array
will be known at compile time.
4. What about when we need dynamically sized segment of memory? For things like building memory buffers, creating vectors, creating strings, etc.? Dynamically sized buffers will need to go into the heap. So a separate heap only array can be created once I have built the heap system and memory management for Bramble.
5. Something I noticed in the Rust examples: some of hte GEP calls use `i64` for the index and some use `i32`? Why does this happen?
6. In Rust, what happens if I make the parameter for a function mutable (`fn test(mut a: [i32; 5])`)? It's locally mutable within the function but the mutations should not be seen outside the function. Rust does this by making a copy of the array at the call site and passing that copy to the function, rather than passing the original.

## Tests
1. Struct with an array field
1. Struct with multiple array fields
1. Array of structs
1. Array of structs that have an array field
1. Struct with a field that's an array of structs

### Bugs
1. When using a struct in the array type of a binding it must be the canonical path or I get an error.  I must have forgotten to canonize array types
1. Parser fails when I put the [] operator after a member dereference
