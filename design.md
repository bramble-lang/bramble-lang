# Design Notes
## 2021-01-12
I am currently working on finishing up support for modules.  The final thing I have to work on is supporting referencing struct definitions in other
modules via a path, e.g. `my_mod::MyStruct`.  I have the semantic analysis done, though there is probably room for more testing (and I realized
that I do not do type checking on routine parameter types to make sure that those types exist!).  What I hope to finish today is the compiler layer:
converting referenced structs into assembly.  The current blocker is looking up struct definitions and how they are arranged in memory, unfortunately,
the design of struct tables tied to the AST tree has proven to be inadequate now: struct definitions could reference other structs in other modules which
would require traversing the AST to find the module and its struct table and then mutate it, while also having the current node mutably borrowed.
With rust, I think the only way to make that design work would be with RefCells and Rcs.  Instead, I have decided to go with a single global StructTable
that maps canonical paths for structs to their memory layout information. This means that struct informatino will be stored separately from variable
information and in a very different format: the downside here is it will no longer mirror the way things are handled in the syntax and semantic layers.
But the reason to keep struct symbols and variable symbols together is to catch name collisions and to handle scoped searches: all things that
matter for semantic analysis but not the compilation.  I have decided that once things move to the compiler layer, all semantic rules can be assumed to
be satisfied and the data can be stored in the way that is most efficient for compilation.

The compiler will be changed so that there is a global table mapping Canonical Paths for structs to their memory data.  There will be a stage in the
process which takes an AST and traverses the AST adding all the structs to the table. Then a stage that resolves struct memory layouts (mirroring what
is already done at the module level)

```
Map: Canonical Path -> StructDefinition

State Transition:
Unresolved -> Resolved
```
The Unresolved state is when the StructTable has been populated with all the struct definitions in the program, but the memory layouts and sizes of
the structs have not been resolved.  `Resolved` is when the structs have all successfully had their layouts and sizes resolved.

The SymbolTable will need to take an AST, traverse the AST and put each StructDef found into the SymbolTable (by its canonical name).  This will
output an `Unresolved` symbol table (or unrealized?).  Then a second function will take the Unresolved table, and attempt to resolve the layout
and sizes of all the structs that are defined within: if this is not possible it will return an error, otherwise it returns a Resolved table.
The Resolved table is what the compiler will use to look up struct definition information.

I want to use the same phantom type concept to create `Path` and `CanonicalPath` types so that I can make sure that all Paths passed to the compiler
are canonical.  This will be supported by allowing only a single function to generate the `CanonicalPath` type from a `Path`.

Things that will be required:
- All fields in a struct will have to use the canonical path to identify their type: this is because the canonical path is required to look the struct
up in the struct table
- Need to compute the canonical path of the structs.  I think that might be in the struct meta data, I should double check.  I want to avoid doing it
in the compiler layer: it should not have to figure out anything necessary to identify items correctly, it should be as dumb as possible on that front.

- Creating an Unresolved StructTable from an AST:
    start at the root of the ast:
        - if it is a module: loop through all the structs, convert the struct name to canonical (it is currently not a path), and add to the StructTable
        - Loop through all the modules: for each module, call this function

## 2021-01-14
### Refactoring the AST
This is something I've wanted to do for a month but kept putting off because: I thought that focusing on other features was more important and
because I didn't have a good sense of where it should go.  This week, I read through the `lalrpop` code and finally got a good idea of how to
design my AST in Rust in a way that balances the value of ADTs and leverages the structs effectively.

Here are the big problems with the current design:
1. The ADT is too big.  Any function dealing with traversing the AST winds up being 200 lines long to deal with all the different cases in the match statement
2. Certain values (e.g. paths) go through state transformations in stages of the compiler pipeline.  For example: paths go from being exactly as written
in the source code to being all canonical paths (absolute paths from `root`).  But it's really hard to remember to make sure that happens for every place
that value appears and theres no way to feel confident that it has happened.  A lot of what time was spent fixing bugs in the module implementation caused
by paths not being canonical when they reached the compiler layer.
3. Working with individual nodes to get specific information or take specific actions is difficult because you have to deal with match statements for everything


What I want:
1. Smaller functions when dealing with an AST
2. A way to enforce state transitions of certain values in the AST (e.g. `Path -> Canonical Path`)
3. Make working with specific types of nodes easier (e.g. if I expect a routine definition, let me restrict to just that rather than allow ANY ast node type)
4. Ideally would let me build some higher order functions that can be used to build a lot of the semantic and compiler transform logic

Basic Design Idea:
1. Mix structs with ADTs: have structs represent specific nodes (e.g. expressions) and then ADTs represent groupings that are related (e.g. FunctionDef and CoroutineDef)
2. Add a generic for the Path and define a `RawPath` that can be transformed into a `CanonicalPath` and let me specify that the AST sent to the compiler
must have `CanonicalPath`

### ADT breakdown:
#### Idea 1
1. Module{routines, structs, modules}
2. Routine: Function|Coroutine
3. Struct
4. Statement Print*|Return|Bind|Yield|Mutate|YieldReturn {expression}
5. Expression: ExpressionBlock|actual expression
6. ExpressionBlock: {[statements]* + [expression]}
7. Literal: Integer|Bool|String
8. Variable: Identifier|MemberAccess

#### Idea 2
1. Module{items, modules}
2. Items: Struct|Function|Coroutine
3. Statement: Print|Return|Bind|Mutate|YieldReturn + Expression + ;
4. Expression: ExpressionBlock|_expression_
5. ExpressionBlock: {[statements]* + [expression]}

One thing that's clear to me is that I need to write out the BNF for my language and get
the syntacts clearly defined to make this easier.  My language has grown a bit and is
at risk of becoming hairy.

Next things to tackle:
1. Do x64 compilation and get it to compile on my Mac and on my Linux box
2. Design the memory/hw model abstraction to make it easier to write logic like the coroutines, or structs, which need to interact with specific locations
in memory.
3. Take coroutines to the next level and support doing yields in a function that was called by a coroutine.
4. Multithreading support!
5. C Interop ABI: take a look at how Rust does this and some other languages, but I think I can start with a super basic design even if it is finicky.
## 2021-01-16
The design philosophy of the interface between different compiler stages.  The interface is the data structure that
is specified as the input for a layer.  Usually this the AST, but in the case of the parser it is a stream of
tokens, and for the lexer it is the raw text of the source code. Each stage expects specific information to have been
added to the AST in order for successful completion (for example, the compiler stage expects to know the types
of all expressions and variables and would love to know that types are consistent). To enforce the correct flow
of data and analysis, the AST is given a generic parameter that specifies a metadata type, this type stores the
analysis results of its origin stage, and every layer specifies what metadata type they require.

The Parser layer generates and AST<ParserInfo>, the semantic analysis layer generates AST<SementicMetadata> (containing
the resolved type of every node in the AST), and, there's an internal stage in the compiler that generates AST<Scope>
(containing memory layout, offset, size information, and other hardware level details).  The compiler layer, requires
AST<SemanticMetadata> as its input, thereby ensuring that the AST has already been validated for semantic soundness
and that the compiler can safely assuming things like: any variable referenced has been declared and has a type, etc.

This overall pattern of enforcing the different states that data structures pass through provides a way to enforce
that certain tasks have been done and to allow us to make more and more assumptions about the AST as we move further
into the compiler process.

Another example of this is the `StructTable` type in `compiler`, this type stores a global table of every single
struct defined in the program, along with its size in bytes, and the relative position in memory of every field in
the struct. It has 3 states. The first is the initial state when all structs along with their definitions have
been added but the sizes and offsets have not been computed. The second state is when all the size and offset
information has been successfully computed and now it can be used by the compiler to compute addresses for fields
for reading and writing. The third state is an Error state, which is used when certain invariants are violated:
struct names are duplicated, a struct references another struct which does not exist, a struct is recursive; these
errors all mean that the struct cannot have its size properly resolved. The Braid compiler makes this explicit
having the `UnresolvedStructTable` and `ResolvedStructTable` types

The `Path` and `CanonicalPath` idea mentioned above is another instance of this philosophy: using types to enforce
a specific order of steps and to help guarantee that those steps have been executed.

Part of this design philosophy is that only the initial value in each state machine can be created by the user.
All other "states" must come as output from the transformation function: e.g. the AST can only be created by
passing a token stream to the Parser, and a CanonicalPath can ony be created by calling `to_canonical` on
a Path value and providing all the data necessary to convert from a relative path to an absolute path.

# 2021-01-18
I went with Idea 2 from above and believe it has worked pretty well.

The AST redesign and refactor is now in a good place.  There is still room for improvement, but I believe that can wait while I focus on some other tasks.
However, one thing I will make sure I do is take an hour or two to clean up and rearrange the code.  After the massive work done to support paths and
the major refactoring, and the fact that the code had gotten messy, the code base could benefit greatly from just moving functions around, grouping things
into files, and doing some renaming to improve clarity.

A couple things I want to do in the future:
1. Make a Parsable trait that types implement if they are parsable syntactic elements. This will couple with a ParserResult type that will allow me to
implement a nice parser Combinator setup.  I tried to do this today, but ran into issues trying to figure out how to write the ParserResult HOFs which
might return different types (e.g. Statement would be Print or Bind or Mutate or Return. I thought I had a  prototype of this working a few days ago
but could not get it working today)
2. Follow the above pattern for type checking: have a TypeCheck trait that syntactic elements implement with their specific type validation logic and
which serves as the transition machine to go from Parser to Semantic states.
3. Create a trait for metadata, maybe create a `derive` macro for metadata, if I'm ambitious
4. Add in logging that I can turn on which will provide details on what the compiler is doing, this would largely be independent of the source code
(unlike the tracing options).  I could use it to tell the compiler is moving from lexing to parsing, how much time was spent on specific stages, stats
like how many of each node type were visited in each stage (to validate that data is not lost and that the AST is not being changed topologically), etc.
5. A new error type that captures the metadata from the node, then I can format the error message with the line number in a single place and not have to
write every single error message with the Line number format.

## Thoughts on scoped mutablility
One of the ideas I've had for a few years in the ability to restrict where a variable is mutable to a scope that is smaller than the scope it is defined
in.  Or to limit it to only be mutable in a single location, but readable anywhere.

For example:
```
// This would be legal
example1() {
    var x = 5;
    while predicate() {
        if something(x) < 10 {
            x += 1;
        }
        print(x);
    }
    post(x);
}

// But this would be illegal
example2() {
    var x = 5;
    while predicate() {
        if something(x) < 10 {
            x += 1;
        }
        print(x);
        x += 2;  // mutated in two places would violate the single mutation rule
    }
    post(x);
}
```

what problem does this solve? It increases the ability to intuit what is happening in code by telling me exactly how often a variable can be changed,
if it can only be changed in a single place, then when I read that line of code I know everything about what that variable represents.  And every time
I read the variable I can rely on knowing that it was never changed anywhere else in the code.  I think there is a relationship between this is what
we think about with inconsistency in data bases.

In a way, this is letting me create custom semantic rules for the compiler to follow.  Like what phantom types do.  And it makes me wonder if there is
a more general concept here that I can expand this to cover?

Something that could be related to this concept:
https://en.wikipedia.org/wiki/Substructural_type_system#:~:text=Different%20substructural%20type%20systems%20%20%20Ordered%20,Allowed%20%20%20Allowed%20%20%20Arbitrarily%20

According to Wikipedia Rust supports Linear or Affine, maybe I should look into this and it would help me better understand Rust's type system and semantic rules
 
## Unrelated I think but maybe worth looking into a bit
Effect Systems: https://en.wikipedia.org/wiki/Effect_system What are they?

# 2021-01-19
Today I will convert my compiler to output in x64 rather than x86.  Once I have it working on Linux I will make the changes needed to support MacOS.

For supporting Mac, the two things that I know I must do so far:
1. Addresses need to be relative rather than absolute: [rel my_label] instead of [my_label].  I have tested this out on Linux and it also works there
so I will just use relative addresses by default.
2. The name of key functions begins with an `_`.  So my main function is `_main` and I need to use `_printf` rather than `printf`.  I will set this up
as a compiler flag, if you target `macho64` (I am using `macho64` because that's the same flag nasm uses for MacOS, it looks like it comes from this:
https://en.wikipedia.org/wiki/Mach-O)

I had two thoughts about how to implement this: one is a converter that takes the stream of x86 instructions and converts them to x64 instructions.
The other is to just wholesale convert the compiler to use x64.

The biggest drawback of the first option is that I have to change the ABI for C interop in 64 bit (it uses registers instead of the stack) and doing
that in a converter would be a pain in the ass.  The other question is why support 32bit right now?  All my dev work and everyone I know would be using
64bit machines and to support multiple platforms I would rather hop over to LLVM and to implement an IL layer.

# 2021-01-21
## Thoughts about Coroutines
I have a goal to update the language to allow for `yield` to work even when called within a function that is in
the call stack of a coroutine.  For example, let `c` be a coroutine and `main` and `f` be functions; `main`
yields to `c`, then `c` calls `f`, then `f` does `yret`: this would behave just as if `yret` was called in `c`,
control is yielded back to `main` and if main calls `yield c` again, then execution would resume in `f` immediately,
after the `yret`. If `yret` is invoked by `f` but is _not_ in the call stack of a coroutine, then it will behave exactly as if 
it were invoking a `return`.

This raises a question about semantics when handling communication between routine primitives.  Currently,
`yret` takes a value which matches the coroutine's return type and returns that value along with control to
the invoker (in the example, `main`).  But this assumes and requires the return type to be defined and to be
consistent: the invoker needs to know what type will be returned by a yield so that it can allocate storage
and properly compile and work with the value returned.  

But then this would mean that `f` maintain the same type requirement when it invokes `yret`. How could `f` possibly
know the `yret` type of the coroutine which called it?  In addition, for `yret` to behave like `return` when `f` is
not in the callstack of a coroutine its type must match the defined return type of `f`. This creates a knowledge requirement
where the User must be aware of the internal workings of `f` when writing `c`, and, when writing `f` they must be
either aware or prescient about the internal workings of any calling coroutine.

There are at least two possible ways forward that I can think of:
1. The Semantic Analysis will only let a coroutine, `c`, call a function, `f`, which invokes `yret` if the
return type of `f` matches the return type of `c`.  And it is a gobal rule that `yret` must match the return
type of the containing function.
2. There is a different primitive for communication between routine primitives and coroutines which allows for
asymetric knowledge of code implementations.  E.g. channels in Go.
3. `yret` can only be used in a coroutine, but coroutines can be called as functions or birthed as coroutines.
If called as a function, then its return type must match the caller coroutines return type, if called within
a coroutine (if called within a function it does not matter).  If created as a coroutine, then it behaves as
coroutines currently do: `yret` returns the line immediately following the `yield` in the routine (function or
coroutine) which invoked the yield.  This satisfies the asymetric knowledge requirement as the fact that a `yret`
may exist is now explicit in the definitin of the routine (if its a coroutine then you must be prepared to deal
with yield returns in one way or another) but does not require knowledge of the internal implementation.

Follow ups:
1. The language Wren supports this behavior in functions called by coroutines, take a look at its semantics and
see what it does to resolve this problem.  Is Wren dynamically typed? That would explain how it could solve this
problem.
2. Look into Stackless vs. Stackful coroutines: https://blog.varunramesh.net/posts/stackless-vs-stackful-coroutines/
3. Lua also supports coroutines: https://www.lua.org/pil/9.1.html.  Lua's docs on coroutines seems very well
written and informative about coroutines, worth my time to read it.

Additionally, is there a type concept I can come up with to differentiate between functions and coroutines?
How do they differ?  I think a lot will depend on how I implement coroutines: if I can pass data to and from
at the call point, then the type will need to capture that.  Likewise, if functions can yield when inside a
coroutine maybe that will need to be captured

# 2021-01-24
## AST Design
I am still not happy with the design of the AST. Following the design I saw in `lalr_pop` was still a significant
improvement, but does not address the challenge of simplifying traversal of the tree as much as possible. My hope
was that the `lalrpop` design would address the tree traversals in some way: either by solving it or by simplifying
the code enough to make it a non-issue.  But it really hasn't: the code is much better organized and easier to
reason about, but tree traversals still require writing specialized code.

I want to update the design to allow for writing as generic as possible code in the semantic analysis stages:
to avoid, when at all possible, doing match expressions on enums.

So, when writing a tree traversal algorithm, here's what I would need in order to make it generic:
1. Be able to recursively iterate over the children
2. Be able to map the children to transform them
3. Be able to take a node, update it's annotation, and create a new instance of the node with a new annotation
type. For example: Take a node from the parser AST and create the semantic annotation and then create a copy
of the node but with SemanticAnnotation

So, if I do a trait I would want it to have:
1. A way to iterate over the child nodes
2. A way to transform the annotation on the node and it's children and output a copy of the AST data but with
new annotation type: `map: (AST<T>, T -> U) -> AST<U>`.
3. A way to get a list of the child nodes

So, I am differentiating between 1 and 2 because some nodes (e.g. if statements) will have children that are each
fundamentally different in there semantic role and when you return just a list or an iterator over them you remove
that semantic meaning.