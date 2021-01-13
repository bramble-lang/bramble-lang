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