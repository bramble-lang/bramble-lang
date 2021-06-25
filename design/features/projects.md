# Projects
Goal:
1. Allow code to be organized across multiples files.  This is a project.
2. Allow a project to reference code in another project.  "importing" or "linking".

Item 2, of course, depends upon item 1.

## Project
A project is a set of source code files that compile into a single output binary.

The design of Braid's project system is simple: a project is a directory that contains Braid source
code files.  A user gives the compiler the directory path and all Braid source files in the directory
(and recursive subdirectories) are compiled as part of the single project.

Internally, Braid links every source file together under a single Module node, called `root`, that
represents the project itself.  This will generate a single LLVM output file that is then compiled
into the final binary.

### Assumptions
Right now, Braid assumes that all projects are executables and has no concept of a library project.
That will presumably change when the second component of this feature timeline is implemented.

## Importing a Project
A user will want to be able to write code that is shared across many different projects (a library).
For this to work, the Braid compiler must support the concept of importing other projects and allowing
a user to reference code from those projects.

At a high level, here are the key steps that must be implemented:
1. How to compile each project.  Is the compiler given a list of all projects and it compiles them or 
one project at a time and then the outputs are linked together?
2. How does the user reference an external project from their code?
3. How is semantic data loaded from an external project so that the user's code can be validated?
4. How are the binaries linked together to generate the final output binary?

Answers:
1. I think it makes the most sense to do one project at a time.  However loading all projects may make semantic
analysis easier.
2. One possibility is to change from "root" to the project name then external projects are referenced using
the paths from the external project name to the target item?
3. If all projects are loaded and the children of an Ur-ur-module then nothing has to really change about the current
system, other than the pathing from the root of a project.  Good reason to implement the `::<Item>` feature _now_?
4. If I go with loading all projects at once then there is no real linking issue as it just becomes a matter of outputting
everything to the same LLVM file.  If I go with multistep compilation (one project at a time) then each will output an
LLVM file that is then linked at a later stage.


### Thoughts
If I go with the "load everything together" approach, I don't want to redesign the path semanttics again.  I want
to leave the `root::` behaving as it currently is with `root::` meaning the root of the project (as I think that's
how any user would think of it).  So instead, I think the right approach would be to create a new path keyword that
for targeting a different project:  `project::std::io::writeln`.

Design wise, I want to keep as much of this implementation and details at the "interface" boundaries between the CLI
and the compiler itself.  This will make it easier to change in the future if I want to.


Rough Draft of Changes:
1. In the parser, the root module is named after the project.  Then `root` becomes a keyword that is synonymous with
the current project.
2. Semantic analysis needs to be able to take a set of projects (where each project is a Module named for the project).  Generate the Item metadata for all of them, then semantic analysis for all of them.
3. The LLVM Compiler needs to take a set of projects (see prev) and compile them into binary output(s).  From here, I 
could still output each as its own LLVM file and then link together.  This would then make it easy for me to slowly
break multiproject compilation apart (I think, compiling one project at a time is preferable but requires more features
to be implemented and so is better to build towards that).
4. Name the LLVM output file after the project rather than `output.ll` or `output.s` or whatever.
5. Name the output binary after the project rather than `output` :).
6. Need to have a policy on picking the name for a project. Will be the name of the directory storing the source files,
but should put that into code.  Also, need to have a policy for when the input is a single file (filename?).

Things to Learn First:
1. How does linking work in LLVM?  How can I output multiple LLVM files and link them together?
2. Do I need to identify Items as external for LLVM? (e.g. will I need to add that metadata to functions during semantic
analysis?)
3. https://llvm.org/docs/LangRef.html#module-structure
4. https://llvm.org/docs/CommandGuide/llvm-link.html
5. I think to link between modules I need to use `extern` in the LLVM bitcode, which means that I need a way to convert
every reference to an external item to the correct LLVM label of that item. (Which I think can be done with the changes
I proposed making to the path concept by adding project name as the root of the project)


Changes to make:
1. use project name for root module and output file
2. Create `project::` path keyword.
3. `project::<current project>::item` is the same as `root::item`
4. Then start work on multiproject compilation
5. There are only 3 ways for a user to create a canonical path in code:
    - `root::<item>`
    - `::<item>`
    - `project::my_proj::item`
Note that the last method is the only one that will allow them to specify the name of the current project.  Without the project keyword prefix, it is not possible for the path resolver to know whether m_proj refers to the project or a module under the project.
6. Remember that `self` refers to the current module.

Notes:
1. The method `    pub fn go_to_module(&self, path: &Path) -> Option<&Module<M>> {` in module.rs has a logical
problem, which is that when you it a path to a module it assumes not that hte path is relative to it but rather
that it is relative to its parent.  That is, it assumes that hte first element in the path will be itself and
so it checks to make sure that the names match.  But that's very hard reason about, most people (myself included)
would expect the path to be relative to the module.  That is, if I called `go_to_module` on module `A` with the
path `B::C`, `A` would check if it had a submodule `B` and the call `go_to` on `B` for the path `C`.  But, instead
it would fail because `A != B`.
    - Stack is the only thing that calls `go_to_module` so I can very easily change the design.
    - The root problem is, root is not a keyword currently it's what is expected to be the name of the module capturing
    all contents of a single file
2. Semantic rules for path `root::item == project::current::item == ::item`
3. Should I create the `CanonicalPath` type to represent canonical paths?  This could be a good opportunity to add
that to my code.
4. One challenge I'm running into is that semantic analysis is using "root" to for teh canonical names of types
and functions.  Need to change that.
5. Another problem is that Path assumes a canonical path starts with "root".  But that's not really true anymore?
Some digging around here is needed.
6. I think the best way to get what I want is to have a different type for Canonical paths?
    - the problem I'm running into is that I have code that expects that a canonical path is one that starts with
    `root` but if root is not the name of the project to search through then things get difficult.  Further, I don't
    want "root" to be in the names items when their LLVM labels are generated.  It should be the project name.
7. Need to distinctly define what a canonical path is. Especially now that "projects" are involved and a path that a
user writes could reference the current project or an external project or it could be relative.  Then define exactly
what the label in LLVM is, which should _not_ include "root" in its name.

Plan 1
0. Need to define how a canonical path will be stored in the system and how it will be represented to the user.
1. Add a flag to Path that indicates if the path is canonical or not.  If it is, then make canonical will do nothing. If it isn't then make canonical will make it canonical and then set the flag to true.
    - to_canonical checks if a path is canonical by seeing if the first element is "root"
    - get_item checks if a path is canonical with the same method
    - From methods also need me to set the flag.  I set them to false here, because the methods are only used when first creating the canonical path
    - DONE
2. If this works, then maybe create a new CanonicalPath type that will allow me to specify what a function wants.
3. I want to remove support for `::item` paths right now, because they are hardcoded to just put "root" at the head of the path. I'd like to get how to map "root" to the canon path figured out first?  (or maybe in to_canon I can change root to the canon path).
4. Change the references from "root::std::io" to "project::std::io".  Here, then in the From fucntions, if the first element is project, I should mark the path as canonical.
    - DONE
5. Need to come up with rules for my_main and how it can be defined.  Can it be in any module, etc?
6. Update the CLI to allow me to specify a project to compile into a binary and a set of import projects to load as libraries (e.g. `--input <src path> --import <path> <path> <path>`).  The `input` source is the one that will determine the name of the output binary.
7. Pull string literals out into constants.
    - DONE