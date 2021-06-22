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