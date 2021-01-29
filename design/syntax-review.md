# Reviewing the current syntax of Braid
## Goal
Review my current syntax design (and semantic design too, where appropriate) and see
how much I like it and where things are rough or feel wrong or are not intuitive.

### How I will judge
I don't yet know how I will judge or even measure the quality of the syntax in Braid.
Part of why I am forcing myself to do this, is to finally sit down and come up with
an answer to that question. My expectation, is that I will have to do this several
times before I really come down with a solid methodology and set of questions that
I can use to judge whether my syntax design is good or not. 

This is, of course, granting that these things are entirely personal and subjective.
The secondary, though in the long term more important, goal is to use this review
to also begin to reify what expectations I want the syntax design of this language
to meet; as I cannot judge the quality of the design without expectations against
which to compare it.

## The Current State of the Syntax
Right now, the language's syntax is heavily influenced by a Rust. Because, I have
been using Rust most recently and have really enjoyed it, but also, because Rust
features many similarities with F# which may be my favorite language to write in.
So specific designs in Rust's syntax are often extra appealing to me because they
resonate with their corallaries in F#.

However, one problem with my language's design is that it's been growing a reactive
and localized way.  Meaning, when I am adding a new feataure to the language that
requires new syntax, I am coming up with the syntax that is easiest to add to the
Parser and which satisifies the needs of the feature, without putting a lot of thought
into how it fits within the whole of the grammar. The same with regards to any new
semantics.

This is, probably, unavoidable, given that this is my first attempt to design a 
language and therefore I have no real intuition about how to manage a language
design. So it's better for me to just do it, get the feature working, see how my
changes cause the whole of the language to change and judge the consequences and use
that to learn from. Reinforcing the need to sit down and do this review.  Also, I
will probably continue to work in this fashion until I develop an intuition about
syntax design and solidify design principles for Braid.  I just have to regularly
sit down and review my design and go back and refactor things that I do not like.

### Accomplishments
Since I started building this language at the end of October, I have added the following
features:

1. Functions, Coroutines, and variables. Integers, strings, and booleans
2. Mathematical expressions and boolean expressions
3. Return statements, yield statements, and yield return statements
4. If expressions, print statements, and expression blocks
5. Structures
6. Modules and referencing modules

Secondary things, but noting because I am on a roll:
1. Integration testing framework
2. Linux and MacOS support.  Migrated from x86 to x64

What I like about the language:
1. I like that mutating a variable requires `mut x := ...`, because it creates a 
nice symetry with `let x := ...` and because it makes it obvious when scanning down
a block of code that a specific line is mutating a variable. The reason I originally
did this was because my parser was not capable of looking ahead so I needed a token
that would immediately indicate a mutation.
2. I like `yret` to return to the function which originally yielded to the coroutine
because it explicitly indicates that it is yielding back to what originally yielded
to the current coroutine: the direct object is easily inferred. While the `yield ..`
wants the direct object to be explicit. So, I am in favor of assymetric syntax
around coroutine semantics.

What I don't like:
1. The return statement. Right now the return statement can only be used at the end
of a function or coroutine (and must appear there).  I think I much prefer the F#
style of not having an return statement at all and the value that a function resolves
to is the final value of the expression block.
2. The syntax for describing a coroutine type: `co i64` (a coroutine that returns
i64). I think it elides the fact that it is a routine and the `i64` is a resolution
of the routine.  So maybe something like `co () -> i64` or `co -> i64` which 
foregrounds the similarity between a coroutine and a function. Coroutines cannot
be passed parameters after initialization, yet, but I expect to add that feature
so the similarity with functions will become even more real.
3. `:=` for assignment.  Just make this `=`, it fucks me up constantly.
4. Prepending `root::` to canonical paths.  It makes things like referencing items
in imported modules look ugly and confusing.  I think, this is something I need to
put some thought into, regarding paths, canonical paths, and importing modules and how
that will look semantically.

## Design Goals
1. It should be easy to intuit and infer what can be done with the language with
simple understanding of the basic elements of the language.  I am inpsired in this
by F# and how everything seemed to recursively reduce down to the basic expression
and pattern matching: and that you could build complex concepts in simple ways
by recursively applying expressions. And knowledge of how expressions work will 
allow you to intuit what could be done.
2. I believe 1 will also be helpful in being the foundation for a language that can
organically grow and add new features in a way that will be consistent and holistic.

## How to Review
1. Write an actual program in Braid and see how everything feels.
- Tower of Hanoi
- Factorial calculator
- Fibonacci Sequence calculator
2. Look at other languages and write down what I like and take a look at their
grammars (the EBNF)
