# Tender Love and Care
The goal of this document is to find root causes of designs that are causing me problems with
the Braid compiler so that I can then redesign and refactor that code. The general goal will
be to document symptoms (code that is hard to work with, easy to break, overloaded, etc) and
find the root causes of those problems.  Or to list out things I would like to do with this
compiler, document what would make them easier, see how I would implement them, then see what
in the extant compiler code inhibits or accelerates such changes.

# Tracing and diagnostic generalization
- Goal: I would like tracing and diagnostics to be automatically incorporated into all semantic
analysis tools, but the traversal operators are hard to implement
- Why? Because of references make tree mutation difficult (borrow rules)
- Why do I need to use the operators?  Why not simply implement it myself?  Because then each time
a new AST feature is added, all traversers will have to add the tracing functionality
- So?  Is that a big deal? Honestly, I don't think so.
- Instead: just give some TLC to that tracing tool you half wrote and use that for everything.
I rarely add new functions to the traversers.

# Want to separate the traversal of trees from the mutators
- Goal: The functions that traverse a tree and generate symbol tables, semantic analysis, type resolution,
etc. all repeate the same traversal logic (woven in with the transformation logic).  I want to
separate the traversal to an operator(s) that can be reused by everything.  But that's been a PITA.
- Why? The nature of trees mean traversing a tree and mutating it create issues with the borrow checker,
I would need to use RCs for everything.  Much of the mutations are to the annotation fields.
- Why? The annotation fields store all the information beyond the node type and the syntactic children
of the node (contextual information).
- Why does it need RCs?  There is a reference from the nodes parent to the node and a reference from the
traversal operator to the node (at the least), so this makes mutating the tree complex and tedious,
especially when a generic way that I would need for an abstract operator.
- Could you be missing something?  Very possibly.  I tried switchign the code to use raw pointers and that
seems fine.
- What about the annotations, could I split them from the tree to solve the multiple borrows issue? 
I could move them to an arena and then mutate them there. But that doesn't address when I would want operators
to mutate the tree itself (e.g. expanding syntactic sugar).
- Is that necessary, do the operators need to change the tree in addition to the annotations?  Eventually,
I think that's guaranteed.  But that doesn't mean I can't do the annotation arena idea.

# There are way too many clones everywhere and the code is clunky
1. Goal: reduce the amount of extra cruft in the code by getting rid of a lot of `clone` calls.
2. Why do I have so many clone calls?  Almost all of them are because the AST values cannot be
copied.
3. Why not? Because they have strings and strings cannot be copied, only cloned.
4. What can be done? Move the strings to an arena (string pool) and use an integer ID instead of the
string itself.
5. Any other ways?  Use references, but these would all link back to the source file that was read from disk.
That means references for thousands of substrings would live from the beginning of the compiler all the way
to the end, being woven in and out of many different ASTs and so on.  I think that this would create an
incredibly complex network of references that would be very hard to reason about and would lead to lots of
annoying borrow checker problems.
6. Are you positive that's the case? Not really.
7. If the work is teh same which would be better? Depends on if &str can be copied or not and if using a string
ID is confusing for the dev.  For the latter, I don't think that will matter, once you get passed parsing you
dont' think about the string at all.  A numeric ID is the same as a string for the developer; it's all about the
borrow checker and how hard it is to write code and if I can get rid of all the clones.
8. Conclusion: check if I can copy a struct with &str then try a 1hr timebox to change AST nodes from String to &str
and see how far I can get.  Otherwise, use the string arena (which is what Rust uses).  Also take some time to
think about the pros and cons.
9. Did a quick test this morning to see how much work it would be and it's quite a bit of work making the changes.
A couple things: 1. Vecs still are not copyable so everything with a Path would still need to be cloned. 2.
Deserialize creates dependencies and adds a lot of trait restrictions that have to be written out.  Item 1 could
be addressed by making a Path arena and all paths are stored there?  Item 2 could probably be addressed by moving
Serde to just be within the Project module and manually implementing the serde.
10. What's the lesson that can be learned from this?

# Canonical Path are critical make them obvious when needed
1. In my code, its critical to know if a path is canonical or not and many parts of logic require canonical
paths (e.g. LLVM IR generation).
2. But when looking at the code it's not obvious if something is canonical or not, nor is it clear that such a
concept really exists (other than clean reading). And it's especially not obvious once you get into code that
assumes that paths are canonical.
3. Why is it essential?  Because the canonical path is the true name for any definition and is needed to look up
the correct definition of type/value/function during semantic analysis and IR generation.
4. Why is it difficult to read?  Canonical and non-canonical paths are both the same type and the only difference
right now is the presence of a boolean flag.  So when looking at the code there's no difference, unless you see
a call to a function that deals with canonical behavior.
5. What can I do?  Create a CanonicalPath type that reprsents the canonical paths and the "Path" types represent
paths that are relative or have not been confirmed to be canonical.

# Code files are Too Big
1. Some of my code files are thousands of lines long
2. Why?  Because the tests are in the same file
3. Why?  Because that's what I learned from the Rust book
4. What should I do? Look at other projects and do some research into Rust testing idioms, maybe the
book is wrong.  Also, regardless, just move the tests to another file, the big problem is access privs.
5. To do: move the tests out to separate files or a separate directory: (/test).  The tests directory is
for integration tests.
6. I see the cpython project puts a `tests.rs` file in the module directories for unit tests. Tokio puts
a `tests` directory under module directories to include unit (I think) tests.
7. Done.  type_resolver.rs is now 1000ln down from 4000 :D