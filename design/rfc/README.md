# RFCs Section
This directory stores all design proposals from their first draft 
to their final state. This will help manage full documentation of
the Stet language.

RFCs are named with the following pattern `rfc<Issue>-<title>.md`, the
`<Issue>` is the issue number associated with the GitHub issue that
was created to track the particular RFC document.  The `rfc` prefix
is so that the `README.md` document is at the top of any directory
listing and is easily found.

When an RFC is finalized and moved into active development, the name
of the RFC is appended with `-accepted`. During development, if 
edge cases or changes need to be made to the design the RFC is updated
and kept as `-accepted`.  Once all tasks are complete and merged
the RFC suffix is changed from `-accepted` to `-final`.

## RFC Requirements
All RFCs related to the the Stet language must contain sections 
covering the following design aspects:

All changes will require these sections:
1. UX: A description of the User Experience goals of the new feature.
These requirements must be met by the Syntax and Semantic sections
1. Implementation: A description of how the feature will be implemented 
in the compiler.
1. Proposed Tests: a set of tests that will be done to prove that the implementatoin
works

Language changes will require:
1. Syntax: the actual syntax of the the language feature. Include how
this satisfies the UX goals.
1. Semantics: a description of how the semantic engine, type resolver, etc
will analyze the new feature. Include how this satisfies the UX goals.
1. Insights: the Insight platform is a first class feature of the Stet
compiler and all new language features must describe how they will be
recorded by the Insight platform and how the Insight Viewer will display
them. Include how any changes to the Viewer will satisfy the UX goals.

Compiler changes will require:
1. Documentation on any Compiler Interface changes.
1. Unrecoverable States: these are the unrecoverable states where the compiler
ought to panic.