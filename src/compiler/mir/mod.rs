/*!
 The middle intermediate representation for the Bramble compiler.
 This models functions as [Control Flow Graphs](https://en.wikipedia.org/wiki/Control-flow_graph)
 and makes lifetime, consistency, and other analyses easier to
 implement.

 This module consists of the following major tools:

 1. IR Model: a set of types which are used to represent program in CFG form.
 2. MIR Compiler: this will convert an AST into a MIR representation.
 3. Analysis: tools used for traversing and transforming the MIR representation
 that is generated from the MIR compiler.
*/

use self::ir::Procedure;

use super::Span;

mod builder;
mod ir;
mod project;
mod test;
mod typetable;

// imports which will be made accessible outside of this module
pub mod transform;

// Unit test modules
#[cfg(test)]
mod typetable_test;
#[cfg(test)]
mod project_test;

pub struct Module {
    funcs: Vec<Procedure>,
    span: Span,
}
