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

mod builder;
mod ops;
mod project;
mod test;
mod typetable;

// imports which will be made accessible outside of this module
pub mod ir;
pub mod transform;

pub use ops::{FunctionTransformer, ProgramTransformer, ProgramTraverser, TransformerError};
pub use project::{DefId, MirProject};
pub use typetable::{MirBaseType, MirTypeDef, TypeId};

// Unit test modules
#[cfg(test)]
mod project_test;
#[cfg(test)]
mod typetable_test;
