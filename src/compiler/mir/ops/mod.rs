//! This module contains different operations which can be applied to a
//! MIR representation of a function.

mod transformer;
mod traverser;

pub use transformer::*;
pub use traverser::ProgramTraverser;
