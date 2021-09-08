#![allow(dead_code)]
#![feature(box_syntax, box_patterns)]

pub mod result;

pub mod cli;
pub mod compiler;
pub mod diagnostics;
pub mod io;
pub mod project;

pub use cli::*;
pub use compiler::{lexer::stringtable::StringTable, llvm, semantics::type_resolver::*};
pub use io::read_manifests;
pub use manifest::Manifest;
pub use project::*;
