#![allow(dead_code)]

pub mod result;

pub mod cli;
pub mod compiler;
pub mod diagnostics;
pub mod io;
pub mod project;

pub use cli::*;
pub use compiler::{llvm, semantics::type_resolver::*, stringtable::*};
pub use io::read_manifests;
pub use project::{get_project_name, parse_project, tokenize_source_map, Manifest};
