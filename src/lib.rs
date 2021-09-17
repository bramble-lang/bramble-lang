#![allow(dead_code)]
#![feature(box_syntax, box_patterns)]

pub mod result;

pub mod cli;
pub mod compiler;
pub mod diagnostics;
pub mod io;
pub mod project;

pub use cli::*;
pub use compiler::{llvm, semantics::type_resolver::*, stringtable::*};
pub use io::read_manifests;
pub use project::{get_project_name, parse_project, read_src_files, tokenize_project, Manifest};
