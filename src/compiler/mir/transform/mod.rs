//! This handles the transforming the Bramble AST into its MIR representation.

mod error;
mod transform;

pub use error::TransformError;
pub use transform::*;
