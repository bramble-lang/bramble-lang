//! This handles the transforming the Bramble AST into its MIR representation.

mod error;
mod transform;
mod function;

pub use error::TransformError;
pub use transform::module_transform;
