//! This handles the transforming the Bramble AST into its MIR representation.

mod error;
mod function;
mod module;

pub use error::TransformError;
pub use module::transform;
