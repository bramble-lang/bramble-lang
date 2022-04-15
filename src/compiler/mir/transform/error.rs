//! Defines the error types which are used by the transformers.

use crate::compiler::mir::{project::StaticDefinitionError, typetable::TypeTableError};

#[derive(Debug)]
pub enum TransformError {
    TypeError(TypeTableError),
    StaticDefError(StaticDefinitionError),
    TypeNotFound,
}

impl From<TypeTableError> for TransformError {
    fn from(tte: TypeTableError) -> Self {
        TransformError::TypeError(tte)
    }
}

impl From<StaticDefinitionError> for TransformError {
    fn from(sde: StaticDefinitionError) -> Self {
        TransformError::StaticDefError(sde)
    }
}
