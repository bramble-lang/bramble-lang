use crate::StringTable;

pub type Result<T> = std::result::Result<T, String>;
pub type NResult<T> = std::result::Result<T, Vec<String>>;
