use crate::StringTable;

pub trait CompilerError {
    fn format(&self, st: &StringTable) -> Result<String, String>;
}
