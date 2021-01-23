use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub struct SymbolTable {
    pub(super) table: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            table: HashMap::new(),
        }
    }
}

impl std::fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("\tName | Size | Offset\n"))?;
        for (_, symbol) in self.table.iter() {
            f.write_fmt(format_args!(
                "\t{} | {} | {}\n",
                symbol.name, symbol.size, symbol.offset
            ))?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub size: i32,
    pub offset: i32,
}

impl Symbol {
    pub fn new(name: &str, size: i32, offset: i32) -> Symbol {
        Symbol {
            name: name.into(),
            size,
            offset,
        }
    }
}
