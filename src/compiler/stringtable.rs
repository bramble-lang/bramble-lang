use std::{cell::RefCell, collections::HashMap, fmt::Display};

use crate::compiler::CompilerDisplayError;

use super::SourceMap;

#[derive(Debug)]
pub enum StringTableError {
    NotFound,
}

impl Display for StringTableError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StringTableError::NotFound => f.write_str("StringId Not Found"),
        }
    }
}

/**
Stores a table of all distinct strings read from source code files.
The different IR structures in the compiler shall use IDs which map
back to the distinct string in the string table

The user can add a stirng to the string table and will be given the
unique ID for that string in return.  If the string already exists in the
table, then the ID for that string will be returned.  If the string does not
exist in the table, then it will be added and a new ID assigned to that stirng
value.

The user can provide a string ID and get the assoicated string value
in return.
 */
#[derive(Debug)]
pub struct StringTable {
    /// The next unique [`StringId`] which has not been assigned to a String
    next_id: RefCell<StringId>,

    /// Table mapping raw strings to their [`StringId`]s. Used for converting
    /// strings read from source code into their [`StringId`].
    table: RefCell<HashMap<String, StringId>>,
}

impl Default for StringTable {
    fn default() -> Self {
        Self {
            next_id: Default::default(),
            table: Default::default(),
        }
    }
}

impl StringTable {
    pub fn new() -> StringTable {
        StringTable {
            next_id: RefCell::new(StringId::new()),
            table: RefCell::new(HashMap::new()),
        }
    }

    /// Inserts a string into the table and returns the assigned ID for that
    /// string value.  If the string is already in the table, then this will
    /// simply return the already assigned ID for that string. Otherwise, it
    /// will add the string to the table and assign it a unique ID.
    pub fn insert(&self, s: String) -> StringId {
        let mut table = self.table.borrow_mut();
        *table
            .entry(s)
            .or_insert_with(|| self.next_id.borrow_mut().get_and_inc())
    }

    /// Search the string table for the given string and, if found, return the
    /// associated [`StringId`]. If not found, then return [`None`](Option::None).
    pub fn find(&self, s: &str) -> Option<StringId> {
        let table = self.table.borrow();
        table.get(s).copied()
    }

    /// Given an ID, if it is assigned to a string, then return the associated
    /// string, otherwise, return None.
    pub fn get(&self, id: StringId) -> Result<String, StringTableError> {
        let table = self.table.borrow();
        for s in table.iter() {
            if *s.1 == id {
                return Ok(s.0.clone());
            }
        }

        Err(StringTableError::NotFound)
    }
}

#[derive(Clone, Copy, PartialEq, Debug, Default, Hash, Eq)]
pub struct StringId(u32);

impl StringId {
    /// Create a new String ID and initialize it to 0
    pub fn new() -> StringId {
        Self::default()
    }

    /// Increment by one and return the value of the ID before the increment.
    fn get_and_inc(&mut self) -> StringId {
        let old = *self;
        self.0 += 1;
        old
    }
}

impl crate::compiler::CompilerDisplay for StringId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _: &SourceMap,
        st: &StringTable,
    ) -> Result<String, CompilerDisplayError> {
        st.get(*self).map_err(|e| e.into())
    }
}

impl std::fmt::Display for StringId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}
