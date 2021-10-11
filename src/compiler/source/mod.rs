//! This module abstracts out the source code from which the compiler derives
//! it's input data.  The [`Source`] module provides a common way to reference
//! and retrieve the source code used by the compiler.

/// Represents a single char from a source code file.  This includes the character
/// and the global offset of the character (which points to the specific source
/// code file and location within that file of this character)
pub struct SourceChar {
    c: char,
}

impl SourceChar {
    pub fn new(c: char) -> SourceChar {
        SourceChar { c }
    }

    pub fn char(&self) -> char {
        self.c
    }
}
