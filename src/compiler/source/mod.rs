//! This module abstracts out the source code from which the compiler derives
//! it's input data.  The [`Source`] module provides a common way to reference
//! and retrieve the source code used by the compiler.

use std::ops::AddAssign;

mod sourcechar;
mod sourcemap;

/// Represents a single char from a source code file.  This includes the character
/// and the global offset of the character (which points to the specific source
/// code file and location within that file of this character)
pub struct SourceChar {
    offset: Offset,
    c: char,
}

impl SourceChar {
    pub fn new(c: char) -> SourceChar {
        SourceChar {
            c,
            offset: Offset(0),
        }
    }

    pub fn char(&self) -> char {
        self.c
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Offset(u32);

impl Offset {
    pub fn inc(&mut self) {
        *self += 1;
    }
}

impl AddAssign<u32> for Offset {
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs
    }
}
