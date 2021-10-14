//! This module abstracts out the source code from which the compiler derives
//! it's input data.  The [`Source`] module provides a common way to reference
//! and retrieve the source code used by the compiler.

use std::{
    fmt::{Display, Write},
    ops::AddAssign,
};

mod sourcechar;
mod sourcemap;
mod span;

pub use sourcechar::{SourceCharIter, SourceError};
pub use sourcemap::{SourceMap, SourceMapEntry, SourceMapError};
pub use span::Span;

/// Represents a single char from a source code file.  This includes the character
/// and the global offset of the character (which points to the specific source
/// code file and location within that file of this character)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SourceChar {
    offset: Offset,
    c: char,
}

impl SourceChar {
    pub fn new(o: Offset, c: char) -> SourceChar {
        SourceChar { c, offset: o }
    }

    pub fn char(&self) -> char {
        self.c
    }

    pub fn offset(&self) -> Offset {
        self.offset
    }

    pub fn from_char(o: u32, c: char) -> SourceChar {
        Self::new(Offset(o), c)
    }

    pub fn is_alphabetic(&self) -> bool {
        self.c.is_alphabetic()
    }

    pub fn is_alphanumeric(&self) -> bool {
        self.c.is_alphanumeric()
    }

    pub fn is_numeric(&self) -> bool {
        self.c.is_numeric()
    }

    pub fn is_whitespace(&self) -> bool {
        self.c.is_whitespace()
    }

    pub fn is_ascii_punctuation(&self) -> bool {
        self.c.is_ascii_punctuation()
    }
}

impl PartialEq<char> for SourceChar {
    fn eq(&self, other: &char) -> bool {
        self.c == *other
    }
}

impl Display for SourceChar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(self.char())
    }
}

/// A unique offset into the global space of all source code.  This offset will
/// uniquely point to a single character or position in the space of all source
/// code that has been input or generated for this Braid project.  The [`SourceMap`]
/// construct manages the the Offset indexing and will convert a given offset to
/// the actual code source (e.g. the file name)
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Offset(u32);

impl Offset {
    pub fn new(o: u32) -> Offset {
        Offset(o)
    }
}

impl AddAssign<u32> for Offset {
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs
    }
}
