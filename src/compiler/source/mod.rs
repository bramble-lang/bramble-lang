//! This module abstracts out the source code from which the compiler derives
//! it's input data.  The [`Source`] module provides a common way to reference
//! and retrieve the source code used by the compiler.

use std::{
    fmt::{Display, Write},
    ops::AddAssign,
};

mod source;
mod sourcechar;
mod sourcemap;
mod span;

pub use source::{LineNumber, Source};
pub use sourcechar::{SourceCharIter, SourceError};
pub use sourcemap::{SourceMap, SourceMapEntry, SourceMapError};
pub use span::{SourceIr, Span};

/// Represents a single char from a source code file.  This includes the character
/// and the global offset of the character (which points to the specific source
/// code file and location within that file of this character)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SourceChar {
    offset: Offset,
    c: char,
}

impl SourceChar {
    /// Create a new SourceChar
    pub fn new(ch: char, offset: Offset) -> SourceChar {
        SourceChar { c: ch, offset }
    }

    /// Get the character value
    pub fn char(&self) -> char {
        self.c
    }

    /// Get the global offset of this character
    pub fn offset(&self) -> Offset {
        self.offset
    }

    pub fn is_alphabetic(&self) -> bool {
        self.c.is_alphabetic()
    }

    pub fn is_alphanumeric(&self) -> bool {
        self.c.is_alphanumeric()
    }

    pub fn is_digit(&self) -> bool {
        self.c.is_digit(10)
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
/// code that has been input or generated for this Bramble project.  The [`SourceMap`]
/// construct manages the the Offset indexing and will convert a given offset to
/// the actual code source (e.g. the file name)
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct Offset(u32);

impl Offset {
    pub fn new(o: u32) -> Offset {
        Offset(o)
    }

    /// Converts a global offset into the local offset of a source file
    pub fn to_local(&self, base: Offset) -> u64 {
        (self.0 - base.0) as u64
    }

    /// Return the value of the offset as a u32.  This is for use with IO
    /// layers (e.g. serializing to JSON or printing to the screen)
    pub fn as_u32(&self) -> u32 {
        self.0
    }
}

impl AddAssign<u32> for Offset {
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs
    }
}

impl std::fmt::Display for Offset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}
