//! Represents the contents of a unit of Source code (which could be a single
//! file, or a String, etc)

use std::ops::Index;

use super::{Offset, SourceChar, Span};

/// A compilation unit
pub struct Source {
    /// The actual text of the source code along with the global offset of
    /// every character.
    text: Vec<SourceChar>,

    /// The range of the global offset space that this Source code covers
    span: Span,
}

impl Source {
    pub fn new(text: Vec<SourceChar>, span: Span) -> Source {
        Source { text, span }
    }

    pub fn len(&self) -> usize {
        self.text.len()
    }

    pub fn high(&self) -> Offset {
        self.span.high()
    }

    pub fn iter(&self) -> std::slice::Iter<SourceChar> {
        self.text.iter()
    }
}

impl Index<usize> for Source {
    type Output = SourceChar;

    fn index(&self, index: usize) -> &Self::Output {
        &self.text[index]
    }
}

/// A line number in a file
#[derive(PartialEq, PartialOrd, Ord, Eq)]
pub struct LineNumber(u32);

impl LineNumber {
    pub fn new(ln: u32) -> LineNumber {
        LineNumber(ln)
    }
}

impl std::fmt::Display for LineNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}
