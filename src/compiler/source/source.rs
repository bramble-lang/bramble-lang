//! Represents the contents of a unit of Source code (which could be a single
//! file, or a String, etc)

use std::{iter::FromIterator, ops::Index};

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

impl FromIterator<SourceChar> for Source {
    fn from_iter<T: IntoIterator<Item = SourceChar>>(iter: T) -> Self {
        let mut text = vec![];

        for sc in iter {
            text.push(sc)
        }

        Source {
            text,
            span: Span::zero(),
        }
    }
}
