use std::fs::File;
use std::io::{Bytes, Read, Seek};

use super::Offset;

/// Iterates over each character in a Source File.
pub struct SourceCharIter {
    /// The source code file that is being read.
    chars: CharIterator,

    /// Marks the beginning of the global offset range that is assigned to this
    /// Source Unit
    low: Offset,

    /// Marks the end of the global offset range that is assigned to this Source
    /// Unit.  The sum of `low` and the length of the Source Unit must equal
    /// `high`.
    high: Offset,

    /// The Global Offset of the next character read from the Source Unit.
    next_global_offset: Offset,

    /// The current file offset (the local offset)
    file_offset: u64,
}

impl SourceCharIter {
    /// Create a new SourceCharIter from the given Reader with the base
    /// [`Offset`]. The maximum [`Offset`] is also given and will be validated
    /// against as characters are read from the Read, if the [`Offset`] of the
    /// source exceeds the maximum offset (meaning that the source cannot fit
    /// within the range assigned to it by [`SourceMap`] then a panic will happen.)
    pub fn new(reader: File, low: Offset, high: Offset) -> SourceCharIter {
        SourceCharIter {
            chars: CharIterator::new(reader),
            low,
            high,
            next_global_offset: low,
            file_offset: 0,
        }
    }

    /// Get the [`Offset`] of the last character read from the character stream.
    pub fn offset() -> Offset {
        Offset(0)
    }

    /// Will join the character with the current global offset value and then
    /// increment the global offset value to point to the start of the next character.
    ///
    /// Returns an error if the source mapping has moved into an invalid state.
    fn join_with_offset(&mut self, ch: char) -> Result<(Offset, char), SourceError> {
        match ch {
            // Check that the offset for the character does not exceed the assigned
            // range for this Source Unit
            _ if self.next_global_offset >= self.high => Err(SourceError::ExceededAssignedRange),
            // Check that the file has not exceeded the maximum allowed range for global offsets
            _ if self.chars.offset() >= u32::MAX as u64 => Err(SourceError::OffsetExceededMaxSize),
            // Group the read character along with the offset of its first byte in the file
            ch => {
                let ch_offset = self.move_offset_to_next_char();

                Ok((ch_offset, ch))
            }
        }
    }

    /// Moves the offset to point to the start of the next character in the file.
    /// Returns the previous value of the offset pointer.
    fn move_offset_to_next_char(&mut self) -> Offset {
        let old_offset = self.next_global_offset;

        // Update the offset in the file and the next available global offset
        let new_file_offset = self.chars.offset();
        self.next_global_offset += (new_file_offset - self.file_offset) as u32;
        self.file_offset = new_file_offset;

        old_offset
    }
}

impl Iterator for SourceCharIter {
    type Item = Result<(Offset, char), SourceError>;

    /// Gets the next character from the source file.
    fn next(&mut self) -> Option<Self::Item> {
        // Get the next byte in the file
        self.chars
            .next()
            .map(|ch| self.join_with_offset(ch?))
            .or_else(|| {
                if self.next_global_offset != self.high {
                    Some(Err(SourceError::UnexpectedEof))
                } else {
                    None
                }
            })
    }
}

struct CharIterator {
    bytes: CountingBytes,
}

impl Iterator for CharIterator {
    type Item = Result<char, CharError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.bytes.next().map(|next| match next {
            Ok(lead) => self.complete_char(lead),
            Err(e) => Err(CharError::SourceError(e)),
        })
    }
}

/// Based on code from https://rosettacode.org/wiki/Read_a_file_character_by_character/UTF8#Rust
impl CharIterator {
    pub fn new(file: File) -> CharIterator {
        CharIterator {
            bytes: CountingBytes::new(file),
        }
    }

    pub fn offset(&self) -> u64 {
        self.bytes.offset()
    }

    /// Parses a unicode character from a byte stream
    fn continuation(&mut self) -> Result<u32, CharError> {
        if let Some(Ok(byte)) = self.bytes.peek() {
            let byte = *byte;

            return if byte & 0b1100_0000 == 0b1000_0000 {
                self.bytes.next();
                Ok((byte & 0b0011_1111) as u32)
            } else {
                Err(CharError::InvalidByte(byte))
            };
        }

        match self.bytes.next() {
            None => Err(CharError::UnexpectedEof),
            Some(Err(e)) => Err(CharError::SourceError(e)),
            Some(Ok(_)) => unreachable!(),
        }
    }

    /// Parses a unicode character from a byte stream
    fn complete_char(&mut self, lead: u8) -> Result<char, CharError> {
        let a = lead as u32; // Let's name the bytes in the sequence

        let result: Result<_, std::io::Error> = if a & 0b1000_0000 == 0 {
            Ok(a)
        } else if lead & 0b1110_0000 == 0b1100_0000 {
            let b = self.continuation()?;
            Ok((a & 0b0001_1111) << 6 | b)
        } else if a & 0b1111_0000 == 0b1110_0000 {
            let b = self.continuation()?;
            let c = self.continuation()?;
            Ok((a & 0b0000_1111) << 12 | b << 6 | c)
        } else if a & 0b1111_1000 == 0b1111_0000 {
            let b = self.continuation()?;
            let c = self.continuation()?;
            let d = self.continuation()?;
            Ok((a & 0b0000_0111) << 18 | b << 12 | c << 6 | d)
        } else {
            panic!("Invalid byte")
        };

        result
            .map_err(|e| e.into())
            .and_then(|by| char::from_u32(by).ok_or(CharError::InvalidWord(by)))
    }
}

/// Iterates over each byte in a file while keeping track of the current offset in the file
struct CountingBytes {
    nread: u64,
    bytes: std::iter::Peekable<Bytes<File>>,
}

impl CountingBytes {
    /// Takes a file and creates a CountingBytes iterator which iterates over
    /// the each byte in the file (starting at the given position) and keeps
    /// tracke of the current offset in the file as the bytes are read.
    pub fn new(mut file: File) -> CountingBytes {
        let nread = file.stream_position().unwrap();
        let bytes = file.bytes();

        CountingBytes {
            nread,
            bytes: bytes.peekable(),
        }
    }

    pub fn offset(&self) -> u64 {
        self.nread
    }

    pub fn peek(&mut self) -> Option<&Result<u8, std::io::Error>> {
        self.bytes.peek()
    }
}

impl Iterator for CountingBytes {
    type Item = Result<u8, std::io::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.bytes.next().map(|r| {
            r.map(|b| {
                self.nread += 1;
                b
            })
        })
    }
}

#[derive(Debug)]
pub enum CharError {
    InvalidByte(u8),
    InvalidWord(u32),
    UnexpectedEof,
    SourceError(std::io::Error),
}

impl From<std::io::Error> for CharError {
    fn from(ioe: std::io::Error) -> Self {
        Self::SourceError(ioe)
    }
}

#[derive(Debug)]
pub enum SourceError {
    ExceededAssignedRange,
    OffsetExceededMaxSize,
    UnexpectedEof,
    CharError(CharError),
}

impl From<CharError> for SourceError {
    fn from(ce: CharError) -> Self {
        SourceError::CharError(ce)
    }
}
