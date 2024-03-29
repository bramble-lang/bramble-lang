use std::io::{Bytes, Read};

use super::{Offset, SourceChar, Span};

/// Iterates over each character in a Source File.
pub struct SourceCharIter<R: Read> {
    /// The source code file that is being read.
    chars: UnicodeCharIterator<R>,

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

impl<R: Read> SourceCharIter<R> {
    /// Create a new SourceCharIter from the given Reader with the base
    /// [`Offset`]. The maximum [`Offset`] is also given and will be validated
    /// against as characters are read from the Read, if the [`Offset`] of the
    /// source exceeds the maximum offset (meaning that the source cannot fit
    /// within the range assigned to it by [`SourceMap`] then a panic will happen.)
    pub(super) fn new(reader: R, low: Offset, high: Offset) -> SourceCharIter<R> {
        SourceCharIter {
            chars: UnicodeCharIterator::new(reader),
            low,
            high,
            next_global_offset: low,
            file_offset: 0,
        }
    }

    /// Returns the upper bound for the global offset range that this iterator will
    /// traverse.
    pub fn high(&self) -> Offset {
        self.high
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

impl<R: Read> Iterator for SourceCharIter<R> {
    type Item = Result<SourceChar, SourceError>;

    /// Gets the next character from the source file.
    fn next(&mut self) -> Option<Self::Item> {
        // Get the next char in the file
        self.chars
            .next()
            .map(|ch| {
                self.join_with_offset(ch?)
                    .map(|(o, c)| SourceChar::new(c, o))
            })
            .or_else(|| {
                if self.next_global_offset != self.high {
                    Some(Err(SourceError::UnexpectedEof))
                } else {
                    None
                }
            })
    }
}

/// Convert a stream of bytes into a stream of unicode characters.
struct UnicodeCharIterator<R: Read> {
    bytes: OffsetBytes<R>,
}

impl<R: Read> Iterator for UnicodeCharIterator<R> {
    type Item = Result<char, UnicodeParsingError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.bytes.next().map(|next| match next {
            Ok(lead) => self.complete_char(lead),
            Err(e) => Err(UnicodeParsingError::SourceError(e)),
        })
    }
}

/// Based on code from https://rosettacode.org/wiki/Read_a_file_character_by_character/UTF8#Rust
impl<R: Read> UnicodeCharIterator<R> {
    pub fn new(file: R) -> UnicodeCharIterator<R> {
        UnicodeCharIterator {
            bytes: OffsetBytes::new(file),
        }
    }

    pub fn offset(&self) -> u64 {
        self.bytes.offset()
    }

    /// Parses a unicode character from a byte stream
    fn continuation(&mut self) -> Result<u32, UnicodeParsingError> {
        if let Some(Ok(byte)) = self.bytes.peek() {
            let byte = *byte;

            return if byte & 0b1100_0000 == 0b1000_0000 {
                self.bytes.next();
                Ok((byte & 0b0011_1111) as u32)
            } else {
                Err(UnicodeParsingError::InvalidByte(byte))
            };
        }

        match self.bytes.next() {
            None => Err(UnicodeParsingError::UnexpectedEof),
            Some(Err(e)) => Err(UnicodeParsingError::SourceError(e)),
            Some(Ok(_)) => unreachable!(),
        }
    }

    /// Parses a unicode character from a byte stream
    fn complete_char(&mut self, lead: u8) -> Result<char, UnicodeParsingError> {
        let a = lead as u32; // Let's name the bytes in the sequence

        let result: Result<_, std::io::Error> = if a & 0b1000_0000 == 0 {
            Ok(a)
        } else if a & 0b1110_0000 == 0b1100_0000 {
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
            return Err(UnicodeParsingError::InvalidWord(a));
        };

        result
            .map_err(|e| e.into())
            .and_then(|by| char::from_u32(by).ok_or_else(|| UnicodeParsingError::InvalidWord(by)))
    }
}

/// Iterates over each byte in a file while keeping track of the current offset in the file
struct OffsetBytes<R: Read> {
    byte_offset: u64,
    bytes: std::iter::Peekable<Bytes<R>>,
}

impl<R: Read> OffsetBytes<R> {
    /// Takes a file and creates a CountingBytes iterator which iterates over
    /// the each byte in the file (starting at the given position) and keeps
    /// tracke of the current offset in the file as the bytes are read.
    pub fn new(reader: R) -> OffsetBytes<R> {
        let bytes_read = 0;
        let bytes = reader.bytes();

        OffsetBytes {
            byte_offset: bytes_read,
            bytes: bytes.peekable(),
        }
    }

    /// Returns the offset of the iterators current position in the source
    pub fn offset(&self) -> u64 {
        self.byte_offset
    }

    /// Attempts to peek at the character the iterator is currently point
    /// to.  This will return None if there are no more characters (EOF)
    /// It may return an error if an error happens while trying to read from
    /// he source.
    pub fn peek(&mut self) -> Option<&Result<u8, std::io::Error>> {
        self.bytes.peek()
    }
}

impl<R: Read> Iterator for OffsetBytes<R> {
    type Item = Result<u8, std::io::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.bytes.next().map(|r| {
            r.map(|b| {
                self.byte_offset += 1;
                b
            })
        })
    }
}

/// Errors that can happen when trying to parse a unicode character from a
/// byte stream.
#[derive(Debug)]
pub enum UnicodeParsingError {
    /// A byte was read which is not valid for the current unicode byte sequence being read
    InvalidByte(u8),

    /// Byte sequence was not a valid unicode character
    InvalidWord(u32),

    /// An EOF was encountered while parsing a unicode character
    UnexpectedEof,

    /// An IO error occurred while parsing unicode
    SourceError(std::io::Error),
}

impl From<std::io::Error> for UnicodeParsingError {
    fn from(ioe: std::io::Error) -> Self {
        Self::SourceError(ioe)
    }
}

impl std::fmt::Display for UnicodeParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnicodeParsingError::InvalidByte(by) => {
                f.write_fmt(format_args!("Invalid byte: {}", by))
            }
            UnicodeParsingError::InvalidWord(w) => f.write_fmt(format_args!("Invalid word: {}", w)),
            UnicodeParsingError::UnexpectedEof => f.write_str("Unexpected EOF"),
            UnicodeParsingError::SourceError(ioe) => f.write_fmt(format_args!("IO Error: {}", ioe)),
        }
    }
}

/// Errors that can happen when trying to read from a source code stream.
#[derive(Debug)]
pub enum SourceError {
    ExceededAssignedRange,
    OffsetExceededMaxSize,
    UnexpectedEof,
    UnicodeError(UnicodeParsingError),
    Io(std::io::Error),
    FromUtf8Error(std::string::FromUtf8Error),
    SourceNotFound(Span),
}

impl From<UnicodeParsingError> for SourceError {
    fn from(ce: UnicodeParsingError) -> Self {
        SourceError::UnicodeError(ce)
    }
}

impl From<std::io::Error> for SourceError {
    fn from(io: std::io::Error) -> Self {
        Self::Io(io)
    }
}

impl From<std::string::FromUtf8Error> for SourceError {
    fn from(e: std::string::FromUtf8Error) -> Self {
        Self::FromUtf8Error(e)
    }
}

impl std::fmt::Display for SourceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SourceError::ExceededAssignedRange => f.write_str("Assigned source map range exceeded"),
            SourceError::OffsetExceededMaxSize => f.write_str("Exceeded max offset size"),
            SourceError::UnexpectedEof => f.write_str("Unexpected EOF"),
            SourceError::UnicodeError(ue) => f.write_fmt(format_args!("Unicode error: {}", ue)),
            SourceError::Io(ioe) => f.write_fmt(format_args!("IO Error: {}", ioe)),
            SourceError::FromUtf8Error(ue) => {
                f.write_fmt(format_args!("UTF8 parsing error: {}", ue))
            }
            SourceError::SourceNotFound(span) => {
                f.write_fmt(format_args!("Source not found for span: {}", span))
            }
        }
    }
}
