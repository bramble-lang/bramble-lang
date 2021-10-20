use std::path::PathBuf;

use super::{sourcechar::SourceCharIter, Offset, Source, SourceError, Span};

const MAX_SOURCE_SIZE: u32 = u32::MAX;

/// The SourceMap keeps a table of input source files and the range of teh Global
/// Offset which maps to that source file.
///
/// When initially created, the SourceMap has no source code units and the Global
/// Offset watermark is 0.
///
/// When a file is added to the SourceMap, it is assigned a low Global Offset and
/// the Global Offset watermark is increased by the size of the source unit in
/// bytes [note].
///
/// [note]: This is ostensibly bytes, but if we have different types of sources, each
/// source type could use a different mapping from Global Offset value to element
/// in the source. What matters is thaself.offset_hight a Global Offset range maps to an element
/// of source code so that the exact code that an IR model represents can be
/// retrieved.  For files, this is the offset range.  For a vector of Tokens it
/// could be the vector index.
#[derive(Debug)]
pub struct SourceMap {
    /// Stores the Source Files and the maximum global offset in the assigned
    /// range. With the file owning the range from the previous high upto but
    /// not including its high.
    map: Vec<SourceMapEntry>,

    /// The upper bound of all the source files currently in the SourceMap
    /// The next added file will have this offset as its low offset and the
    /// offset_high will be incremented by the size of the added source file.
    offset_high: Offset,
}

impl SourceMap {
    /// Creates a new SourceMap with no source units and an initial Offset high
    /// watermark of 0.
    pub fn new() -> SourceMap {
        SourceMap {
            offset_high: Offset(0),
            map: Vec::new(),
        }
    }

    /// Add a file as unit of source code to the [`SourceMap`].
    pub fn add_file(&mut self, path: PathBuf) -> Result<(), SourceMapError> {
        let file = std::fs::File::open(&path)?;

        let file_len = file.metadata()?.len();
        if file_len >= MAX_SOURCE_SIZE as u64 {
            return Err(SourceMapError::FileTooBig);
        }

        /*** Create a Source Char Iterator ***/
        // Get the low offset for the file
        let low = self.offset_high;

        // Increment the offset high so that the file fits within the new range
        self.offset_high += file_len as u32;
        let high = self.offset_high;

        let src = SourceType::File(path.clone());

        // Add source file to the offset map
        let entry = SourceMapEntry::new(low, high, src, path);
        self.map.push(entry);

        Ok(())
    }

    /// Adds a string as a unit of source code to the [`SourceMap`].
    pub fn add_string(&mut self, text: &str, path: PathBuf) -> Result<(), SourceMapError> {
        if text.len() >= MAX_SOURCE_SIZE as usize {
            return Err(SourceMapError::FileTooBig);
        }

        /*** Create a Source Char Iterator ***/
        // Get the low offset for the file
        let low = self.offset_high;

        // Increment the offset high so that the file fits within the new range
        self.offset_high += text.len() as u32;
        let high = self.offset_high;

        let src = SourceType::Text(text.into());

        // Add source file to the offset map
        let entry = SourceMapEntry::new(low, high, src, path);
        self.map.push(entry);

        Ok(())
    }

    /// Returns the number of entries in the [`SourceMap`]
    pub fn len(&self) -> usize {
        self.map.len()
    }

    /// If `idx` is less than the number of entries, then this will return
    /// a reference to the entry at position `idx`.  Otherwise, this will
    /// return [`None`].
    pub fn get(&self, idx: usize) -> Option<&SourceMapEntry> {
        if idx < self.len() {
            Some(&self.map[idx])
        } else {
            None
        }
    }

    /// Returns a Span that covers the entire source code space.
    /// This will return `None` if there is no source code
    pub fn span(&self) -> Option<Span> {
        if self.offset_high > Offset::new(0) {
            Some(Span::new(Offset::new(0), self.offset_high))
        } else {
            None
        }
    }

    /// Returns the file(s) a span covers
    pub fn files(&self, span: Span) -> Vec<&PathBuf> {
        self.map
            .iter()
            .filter(|e| span.intersects(e.span))
            .map(|e| &e.path)
            .collect()
    }

    /// Returns the Line number of a span
    pub fn lines(&self, span: Span) -> Vec<(&PathBuf, Vec<u32>)> {
        // Get the list of files that the span covers
        self.map
            .iter()
            .filter(|e| span.intersects(e.span))
            .map(|file| {
                let lines = self.file_lines(file, span);
                (&file.path, lines)
            })
            .collect()
    }

    /// Returns the lines that a span covers in the given file.
    /// Will return an empty vector if `span` does not intersect the file
    /// at all.
    fn file_lines(&self, file: &SourceMapEntry, span: Span) -> Vec<u32> {
        let mut lines = vec![];
        // Check that span intersects with the file's span in the global offset space
        match file.span.intersection(span) {
            Some(intersection) => {
                // Then search through the file from the beginning until it reaches the
                // start of the span, counting the number of new lines
                // Then from the start of the span until the end of the span or the file
                // Count each new line and add it to the vector
                let text = file.read().unwrap();
                let mut stream = text.iter();

                let mut line = 1;
                let mut prev_line = 0;
                while let Some(c) = stream.next() {
                    if c.offset() >= intersection.low() && c.offset() < intersection.high() {
                        // if line number has changed then push onto the vector
                        if line != prev_line {
                            lines.push(line);
                            prev_line = line;
                        }
                    }

                    if *c == '\n' {
                        line += 1;
                    }
                }
            }
            None => (),
        }
        lines
    }
}

/// Tracks the assignment of a range within the global offset space
#[derive(Debug)]
pub struct SourceMapEntry {
    span: Span,
    source: SourceType,
    path: PathBuf,
}

impl SourceMapEntry {
    fn new(low: Offset, high: Offset, source: SourceType, path: PathBuf) -> SourceMapEntry {
        SourceMapEntry {
            span: Span::new(low, high),
            source: source,
            path: path,
        }
    }

    /// Get the file path for the source code that this entry in the [`SourceMap`]
    /// represents
    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    /// Creates a iterator over the unicode characters in the source code that
    /// this entry represents. Each entry in the iterator will include the
    /// unicode character and it's offset within the global offset space.
    pub fn read(&self) -> Result<Source, SourceError> {
        let text = match &self.source {
            SourceType::File(f) => {
                let file = std::fs::File::open(f)?;
                let iter = SourceCharIter::new(file, self.span.low(), self.span.high());
                let text: Result<Vec<_>, _> = iter.collect();
                text
            }
            SourceType::Text(s) => {
                let iter = SourceCharIter::new(s.as_bytes(), self.span.low(), self.span.high());
                let text: Result<Vec<_>, _> = iter.collect();
                text
            }
        };
        Ok(Source::new(text?, self.span))
    }
}

#[derive(Debug)]
enum SourceType {
    File(PathBuf),
    Text(String),
}

#[derive(Debug)]
pub enum SourceMapError {
    FileTooBig,
    Io(std::io::Error),
}

impl From<std::io::Error> for SourceMapError {
    fn from(e: std::io::Error) -> Self {
        Self::Io(e)
    }
}
