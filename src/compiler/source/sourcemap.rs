use std::path::PathBuf;

use super::{sourcechar::SourceCharIter, Offset, Span};

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

    /// Add a source unit which is a file to the [`SourceMap`]. If the source unit
    /// is successfully added to the [`SourceMap`] then this will return a reference
    /// to the [`SourceMapEntry`] for the given source code unit.  
    ///
    /// The entry provides
    /// an interface for interacting with (e.g. reading) the file that also provides
    /// global offset data about each character read from the file.
    pub fn add_file(&mut self, path: PathBuf) -> Result<(), SourceMapError> {
        let file = std::fs::File::open(&path)?;

        let file_len = file.metadata()?.len();
        if file_len >= u32::MAX as u64 {
            return Err(SourceMapError::FileTooBig);
        }

        /*** Create a Source Char Iterator ***/
        // Get the low offset for the file
        let low = self.offset_high;

        // Increment the offset high so that the file fits within the new range
        self.offset_high += file_len as u32;
        let high = self.offset_high;

        // Add source file to the offset map
        let entry = SourceMapEntry::new(low, high, path);
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
}

/// Tracks the assignment of a range within the global offset space
#[derive(Debug)]
pub struct SourceMapEntry {
    low: Offset,
    high: Offset,
    path: PathBuf,
}

impl SourceMapEntry {
    fn new(low: Offset, high: Offset, path: PathBuf) -> SourceMapEntry {
        SourceMapEntry { low, high, path }
    }

    /// Get the file path for the source code that this entry in the [`SourceMap`]
    /// represents
    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    /// Creates a iterator over the unicode characters in the source code that
    /// this entry represents. Each entry in the iterator will include the
    /// unicode character and it's offset within the global offset space.
    pub fn read(&self) -> Result<SourceCharIter, std::io::Error> {
        let file = std::fs::File::open(&self.path)?;
        Ok(SourceCharIter::new(file, self.low, self.high))
    }
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
