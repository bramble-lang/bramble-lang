use std::path::PathBuf;

use super::{sourcechar::SourceCharIter, Offset};

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
    ///
    /// Not a fan of that design, changing to store the low and high.  Can
    /// improve later.
    ///
    /// I probably want to create a simple struct to store the entries.
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

    /// Add a source unit which is a file to the [`SourceMap`].
    ///
    /// What should this return?
    /// A Result with:
    /// 1. The offset range assigned to this file?
    /// 2. The SourceCharIter that iterates over the source unit?
    /// 3. A Source Unit ID that can be used to get things from the SourceMap related to this Source Unit?
    pub fn add_file(&mut self, path: PathBuf) -> Result<SourceCharIter, String> {
        let file = std::fs::File::open(&path).unwrap();

        let file_len = file.metadata().unwrap().len();
        if file_len >= u32::MAX as u64 {
            panic!("File is way too big");
        }

        /*** Create a Source Char Iterator ***/
        // Get the low offset for the file
        let low = self.offset_high;

        // Increment the offset high so that the file fits within the new range
        self.offset_high += file_len as u32;
        let high = self.offset_high;

        // Add source file to the offset map
        self.map.push(SourceMapEntry::new(low, high, path));

        Ok(SourceCharIter::new(file, low, high))
    }
}

/// Tracks the assignment of a range within the global offset space
#[derive(Debug)]
struct SourceMapEntry {
    low: Offset,
    high: Offset,
    path: PathBuf,
}

impl SourceMapEntry {
    fn new(low: Offset, high: Offset, path: PathBuf) -> SourceMapEntry {
        SourceMapEntry { low, high, path }
    }
}
