use std::{
    cell::RefCell,
    io::{BufWriter, Write},
};

use crate::{
    compiler::{diagnostics::Writer, SourceMap},
    StringTable,
};

/// Writes compiler trace data to a JSON file.
pub struct JsonWriter<'a, W: Write> {
    /// Output target for the JSON Writer
    writer: RefCell<BufWriter<W>>,

    /// Maps [`StringId`]s to string values
    string_table: &'a StringTable,
}

impl<'a, W: Write> JsonWriter<'a, W> {
    pub fn new(file: W, string_table: &'a StringTable) -> JsonWriter<'a, W> {
        JsonWriter {
            writer: RefCell::new(BufWriter::new(file)),
            string_table,
        }
    }
}

impl<'a, W: Write> Writer for JsonWriter<'a, W> {
    fn write_span(&self, field: &str, span: crate::compiler::Span) {
        let field = format!("{}: [{}, {}], ", field, span.low(), span.high());
        self.writer.borrow_mut().write(field.as_bytes()).unwrap();
    }

    fn write_field(&self, label: &str, s: &dyn crate::compiler::diagnostics::Writable) {
        self.writer.borrow_mut().write(label.as_bytes()).unwrap();
        self.writer.borrow_mut().write(": ".as_bytes()).unwrap();
        s.write(self);
        self.writer.borrow_mut().write(", ".as_bytes()).unwrap();
    }

    fn write(&self, s: &dyn crate::compiler::diagnostics::Writable) {
        s.write(self)
    }

    fn write_str(&self, s: &str) {
        self.writer.borrow_mut().write(s.as_bytes()).unwrap();
    }

    fn write_stringid(&self, s: crate::StringId) {
        let val = self.string_table.get(s).unwrap();
        self.writer.borrow_mut().write(val.as_bytes()).unwrap();
    }

    fn start_event(&self) {
        self.writer.borrow_mut().write("{".as_bytes()).unwrap();
    }

    fn stop_event(&self) {
        self.writer.borrow_mut().write("}\n".as_bytes()).unwrap();
    }
}

pub fn write_source_map<W: Write>(mut w: W, sm: &SourceMap) {
    let len = sm.len();
    for idx in 0..len {
        let s = sm.get(idx).unwrap();
        let span = s.span();
        let path = s.path();
        let entry = format!(
            "[{}, {}]: \"{}\"\n",
            span.low(),
            span.high(),
            path.display()
        );
        w.write(entry.as_bytes()).unwrap();
    }
}
