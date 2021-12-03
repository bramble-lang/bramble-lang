use std::{
    cell::{Cell, RefCell},
    io::{BufWriter, Write},
};

use crate::{
    compiler::{ast::*, diagnostics::Writer, SourceMap},
    StringTable,
};

/// Writes compiler trace data to a JSON file.
pub struct JsonWriter<'a, W: Write> {
    /// Output target for the JSON Writer
    writer: RefCell<BufWriter<W>>,

    /// Maps [`StringId`]s to string values
    string_table: &'a StringTable,

    /// Was there a field before the current field? If so, then add a comma separator
    comma_prefix: Cell<bool>,

    /// Was there an event before the current event? If so, then add a command separator
    event_comma_prefix: Cell<bool>,
}

impl<'a, W: Write> JsonWriter<'a, W> {
    pub fn new(file: W, string_table: &'a StringTable) -> JsonWriter<'a, W> {
        let jw = JsonWriter {
            writer: RefCell::new(BufWriter::new(file)),
            string_table,
            comma_prefix: Cell::new(false),
            event_comma_prefix: Cell::new(false),
        };

        jw.writer.borrow_mut().write("[".as_bytes()).unwrap();

        jw
    }
}

impl<'a, W: Write> Drop for JsonWriter<'a, W> {
    fn drop(&mut self) {
        self.writer.borrow_mut().write("]".as_bytes()).unwrap();
    }
}

impl<'a, W: Write> Writer for JsonWriter<'a, W> {
    fn write_span(&self, field: &str, span: crate::compiler::Span) {
        if self.comma_prefix.get() {
            self.writer.borrow_mut().write(", ".as_bytes()).unwrap();
        } else {
            self.comma_prefix.set(true);
        }

        let field = format!("\"{}\": [{}, {}]", field, span.low(), span.high());
        self.writer.borrow_mut().write(field.as_bytes()).unwrap();
    }

    fn write_field(&self, label: &str, s: &dyn crate::compiler::diagnostics::Writable) {
        if self.comma_prefix.get() {
            self.writer.borrow_mut().write(", ".as_bytes()).unwrap();
        } else {
            self.comma_prefix.set(true);
        }

        let field = format!("\"{}\": ", label);
        self.writer.borrow_mut().write(field.as_bytes()).unwrap();
        s.write(self);
    }

    fn write(&self, s: &dyn crate::compiler::diagnostics::Writable) {
        s.write(self)
    }

    fn write_str(&self, s: &str) {
        let js = format!("\"{}\"", s);
        self.writer.borrow_mut().write(js.as_bytes()).unwrap();
    }

    fn write_stringid(&self, s: crate::StringId) {
        let val = self.string_table.get(s).unwrap();
        self.writer.borrow_mut().write(val.as_bytes()).unwrap();
    }

    fn write_text(&self, s: &str) {
        let js = format!("{}", s);
        self.writer.borrow_mut().write(js.as_bytes()).unwrap();
    }

    fn start_event(&self) {
        if self.event_comma_prefix.get() {
            self.writer.borrow_mut().write(",\n".as_bytes()).unwrap();
        } else {
            self.event_comma_prefix.set(true);
        }

        self.writer.borrow_mut().write("{".as_bytes()).unwrap();
    }

    fn stop_event(&self) {
        self.writer.borrow_mut().write("}".as_bytes()).unwrap();
        self.comma_prefix.set(false);
    }

    fn write_path(&self, p: &crate::compiler::ast::Path) {
        if p.is_canonical() {
            self.write_text("$");
        }

        let len = p.len();

        for idx in 0..len {
            match &p[idx] {
                Element::FileRoot => self.write_text(ROOT_PATH),
                Element::CanonicalRoot => self.write_text(CANONICAL_ROOT),
                Element::Selph => self.write_text(SELF),
                Element::Super => self.write_text(SUPER),
                Element::Id(sid) => self.write_stringid(*sid),
            }

            if idx < len - 1 {
                self.write_text("::");
            }
        }
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
