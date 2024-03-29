use std::{
    cell::{Cell, RefCell},
    io::{BufWriter, Write},
    path::PathBuf,
};

use serde::Serialize;

use crate::{
    compiler::{ast::*, diagnostics::Writer, SourceMap, Span},
    StringTable,
};

/// Writes compiler trace data to a JSON file.
pub struct JsonWriter<'a, W: Write> {
    /// Output target for the JSON Writer
    writer: RefCell<BufWriter<W>>,

    /// Maps [`Span`]s to files
    source_map: &'a SourceMap,

    /// Maps [`StringId`]s to string values
    string_table: &'a StringTable,

    /// Was there a field before the current field? If so, then add a comma separator
    comma_prefix: Cell<bool>,

    /// Was there an event before the current event? If so, then add a command separator
    event_comma_prefix: Cell<bool>,
}

impl<'a, W: Write> JsonWriter<'a, W> {
    pub fn new(
        file: W,
        source_map: &'a SourceMap,
        string_table: &'a StringTable,
    ) -> JsonWriter<'a, W> {
        let jw = JsonWriter {
            writer: RefCell::new(BufWriter::new(file)),
            source_map,
            string_table,
            comma_prefix: Cell::new(false),
            event_comma_prefix: Cell::new(false),
        };

        jw.writer.borrow_mut().write_all("[".as_bytes()).unwrap();

        jw
    }
}

impl<'a, W: Write> Drop for JsonWriter<'a, W> {
    fn drop(&mut self) {
        self.writer.borrow_mut().write_all("]".as_bytes()).unwrap();
    }
}

impl<'a, W: Write> Writer for JsonWriter<'a, W> {
    fn write_span(&self, field: &str, span: crate::compiler::Span) {
        if self.comma_prefix.get() {
            self.writer.borrow_mut().write_all(", ".as_bytes()).unwrap();
        } else {
            self.comma_prefix.set(true);
        }

        let field = format!("\"{}\": [{}, {}]", field, span.low(), span.high());
        self.writer
            .borrow_mut()
            .write_all(field.as_bytes())
            .unwrap();
    }

    fn write_field(&self, label: &str, s: &dyn crate::compiler::diagnostics::Writable) {
        if self.comma_prefix.get() {
            self.writer.borrow_mut().write_all(", ".as_bytes()).unwrap();
        } else {
            self.comma_prefix.set(true);
        }

        let field = format!("\"{}\": ", label);
        self.writer
            .borrow_mut()
            .write_all(field.as_bytes())
            .unwrap();
        s.write(self);
    }

    fn write(&self, s: &dyn crate::compiler::diagnostics::Writable) {
        s.write(self)
    }

    fn write_str(&self, s: &str) {
        let js = format!("\"{}\"", s);
        self.writer.borrow_mut().write_all(js.as_bytes()).unwrap();
    }

    fn write_u64(&self, u: u64) {
        let js = format!("{}", u);
        self.writer.borrow_mut().write_all(js.as_bytes()).unwrap();
    }

    fn write_stringid(&self, s: crate::StringId) {
        let val = self.string_table.get(s).unwrap();
        self.writer.borrow_mut().write_all(val.as_bytes()).unwrap();
    }

    fn write_error(&self, e: &dyn crate::compiler::CompilerDisplay) {
        let s = e.fmt(self.source_map, self.string_table).unwrap();
        self.write_str(&s);
    }

    fn write_text(&self, s: &str) {
        let js = s.to_string();
        self.writer.borrow_mut().write_all(js.as_bytes()).unwrap();
    }

    fn start_event(&self) {
        if self.event_comma_prefix.get() {
            self.writer
                .borrow_mut()
                .write_all(",\n".as_bytes())
                .unwrap();
        } else {
            self.event_comma_prefix.set(true);
        }

        self.writer.borrow_mut().write_all("{".as_bytes()).unwrap();
    }

    fn stop_event(&self) {
        self.writer.borrow_mut().write_all("}".as_bytes()).unwrap();
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

pub fn write_source_map<W: Write>(w: W, sm: &SourceMap) {
    let json_sm: JsonSourceMap = sm.into();
    serde_json::to_writer(w, &json_sm.map).unwrap();
}

/// Represents the interface between the on-disk file for saving the SourceMap
/// and the [`SourceMap`] used by the compiler. Having this type defined here
/// allows for the decoupling of serialization and deserialization from the compiler
/// code and the UI code; meaning that serde logic can be written without ever
/// making changes to the Compiler's types.
struct JsonSourceMap {
    map: Vec<JsonSourceMapEntry>,
}

impl From<&SourceMap> for JsonSourceMap {
    fn from(sm: &SourceMap) -> Self {
        let mut map = vec![];
        for idx in 0..sm.len() {
            if let Some(entry) = sm.get(idx).map(|entry| JsonSourceMapEntry {
                source: entry.path().clone(),
                span: entry.span().into(),
            }) {
                map.push(entry)
            }
        }
        JsonSourceMap { map }
    }
}

#[derive(Serialize)]
struct JsonSourceMapEntry {
    source: PathBuf,
    span: JsonSpan,
}

#[derive(Serialize)]
struct JsonSpan(u32, u32);

impl From<Span> for JsonSpan {
    fn from(s: Span) -> Self {
        JsonSpan(s.low().as_u32(), s.high().as_u32())
    }
}
