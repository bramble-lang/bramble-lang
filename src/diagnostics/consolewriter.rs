use crate::{
    compiler::{
        diagnostics::{Writable, Writer},
        SourceMap,
    },
    StringTable,
};

/// Writes Compiler events to the console. This Writer will use the [`SourceMap`]
/// to convert Span information to human readable source code references.
pub struct ConsoleWriter<'a> {
    source_map: &'a SourceMap,
    string_table: &'a StringTable,
}

impl<'a> ConsoleWriter<'a> {
    pub fn new(source_map: &'a SourceMap, string_table: &'a StringTable) -> ConsoleWriter<'a> {
        ConsoleWriter {
            source_map,
            string_table,
        }
    }
}

impl<'a> Writer for ConsoleWriter<'a> {
    fn write_span(&self, field: &str, span: crate::compiler::Span) {
        print!("{}: ", field);

        // Get source code that the span covers
        let src = self.source_map.text_in_span(span).unwrap();

        // Compress blocks of whitespace to a single space
        let src = src.split_ascii_whitespace().collect::<Vec<_>>().join(" ");

        let width = 20;
        if src.len() < width {
            print!("[{}], ", src);
        } else {
            print!(
                "[{}...{}], ",
                &src[0..width / 2],
                &src[src.len() - width / 2..]
            );
        };
    }

    fn write_field(&self, label: &str, s: &dyn Writable) {
        print!("{}: ", label);
        s.write(self);
        print!(", ");
    }

    fn write(&self, s: &dyn Writable) {
        s.write(self);
    }

    fn start_event(&self) {
        print!("{{");
    }

    fn stop_event(&self) {
        print!("}}\n");
    }

    fn write_str(&self, s: &str) {
        print!("{}", s);
    }

    fn write_stringid(&self, s: crate::StringId) {
        print!("{}", self.string_table.get(s).unwrap());
    }

    fn write_text(&self, s: &str) {
        print!("{}", s);
    }
}
