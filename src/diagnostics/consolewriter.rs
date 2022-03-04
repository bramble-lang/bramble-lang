use crate::{
    compiler::{
        ast::*,
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

    fn write_error(&self, e: &dyn crate::compiler::CompilerDisplay) {
        let s = e.fmt(self.source_map, self.string_table).unwrap();
        print!("{}", s);
    }

    fn write_text(&self, s: &str) {
        print!("{}", s);
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

    fn write_u64(&self, u: u64) {
        print!("{}", u);
    }

    fn write_i64(&self, i: i64) {
        print!("{}", i);
    }

    fn write_bool(&self, b: bool) {
        print!("{}", b);
    }
}
