#[cfg(test)]
mod tests {
    use std::cell::RefCell;

    use crate::compiler::{
        diagnostics::{logger::Logger, Event, Writer},
        lexer::LexerError,
        Span,
    };

    #[test]
    fn test_write_event() {
        let mut logger = Logger::new();
        let writer = TestWriter::new();
        logger.add_writer(&writer);

        logger.enable();

        let evt = Event::<_, LexerError>::new("test", Span::zero(), Ok("Hello"));

        logger.write(evt);
        assert_eq!(
            "{id: 1, stage: \"test\", source: [0,0], ok: \"Hello\", }",
            *writer.buf.borrow()
        );
    }

    #[test]
    fn test_disable() {
        let mut logger = Logger::new();
        let writer = TestWriter::new();
        logger.add_writer(&writer);

        logger.disable();
        let evt = Event::<_, LexerError>::new("test", Span::zero(), Ok("Hello"));
        logger.write(evt);
        assert_eq!("", *writer.buf.borrow());
    }

    #[test]
    fn test_enable() {
        let mut logger = Logger::new();
        let writer = TestWriter::new();
        logger.add_writer(&writer);

        // First disable the logger and test that writes are blocked
        logger.disable();
        let evt = Event::<_, LexerError>::new("test", Span::zero(), Ok("Hello"));
        logger.write(evt);
        assert_eq!("", *writer.buf.borrow());

        // Then enable the logger and confirm that writes are now happening
        logger.enable();
        let evt = Event::<_, LexerError>::new("test", Span::zero(), Ok("Hello"));
        logger.write(evt);
        assert_eq!(
            "{id: 1, stage: \"test\", source: [0,0], ok: \"Hello\", }",
            *writer.buf.borrow()
        );
    }

    /// Writer to be used for unit testing
    struct TestWriter {
        buf: RefCell<String>,
    }

    impl TestWriter {
        pub fn new() -> TestWriter {
            TestWriter {
                buf: RefCell::new(String::new()),
            }
        }
    }

    impl Writer for TestWriter {
        fn write_span(&self, field: &str, span: crate::compiler::Span) {
            self.buf.borrow_mut().push_str(&format!(
                "{}: [{},{}], ",
                field,
                span.low(),
                span.high()
            ));
        }

        fn start_event(&self) {
            self.buf.borrow_mut().push_str("{");
        }

        fn stop_event(&self) {
            self.buf.borrow_mut().push_str("}");
        }

        fn write_str(&self, s: &str) {
            self.buf.borrow_mut().push_str(&format!("\"{}\"", s));
        }

        fn write_field(&self, label: &str, s: &dyn crate::compiler::diagnostics::Writable) {
            self.buf.borrow_mut().push_str(&format!("{}: ", label));
            s.write(self);
            self.buf.borrow_mut().push_str(&format!(", "));
        }

        fn write_stringid(&self, _s: crate::StringId) {
            todo!()
        }

        fn write(&self, s: &dyn crate::compiler::diagnostics::Writable) {
            s.write(self);
        }

        fn write_text(&self, _: &str) {
            todo!()
        }

        fn write_path(&self, _: &crate::compiler::ast::Path) {
            todo!()
        }

        fn write_u64(&self, _: u64) {
            self.buf.borrow_mut().push_str(&format!("{}", 1));
        }

        fn write_error(&self, _e: &dyn crate::compiler::CompilerDisplay) {
            todo!()
        }
    }
}
