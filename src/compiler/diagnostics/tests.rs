#[cfg(test)]
mod tests {
    use std::cell::RefCell;

    use crate::compiler::{
        diagnostics::{logger::Logger, Event, Writer},
        Span,
    };

    #[test]
    fn test_write_event() {
        let mut logger = Logger::new();
        let writer = TestWriter::new();
        logger.add_writer(&writer);

        let evt = Event {
            span: Span::zero(),
            msg: "Hello".into(),
        };

        logger.write(evt);
        assert_eq!("{[0,0], msg: \"Hello\", }", *writer.buf.borrow());
    }

    #[test]
    fn test_disable() {
        let mut logger = Logger::new();
        let writer = TestWriter::new();
        logger.add_writer(&writer);

        logger.disable();
        let evt = Event {
            span: Span::zero(),
            msg: "Hello".into(),
        };
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
        let evt = Event {
            span: Span::zero(),
            msg: "Hello".into(),
        };
        logger.write(evt);
        assert_eq!("", *writer.buf.borrow());

        // Then enable the logger and confirm that writes are now happening
        logger.enable();
        let evt = Event {
            span: Span::zero(),
            msg: "Hello".into(),
        };
        logger.write(evt);
        assert_eq!("{[0,0], msg: \"Hello\", }", *writer.buf.borrow());
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
        fn write_span(&self, span: crate::compiler::Span) {
            self.buf
                .borrow_mut()
                .push_str(&format!("[{},{}], ", span.low(), span.high()));
        }

        fn write_str(&self, label: &str, s: &str) {
            self.buf
                .borrow_mut()
                .push_str(&format!("{}: \"{}\", ", label, s));
        }

        fn start_event(&self) {
            self.buf.borrow_mut().push_str("{");
        }

        fn stop_event(&self) {
            self.buf.borrow_mut().push_str("}");
        }
    }
}
