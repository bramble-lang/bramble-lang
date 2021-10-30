#[cfg(test)]
mod tests {
    use crate::compiler::{
        diagnostics::{logger::Logger, Event, Writer},
        Span,
    };

    #[test]
    fn test_write_event() {
        let mut logger = Logger::new();
        let mut writer = TestWriter::new();
        logger.add_writer(&mut writer);

        let evt = Event {
            span: Span::zero(),
            msg: "Hello".into(),
        };

        logger.write(evt);
        assert_eq!("{[0,0], msg: \"Hello\", }", writer.buf);
    }

    /// Writer to be used for unit testing
    struct TestWriter {
        buf: String,
    }

    impl TestWriter {
        pub fn new() -> TestWriter {
            TestWriter { buf: String::new() }
        }
    }

    impl Writer for TestWriter {
        fn write_span(&mut self, span: crate::compiler::Span) {
            self.buf
                .push_str(&format!("[{},{}], ", span.low(), span.high()));
        }

        fn write_str(&mut self, label: &str, s: &str) {
            self.buf.push_str(&format!("{}: \"{}\", ", label, s));
        }

        fn start_event(&mut self) {
            self.buf.push_str("{");
        }

        fn stop_event(&mut self) {
            self.buf.push_str("}");
        }
    }
}
