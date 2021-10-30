//! Tools used for collecting contextual data that can be used for insight
//! diagnostics and transparency.  These tools are meant to generate data
//! that can be used by both Compiler developers and Compiler users.  For
//! developers this data will provide insight into what the compiler is doing,
//! why it is doing it, and when it is doing it.  Helping the Compiler developer
//! understand debug issues or implement new features.
//!
//! For the compiler user, this data will help them understand what their
//! code is being transformed into and why that is happening. The goal is to
//! provide deep but friendly insight into how the code they write becomes what
//! the CPU executes.

use super::Span;

mod logger;
mod tests;

/// Defines a way for the [`Logger`] to write events that are emitted by the
/// Compiler to the user.
pub trait Writer {
    fn write_span(&mut self, span: Span);
    fn write_str(&mut self, label: &str, s: &str);
    fn start_event(&mut self);
    fn stop_event(&mut self);
}

/// Define how a type will be written to an Event log by a [`Writer`].
pub trait Writable {
    /// Uses the given [`Writer`] to write the data in an instance of this type
    /// to an output target.
    fn write(&self, w: &mut dyn Writer);
}

pub struct Event {
    span: Span,
    msg: String,
}

impl Writable for Event {
    fn write(&self, w: &mut dyn Writer) {
        w.start_event();
        w.write_span(self.span);
        w.write_str("msg", &self.msg);
        w.stop_event();
    }
}
