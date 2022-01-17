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
use crate::StringId;

use super::{ast::Path, CompilerDisplay, CompilerError, Span};

mod event;
mod logger;
mod tests;
mod view;

pub use event::event_id::{EventId, EventStack};
pub use event::Event;
pub use logger::Logger;
pub use view::*;

/// Defines a way for the [`Logger`] to write events that are emitted by the
/// Compiler to the user.
pub trait Writer {
    /// Write a Span to the current event
    fn write_span(&self, field: &str, span: Span);

    /// Write a field with a [`Writable`] value to the current event
    fn write_field(&self, label: &str, s: &dyn Writable);

    /// Write a [`Writable`] value to the current event
    fn write(&self, s: &dyn Writable);

    /// Write a string value to the current event
    fn write_str(&self, s: &str);

    /// Write a [`StringId`] value to the current event
    fn write_stringid(&self, s: StringId);

    /// Write a [`u64`] value to the current event
    fn write_u64(&self, u: u64);

    /// Write a [`Path`] value to the current event
    fn write_path(&self, p: &Path);

    /// Write text to the current event
    fn write_text(&self, s: &str);

    /// Writes an error message
    fn write_error(&self, e: &dyn CompilerDisplay);

    /// Start writing a new compiler event.  This should emit any tokens which
    /// signal the start of an event.
    fn start_event(&self);

    /// Stop writing a the current compiler event.  This should emit any tokens
    /// which are needed to signal the end of an event.
    fn stop_event(&self);
}

/// Define how a type will be written to an Event log by a [`Writer`].
pub trait Writable {
    /// Uses the given [`Writer`] to write the data in an instance of this type
    /// to an output target.
    fn write(&self, w: &dyn Writer);
}

impl<E: CompilerDisplay> Writable for &CompilerError<E> {
    fn write(&self, w: &dyn Writer) {
        w.write_error(self.to_compilerdisplay());
    }
}

impl Writable for &str {
    fn write(&self, w: &dyn Writer) {
        w.write_str(&self)
    }
}

impl Writable for String {
    fn write(&self, w: &dyn Writer) {
        w.write_str(&self)
    }
}

impl Writable for &String {
    fn write(&self, w: &dyn Writer) {
        w.write_str(&self)
    }
}
