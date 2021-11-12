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

use std::fmt::Debug;

use crate::StringId;

use super::{CompilerDisplay, CompilerError, Span};

mod logger;
mod tests;
mod view;

pub use logger::Logger;
pub use view::*;

/// Defines a way for the [`Logger`] to write events that are emitted by the
/// Compiler to the user.
pub trait Writer {
    /// Write a Span to the current event
    fn write_span(&self, span: Span);

    /// Write a field with a [`Writable`] value to the current event
    fn write_field(&self, label: &str, s: &dyn Writable);

    /// Write a string value to the current event
    fn write_str(&self, s: &str);

    /// Write a [`StringId`] value to the current event
    fn write_stringid(&self, s: StringId);

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

/// An event from any stage in the Compiler caused by the given span of source
/// code.
pub struct Event<'a, V: Writable, E: CompilerDisplay + Debug> {
    /// The stage of compilation that generated this event
    pub stage: &'static str,

    /// The [`Span`] of input source code that caused this event to occur
    pub input: Span,

    /// A description of the event
    pub msg: Result<V, &'a CompilerError<E>>,
}

impl<'a, V: Writable, E: CompilerDisplay + Debug> Writable for Event<'a, V, E> {
    fn write(&self, w: &dyn Writer) {
        w.start_event();
        w.write_field("stage", &self.stage);
        w.write_span(self.input);
        match &self.msg {
            Ok(msg) => w.write_field("ok", msg),
            Err(err) => w.write_field("error", &format!("{:?}", err)),
        }
        w.stop_event();
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
