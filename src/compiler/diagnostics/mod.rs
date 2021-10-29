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

mod logger;

/// Defines a way for the [`Logger`] to write events that are emitted by the
/// Compiler to the user.
pub trait Writer {}

/// Define how a type will be written to an Event log by a [`Writer`].
pub trait Writable {
    /// Uses the given [`Writer`] to write the data in an instance of this type
    /// to an output target.
    fn write(w: &dyn Writer);
}
