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
