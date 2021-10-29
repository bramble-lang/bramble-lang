use super::{Writable, Writer};

pub struct Logger<'a> {
    /// Whether this [`Logger`] will pass events it receives to the writers
    enabled: bool,

    /// A set of writer sinks that this [`Logger`] will used to write every Event
    /// that this receives.
    writers: Vec<&'a dyn Writer>,
}

impl<'a> Logger<'a> {
    /// Creates a new Logger with not writers and that is enabled.
    pub fn new() -> Logger<'a> {
        Logger {
            enabled: true,
            writers: vec![],
        }
    }

    /// Write an event to ever [`Writer`] in this [`Logger`]
    pub fn write<E: Writable>(&self, evt: E) {
        for w in &self.writers {
            evt.write(*w)
        }
    }

    /// Add a [`Writer`] to this [`Logger`]
    pub fn add_writer(&mut self, w: &'a dyn Writer) {
        self.writers.push(w);
    }

    /// This [`Logger`] will send any event received through `write` to its
    /// [`Writer`]s.
    pub fn enable(&mut self) {
        self.enabled = true;
    }

    /// This [`Logger`] will NOT send events received through `write` to its
    /// [`Writer`]s.
    pub fn disable(&mut self) {
        self.enabled = false;
    }
}
