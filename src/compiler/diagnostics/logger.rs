use super::Writer;

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
        todo!()
    }

    /// Write an event to ever [`Writer`] in this [`Logger`]
    pub fn write(&self) {
        todo!()
    }

    /// Add a [`Writer`] to this [`Logger`]
    pub fn add_writer(&mut self, _w: &'a dyn Writer) {
        todo!()
    }

    /// This [`Logger`] will send any event received through `write` to its
    /// [`Writer`]s.
    pub fn enable(&mut self) {
        todo!()
    }

    /// This [`Logger`] will NOT send events received through `write` to its
    /// [`Writer`]s.
    pub fn disable(&mut self) {
        todo!()
    }
}
