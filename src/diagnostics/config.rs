#[derive(Debug)]
pub enum TracingConfig {
    All,
    Between(usize, usize),
    Before(usize),
    After(usize),
    Only(usize),
    Off,
}

pub trait Tracing {
    fn set_tracing(&mut self, config: TracingConfig);
}
