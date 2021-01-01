pub enum TracingConfig {
    All,
    Between(usize, usize),
    Before(usize),
    After(usize),
    Only(usize),
    Off,
}