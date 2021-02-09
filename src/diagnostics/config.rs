#[derive(Debug)]
pub enum TracingConfig {
    All,
    Between(usize, usize),
    Before(usize),
    After(usize),
    Only(usize),
    Off,
}

impl TracingConfig {
    pub fn parse(v: Option<&str>) -> TracingConfig {
        match v.map(|v| v.to_lowercase()) {
            Some(s) if s == "all" => TracingConfig::All,
            Some(v) => {
                let split: Vec<_> = v.split(':').collect();
                if split.len() == 1 {
                    let line = split[0]
                        .parse::<usize>()
                        .expect("Expected integer in Trace Configuration");
                    TracingConfig::Only(line)
                } else if split.len() == 2 {
                    if split[0].len() == 0 && split[1].len() == 0 {
                        TracingConfig::All
                    } else if split[0].len() == 0 {
                        let before = split[1]
                            .parse::<usize>()
                            .expect("Expected integer in Trace Configuration");
                        TracingConfig::Before(before)
                    } else if split[1].len() == 0 {
                        let after = split[0]
                            .parse::<usize>()
                            .expect("Expected integer in Trace Configuration");
                        TracingConfig::After(after)
                    } else {
                        let start = split[0]
                            .parse::<usize>()
                            .expect("Expected integer in Trace Configuration");
                        let end = split[1]
                            .parse::<usize>()
                            .expect("Expected integer in Trace Configuration");
                        TracingConfig::Between(start, end)
                    }
                } else {
                    panic!("Invalid configuration value provided for tracing");
                }
            }
            None => TracingConfig::Off,
        }
    }
}

pub trait Tracing {
    fn set_tracing(&mut self, config: TracingConfig);
}
