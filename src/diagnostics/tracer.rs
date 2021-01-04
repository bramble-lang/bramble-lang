#[macro_export]
macro_rules! trace {
    ($ts:expr) => {
        let print_trace = match &$ts.tracing {
            &TracingConfig::Only(ln) => ($ts.line() as usize) == ln,
            &TracingConfig::Before(ln) => ($ts.line() as usize) <= ln,
            &TracingConfig::After(ln) => ($ts.line() as usize) >= ln,
            &TracingConfig::Between(start, end) => {
                ($ts.line() as usize) >= start && ($ts.line() as usize) <= end
            }
            &TracingConfig::All => true,
            &TracingConfig::Off => false,
        };
        if print_trace {
            println!(
                "{} <- L{}:{:?}",
                function_name!(),
                $ts.line(),
                $ts.current_token()
            )
        }
    };
}
