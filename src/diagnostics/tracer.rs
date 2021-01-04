#[macro_export]
macro_rules! trace {
    ($ts:expr) => {
        let print_trace = match &$ts.tracing {
            &TracingConfig::Only(ln) if ($ts.line() as usize) == ln => true,
            &TracingConfig::Before(ln) if ($ts.line() as usize) <= ln => true,
            &TracingConfig::After(ln) if ($ts.line() as usize) >= ln => true,
            &TracingConfig::Between(start, end)
                if ($ts.line() as usize) >= start && ($ts.line() as usize) <= end =>
            {
                true
            }
            &TracingConfig::All => true,
            _ => false,
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
