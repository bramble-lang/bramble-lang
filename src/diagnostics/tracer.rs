#[macro_export]
macro_rules! trace {
    ($ts:expr) => {
        match &$ts.tracing {
            &TracingConfig::Only(ln) if ($ts.line() as usize) == ln => println!(
                "{} <- L{}:{:?}",
                function_name!(),
                $ts.line(),
                $ts.current_token()
            ),
            &TracingConfig::Before(ln) if ($ts.line() as usize) <= ln => println!(
                "{} <- L{}:{:?}",
                function_name!(),
                $ts.line(),
                $ts.current_token()
            ),
            &TracingConfig::After(ln) if ($ts.line() as usize) >= ln => println!(
                "{} <- L{}:{:?}",
                function_name!(),
                $ts.line(),
                $ts.current_token()
            ),
            &TracingConfig::Between(start, end)
                if ($ts.line() as usize) >= start && ($ts.line() as usize) <= end =>
            {
                println!(
                    "{} <- L{}:{:?}",
                    function_name!(),
                    $ts.line(),
                    $ts.current_token()
                )
            }
            &TracingConfig::All => println!(
                "{} <- L{}:{:?}",
                function_name!(),
                $ts.line(),
                $ts.current_token()
            ),
            _ => {}
        }
    };
}
