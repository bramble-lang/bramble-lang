#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RoutineDef {
    Function,
    Coroutine,
}

impl std::fmt::Display for RoutineDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        use RoutineDef::*;
        match self {
            Coroutine => f.write_str("coroutine def"),
            Function => f.write_str("function def"),
        }
    }
}