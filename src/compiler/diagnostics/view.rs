pub trait View<V> {
    fn view<F: Fn(&V)>(self, f: F) -> Self;
}

pub trait ViewErr<E> {
    fn view_err<F: Fn(&E)>(self, f: F) -> Self;
}

impl<V, E> View<V> for Result<Option<V>, E> {
    fn view<F: Fn(&V)>(self, f: F) -> Self {
        match &self {
            Ok(Some(v)) => f(v),
            _ => (),
        }

        self
    }
}

impl<V, E> ViewErr<E> for Result<V, E> {
    fn view_err<F: Fn(&E)>(self, f: F) -> Self {
        match &self {
            Err(e) => f(e),
            _ => (),
        }

        self
    }
}

/*impl<V, E> View<V> for Result<V, E> {
    fn view<F: Fn(&V)>(self, f: F) -> Self {
        match &self {
            Ok(v) => f(v),
            _ => (),
        }

        self
    }
}*/

impl<V> View<V> for Option<V> {
    fn view<F: Fn(&V)>(self, f: F) -> Self {
        match &self {
            Some(v) => f(v),
            _ => (),
        }

        self
    }
}
