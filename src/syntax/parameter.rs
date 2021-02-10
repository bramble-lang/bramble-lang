use super::ty::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct Parameter<M> {
    pub annotation: M,
    pub name: String,
    pub ty: Type,
}

impl<M> Parameter<M> {
    pub fn new(a: M, name: &str, ty: &Type) -> Parameter<M> {
        Parameter {
            annotation: a,
            name: name.into(),
            ty: ty.clone(),
        }
    }

    pub fn get_annotations(&self) -> &M {
        &self.annotation
    }

    pub fn get_annotations_mut(&mut self) -> &mut M {
        &mut self.annotation
    }

    pub fn root_str(&self) -> String {
        format!("{}:{}", self.name, self.ty)
    }

    pub fn map_annotation<F, N>(&self, mut f: F) -> Parameter<N>
    where
        F: FnMut(&M) -> N,
    {
        Parameter {
            annotation: f(&self.annotation),
            name: self.name.clone(),
            ty: self.ty.clone(),
        }
    }
}

impl<M> std::fmt::Display for Parameter<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.write_str(&self.root_str())
    }
}
