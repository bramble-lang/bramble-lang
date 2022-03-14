#![cfg(test)]

mod static_defs {
    use crate::{
        compiler::{
            ast::{Element, Path, Type},
            mir::{ir::Procedure, project::{MirProject, StaticItem}},
            Span,
        },
        StringId,
    };

    #[test]
    fn add_func() {
        // Create a static defs table
        let mut proj = MirProject::new();

        // Add a procedure to the table
        let path: Path = vec![Element::CanonicalRoot, Element::Id(StringId::new())].into();
        let func = Procedure::new(&path, &Type::Unit, Span::zero());

        // Get the procedure with the given def id
        let id = proj.add_func(func.clone()).unwrap();

        // Assert that it's the same
        let actual = proj.get_def(id);
        assert_eq!(actual, &StaticItem::Function(func));
    }

    fn find_func() {
        // Add a func to the static def table
        // Find with canonical path
    }
}
