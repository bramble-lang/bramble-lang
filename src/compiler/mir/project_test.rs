#![cfg(test)]

/// Unit Tests for the Static Definition table.
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

    #[test]
    fn overwrite_func() {
        // Create a static defs table
        let mut proj = MirProject::new();

        // Add a procedure to the table
        let path: Path = vec![Element::CanonicalRoot, Element::Id(StringId::new())].into();
        let mut func = Procedure::new(&path, &Type::Unit, Span::zero());

        // Get the procedure with the given def id
        let id = proj.add_func(func.clone()).unwrap();

        // Change the function
        func.add_temp(&Type::U32, Span::zero());
        let id2 = proj.add_func(func.clone()).unwrap();

        // Check that same DefId is returned
        assert_eq!(id, id2);

        // Assert that the function got overwritten
        let actual = proj.get_def(id);
        assert_eq!(actual, &StaticItem::Function(func));
    }

    #[test]
    fn find_func() {
        let mut proj = MirProject::new();

        // Add a procedure to the table
        let path: Path = vec![Element::CanonicalRoot, Element::Id(StringId::new())].into();
        let func = Procedure::new(&path, &Type::Unit, Span::zero());

        // Get the procedure with the given def id
        let expected_id = proj.add_func(func.clone()).unwrap();

        // Search for the function
        let actual_id = proj.find_def(&path).unwrap();

        assert_eq!(actual_id, expected_id);
    }

    #[test]
    fn find_func_none() {
        let mut proj = MirProject::new();

        // Add a procedure to the table
        let path: Path = vec![Element::CanonicalRoot, Element::Id(StringId::new())].into();
        let func = Procedure::new(&path, &Type::Unit, Span::zero());

        // Get the procedure with the given def id
        proj.add_func(func.clone()).unwrap();

        // Search for the function
        let mispath: Path = vec![Element::CanonicalRoot, Element::Id(StringId::new()), Element::Id(StringId::new())].into();
        let actual_id = proj.find_def(&mispath);

        assert_eq!(actual_id, None);
    }
}
