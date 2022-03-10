#[cfg(test)]
mod tests {
    use crate::{
        compiler::{ast::*, parser::ParserContext, semantics::semanticnode::SemanticContext, Span},
        StringId,
    };

    use super::super::typetable::*;

    #[test]
    fn base_types() {
        let table = TypeTable::new();

        for ty in [
            Type::Unit,
            Type::Null,
            Type::U8,
            Type::U16,
            Type::U32,
            Type::U64,
            Type::I8,
            Type::I16,
            Type::I32,
            Type::I64,
            Type::F64,
            Type::Bool,
            Type::StringLiteral,
        ] {
            table.find(&ty).unwrap();
        }
    }

    #[test]
    fn add_array_type() {
        let mut table = TypeTable::new();
        for ty in [
            Type::Unit,
            Type::Null,
            Type::U8,
            Type::U16,
            Type::U32,
            Type::U64,
            Type::I8,
            Type::I16,
            Type::I32,
            Type::I64,
            Type::F64,
            Type::Bool,
            Type::StringLiteral,
        ] {
            let expected = MirTypeDef::Array {
                ty: table.find(&ty).unwrap(),
                sz: 4,
            };

            let arr = Type::Array(Box::new(ty), 4);
            let tid = table.add(&arr).unwrap();

            let actual = table.get(tid);

            assert_eq!(actual, &expected)
        }
    }

    #[test]
    fn add_raw_pointer_type() {
        let mut table = TypeTable::new();
        for ty in [
            Type::Unit,
            Type::Null,
            Type::U8,
            Type::U16,
            Type::U32,
            Type::U64,
            Type::I8,
            Type::I16,
            Type::I32,
            Type::I64,
            Type::F64,
            Type::Bool,
            Type::StringLiteral,
        ] {
            for mutable in [PointerMut::Const, PointerMut::Mut] {
                let expected = MirTypeDef::RawPointer {
                    mutable,
                    target: table.find(&ty).unwrap(),
                };

                let ptr = Type::RawPointer(mutable, Box::new(ty.clone()));
                let tid = table.add(&ptr).unwrap();

                let actual = table.get(tid);

                assert_eq!(actual, &expected)
            }
        }
    }

    #[test]
    fn define_struct_that_is_declared() {
        let mut table = TypeTable::new();

        let path: Path = vec![Element::CanonicalRoot, Element::Id(StringId::new())].into();
        let decl_id = table.add(&Type::Custom(path.clone())).unwrap();

        let actual = table.get(decl_id);

        let mut expected = MirTypeDef::Structure {
            path: path.clone(),
            def: MirStructDef::Declared,
        };
        assert_eq!(actual, &expected);

        // Test adding a definition to the structure
        let mut sm = SemanticContext::new_local(0, ParserContext::new(Span::zero()), Type::Unit);
        sm.set_canonical_path(path.clone());
        let field = Parameter::new(sm.clone(), StringId::new(), &Type::I64);
        let sd = StructDef::new(StringId::new(), sm, vec![field]);

        let def_id = table.add_struct_def(&sd).unwrap();

        assert_eq!(decl_id, def_id);

        let actual = table.get(def_id);
        let mir_field = Field {
            name: StringId::new(),
            ty: table.find(&Type::I64).unwrap(),
        };
        expected = MirTypeDef::Structure {
            path: path.clone(),
            def: MirStructDef::Defined(vec![mir_field]),
        };
        assert_eq!(actual, &expected);
        match actual {
            MirTypeDef::Structure { def, .. } => {
                assert_eq!(def, &MirStructDef::Defined(vec![mir_field]))
            }
            _ => panic!("Expected structure"),
        }
    }

    #[test]
    fn define_struct_that_is_not_declared() {
        let mut table = TypeTable::new();

        let path: Path = vec![Element::CanonicalRoot, Element::Id(StringId::new())].into();

        // Test adding a definition to the structure
        let mut sm = SemanticContext::new_local(0, ParserContext::new(Span::zero()), Type::Unit);
        sm.set_canonical_path(path.clone());
        let field = Parameter::new(sm.clone(), StringId::new(), &Type::I64);
        let sd = StructDef::new(StringId::new(), sm, vec![field]);

        let def_id = table.add_struct_def(&sd).unwrap();

        let actual = table.get(def_id);
        let mir_field = Field {
            name: StringId::new(),
            ty: table.find(&Type::I64).unwrap(),
        };
        let expected = MirTypeDef::Structure {
            path: path.clone(),
            def: MirStructDef::Defined(vec![mir_field]),
        };
        assert_eq!(actual, &expected);
        match actual {
            MirTypeDef::Structure { def, .. } => {
                assert_eq!(def, &MirStructDef::Defined(vec![mir_field]))
            }
            _ => panic!("Expected structure"),
        }
    }

    #[test]
    fn is_complete_when_not_complete() {
        let mut table = TypeTable::new();

        let path: Path = vec![Element::CanonicalRoot, Element::Id(StringId::new())].into();
        let decl_id = table.add(&Type::Custom(path.clone())).unwrap();

        let actual = table.get(decl_id);

        let expected = MirTypeDef::Structure {
            path: path.clone(),
            def: MirStructDef::Declared,
        };
        assert_eq!(actual, &expected);

        // is_complete should return false
        assert_eq!(table.is_complete(), false);
    }

    #[test]
    fn is_complete_when_complete() {
        let mut table = TypeTable::new();

        let path: Path = vec![Element::CanonicalRoot, Element::Id(StringId::new())].into();

        // Test adding a definition to the structure
        let mut sm = SemanticContext::new_local(0, ParserContext::new(Span::zero()), Type::Unit);
        sm.set_canonical_path(path.clone());
        let field = Parameter::new(sm.clone(), StringId::new(), &Type::I64);
        let sd = StructDef::new(StringId::new(), sm, vec![field]);

        table.add_struct_def(&sd).unwrap();

        assert_eq!(table.is_complete(), true);
    }
}
