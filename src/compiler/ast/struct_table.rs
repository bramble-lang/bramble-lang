use std::{collections::HashMap, marker::PhantomData};

use crate::{
    semantics::semanticnode::SemanticNode,
    syntax::ty::Type,
    syntax::path::Path,
};

use super::struct_definition::{FieldInfo, StructDefinition};

#[derive(Debug)]
pub struct StructTable<S> {
    table: HashMap<String, StructDefinition>,
    state: PhantomData<S>,
}

impl<S> std::fmt::Display for StructTable<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (canon_path, st) in self.table.iter() {
            f.write_fmt(format_args!(
                "\t{} => {} | {:?} | ",
                canon_path, st.name, st.size
            ))?;

            for FieldInfo{name, ty, offset} in st.fields.iter() {
                f.write_fmt(format_args!(
                    "[{}, {}, {:?}], ",
                    name,
                    ty,
                    offset
                ))?;
            }

            f.write_str("\n")?;
        }
        Ok(())
    }
}

pub struct Unrealized {}
pub struct Resolved {}

pub type UnresolvedStructTable = StructTable<Unrealized>;
pub type ResolvedStructTable = StructTable<Resolved>;

impl UnresolvedStructTable {
    pub fn new() -> UnresolvedStructTable {
        UnresolvedStructTable {
            table: HashMap::new(),
            state: PhantomData,
        }
    }

    pub fn from(root: &SemanticNode) -> Result<UnresolvedStructTable, String> {
        let mut table = UnresolvedStructTable {
            table: HashMap::new(),
            state: PhantomData,
        };

        Self::traverse(root, &mut table.table)?;
        Ok(table)
    }

    fn traverse(
        node: &SemanticNode,
        table: &mut HashMap<String, StructDefinition>,
    ) -> Result<(), String> {
        match node {
            SemanticNode::Module {
                structs, modules, ..
            } => {
                for s in structs.iter() {
                    if let SemanticNode::StructDef(meta, name, fields) = s {
                        let struct_def = StructDefinition::new(name, fields.clone());
                        Self::insert_struct(table, meta.get_canonical_path(), struct_def)?;
                    } else {
                        return Err(format!(
                            "Found {} in the structs section of a module",
                            s.root_str()
                        ));
                    }
                }

                for m in modules.iter() {
                    Self::traverse(m, table)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    pub fn resolve(&self) -> Result<ResolvedStructTable, String> {
        let mut resolved = ResolvedStructTable {
            table: HashMap::new(),
            state: PhantomData,
        };
        // for each struct in the table
        loop {
            let mut num_resolved = 0;

            for (name, def) in self.table.iter() {
                // If no more structs can be resolved then break
                // If the struct is resolved, then skip
                if resolved.table.contains_key(name) {
                    continue;
                }

                // for each field in the struct
                // check if field has a size
                if let Some(sz) = self.attempt_size_resolution(def, &resolved.table) {
                    let resolved_struct_def = def.set_field_sizes(&sz)?;
                    resolved.table.insert(name.clone(), resolved_struct_def);
                    num_resolved += 1;
                }
            }

            if num_resolved == 0 {
                break;
            }
        }

        self.validate_resolution_attempt(&resolved)
            .map(|()| resolved)
    }

    fn validate_resolution_attempt(&self, resolved: &ResolvedStructTable) -> Result<(), String> {
        let mut unresolved_structs = self.get_unresolved_structs(resolved);
        if unresolved_structs.len() == 0 {
            Ok(())
        } else {
            unresolved_structs.sort();
            Err(format!(
                "Could not resolve these structs: {}",
                unresolved_structs.join(", ")
            ))
        }
    }

    fn attempt_size_resolution(
        &self,
        st: &StructDefinition,
        resolved_structs: &HashMap<String, StructDefinition>,
    ) -> Option<Vec<i32>> {
        fn get_resolved_size(
            ty: &Type,
            resolved_structs: &HashMap<String, StructDefinition>,
        ) -> Option<i32> {
            match ty {
                Type::I32 => Some(4),
                Type::Bool => Some(4),
                Type::Custom(name) => resolved_structs.get(&name.to_string())?.size,
                Type::Coroutine(_) => Some(4),
                Type::StringLiteral => Some(4),
                Type::Unit => None,
                Type::StructDef(_) => None,
                Type::FunctionDef(_, _) => None,
                Type::CoroutineDef(_, _) => None,
                Type::Unknown => None,
            }
        }

        // Loop through each struct in the table and attempt to resolve its size
        st.fields
            .iter()
            .map(|FieldInfo{ty, ..}| get_resolved_size(ty, resolved_structs))
            .collect::<Option<Vec<i32>>>()
    }

    fn get_unresolved_structs(&self, resolved_structs: &ResolvedStructTable) -> Vec<String> {
        let mut unresolved_structs = vec![];
        for st in self.table.keys() {
            if !resolved_structs.table.contains_key(st) {
                unresolved_structs.push(st.clone());
            }
        }

        unresolved_structs
    }

    fn insert_struct(
        table: &mut HashMap<String, StructDefinition>,
        canon_path: &Path,
        def: StructDefinition,
    ) -> Result<(), String> {
        if canon_path.len() == 0 {
            return Err("Cannot insert empty path into StructTable".into());
        }

        let canon_name = canon_path.to_string();
        let already_exists = table.insert(canon_name.clone(), def).is_some();
        if already_exists {
            Err(format!("Struct {} already exists", canon_name))
        } else {
            Ok(())
        }
    }
}

impl ResolvedStructTable {
    pub fn get(&self, canon_path: &Path) -> Option<&StructDefinition> {
        self.table.get(&canon_path.to_string())
    }

    pub fn size_of(&self, ty: &Type) -> Option<i32> {
        match ty {
            Type::I32 => Some(4),
            Type::Bool => Some(4),
            Type::StringLiteral => Some(4),
            Type::Coroutine(_) => Some(4),
            Type::Custom(canon_path) => self.get(canon_path).map(|st| st.size).flatten(),
            Type::FunctionDef(..) => Some(0),
            Type::CoroutineDef(..) => Some(0),
            Type::StructDef(..) => Some(0),
            Type::Unit => Some(0),
            Type::Unknown => panic!("Requesting size for a type of Unknown"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        diagnostics::config::TracingConfig,
        lexer::{lexer::Lexer, tokens::Token},
        semantics::type_checker::type_check,
    };

    #[test]
    pub fn test_adding_a_struct_that_references_a_struct() {
        use crate::syntax::parser;
        for text in vec![
            "
                struct test{i: i32}
                struct test2{t: test}
                ",
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_, _>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let semantic_ast = type_check(&ast, TracingConfig::Off, TracingConfig::Off).unwrap();
            let unrealized_st = UnresolvedStructTable::from(&semantic_ast).unwrap();
            assert_eq!(unrealized_st.table.len(), 2);
            assert_eq!(
                unrealized_st.table["root::test"],
                StructDefinition::new("test", vec![("i".into(), Type::I32)])
            );
            assert_eq!(
                unrealized_st.table["root::test2"],
                StructDefinition::new(
                    "test2",
                    vec![("t".into(), Type::Custom(vec!["root", "test"].into()))]
                )
            )
        }
    }

    #[test]
    pub fn test_nested_in_module() {
        use crate::syntax::parser;
        for text in vec![
            "
            mod my_mod {
                struct test{i: i32}
            }

            struct test2{t: my_mod::test}
            ",
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_, _>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let semantic_ast = type_check(&ast, TracingConfig::Off, TracingConfig::Off).unwrap();
            let unrealized_st = UnresolvedStructTable::from(&semantic_ast).unwrap();
            assert_eq!(unrealized_st.table.len(), 2);
            assert_eq!(
                unrealized_st.table["root::my_mod::test"],
                StructDefinition::new("test", vec![("i".into(), Type::I32)])
            );
            assert_eq!(
                unrealized_st.table["root::test2"],
                StructDefinition::new(
                    "test2",
                    vec![(
                        "t".into(),
                        Type::Custom(vec!["root", "my_mod", "test"].into())
                    )]
                )
            )
        }
    }

    #[test]
    pub fn test_same_names_different_modules() {
        use crate::syntax::parser;
        for text in vec![
            "
            mod my_mod {
                struct test{i: i32}
            }

            struct test{t: my_mod::test}
            ",
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_, _>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let semantic_ast = type_check(&ast, TracingConfig::Off, TracingConfig::Off).unwrap();
            let unrealized_st = UnresolvedStructTable::from(&semantic_ast).unwrap();
            assert_eq!(unrealized_st.table.len(), 2);
            assert_eq!(
                unrealized_st.table["root::my_mod::test"],
                StructDefinition::new("test", vec![("i".into(), Type::I32)])
            );
            assert_eq!(
                unrealized_st.table["root::test"],
                StructDefinition::new(
                    "test",
                    vec![(
                        "t".into(),
                        Type::Custom(vec!["root", "my_mod", "test"].into())
                    )]
                )
            )
        }
    }

    #[test]
    pub fn test_duplicate() {
        let mut st = UnresolvedStructTable {
            table: HashMap::new(),
            state: PhantomData,
        };
        let result = UnresolvedStructTable::insert_struct(
            &mut st.table,
            &vec!["root", "test"].into(),
            StructDefinition::new("test", vec![("i".into(), Type::I32)]),
        );
        assert_eq!(result, Ok(()));
        let result = UnresolvedStructTable::insert_struct(
            &mut st.table,
            &vec!["root", "test"].into(),
            StructDefinition::new("test", vec![("i".into(), Type::I32)]),
        );
        assert_eq!(result, Err("Struct root::test already exists".into()));
    }

    /*
    Tests to add:
    1. recursive and cyclical structs and test that they fail
     */

    #[test]
    pub fn test_resolving_a_flat_struct() {
        use crate::syntax::parser;
        for (text, canonical_name, expected) in vec![
            (
                "struct test{i: i32}",
                "root::test",
                StructDefinition {
                    name: "test".into(),
                    size: Some(4),
                    fields: vec![FieldInfo{name: "i".into(), ty: Type::I32, offset: Some(4)}],
                },
            ),
            (
                "struct test{y: bool}",
                "root::test",
                StructDefinition {
                    name: "test".into(),
                    size: Some(4),
                    fields: vec![FieldInfo{name: "y".into(), ty: Type::Bool, offset: Some(4)}],
                },
            ),
            (
                "struct test{i: i32, y: bool}",
                "root::test",
                StructDefinition {
                    name: "test".into(),
                    size: Some(8),
                    fields: vec![
                        FieldInfo{name: "i".into(), ty: Type::I32, offset: Some(4)},
                        FieldInfo{name: "y".into(), ty: Type::Bool, offset: Some(8)},
                    ],
                },
            ),
            (
                "struct test{s: string}",
                "root::test",
                StructDefinition {
                    name: "test".into(),
                    size: Some(4),
                    fields: vec![FieldInfo{name: "s".into(), ty: Type::StringLiteral, offset: Some(4)}],
                },
            ),
            (
                "mod my_mod{struct inner_test{i: i32, y: bool}}",
                "root::my_mod::inner_test",
                StructDefinition {
                    name: "inner_test".into(),
                    size: Some(8),
                    fields: vec![
                        FieldInfo{name: "i".into(), ty: Type::I32, offset: Some(4)},
                        FieldInfo{name: "y".into(), ty: Type::Bool, offset: Some(8)},
                    ],
                },
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_, _>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let semantic_ast = type_check(&ast, TracingConfig::Off, TracingConfig::Off).unwrap();
            let unrealized_st = UnresolvedStructTable::from(&semantic_ast).unwrap();
            let resolved_st = unrealized_st.resolve().unwrap();

            assert_eq!(resolved_st.table[canonical_name], expected);
        }
    }

    #[test]
    pub fn test_nested_struct() {
        use crate::syntax::parser;
        for (text, canonical_name, expected) in vec![
            (
                "
            struct test{i: i32}
            struct test2{t: test}
            ",
                "root::test2",
                StructDefinition {
                    name: "test2".into(),
                    size: Some(4),
                    fields: vec![FieldInfo{
                        name: "t".into(),
                        ty: Type::Custom(vec!["root", "test"].into()),
                        offset: Some(4),
                    }],
                },
            ),
            (
                "
            struct test{i: i32}
            struct test2{x: i32, t: test, b: bool}
            ",
                "root::test2",
                StructDefinition {
                    name: "test2".into(),
                    size: Some(12),
                    fields: vec![
                        FieldInfo{name: "x".into(), ty: Type::I32, offset: Some(4)},
                        FieldInfo{
                            name: "t".into(),
                            ty: Type::Custom(vec!["root", "test"].into()),
                            offset: Some(8),
                        },
                        FieldInfo{name: "b".into(), ty: Type::Bool, offset: Some(12)},
                    ],
                },
            ),
            (
                "
            struct test{i: i32, j: i32}
            struct test2{x: i32, b: bool, t: test}
            ",
                "root::test2",
                StructDefinition {
                    name: "test2".into(),
                    size: Some(16),
                    fields: vec![
                        FieldInfo{name: "x".into(), ty: Type::I32, offset: Some(4)},
                        FieldInfo{name: "b".into(), ty: Type::Bool, offset: Some(8)},
                        FieldInfo{
                            name: "t".into(),
                            ty: Type::Custom(vec!["root", "test"].into()),
                            offset: Some(16),
                        },
                    ],
                },
            ),
            (
                "
            struct test{i: i32}
            struct test2{t: test}
            struct test3{t2: test2}
            ",
                "root::test3",
                StructDefinition {
                    name: "test3".into(),
                    size: Some(4),
                    fields: vec![FieldInfo{
                        name: "t2".into(), 
                        ty: Type::Custom(vec!["root", "test2"].into()),
                        offset: Some(4),
                    }],
                },
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_, _>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let semantic_ast = type_check(&ast, TracingConfig::Off, TracingConfig::Off).unwrap();
            let unrealized_st = UnresolvedStructTable::from(&semantic_ast).unwrap();
            let resolved_st = unrealized_st.resolve().unwrap();

            assert_eq!(resolved_st.table[canonical_name], expected);
        }
    }

    #[test]
    pub fn test_cyclical_fails() {
        use crate::syntax::parser;
        for text in vec![
            "
                struct test{t2: test2}
                struct test2{t: test}
                ",
            "
                struct test{t2: test2}
                struct test2{t: test}
                struct good{i: i32, b: bool}
                ",
            "
                struct test{t2: test2}
                struct test2{t: test}
                struct good{i: i32, b: bool}
                struct good2{g: good}
                ",
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_, _>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let semantic_ast = type_check(&ast, TracingConfig::Off, TracingConfig::Off).unwrap();
            let unrealized_st = UnresolvedStructTable::from(&semantic_ast).unwrap();
            let resolved = unrealized_st.resolve();

            assert_eq!(
                resolved.err(),
                Some("Could not resolve these structs: root::test, root::test2".into())
            );
        }
    }
}
