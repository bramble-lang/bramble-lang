use std::{collections::HashMap, marker::PhantomData};

use crate::{semantics::semanticnode::SemanticNode, syntax::ast::{self, Path}};

use super::struct_table::StructDefinition;

#[derive(Debug)]
pub struct StructTable<S> {
    table: HashMap<String, StructDefinition>,
    state: PhantomData<S>,
}

impl<S> std::fmt::Display for StructTable<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (_, st) in self.table.iter() {
            f.write_fmt(format_args!("\t{} | {:?} | ", st.name, st.size))?;

            for (field_name, field_type, field_size) in st.fields.iter() {
                f.write_fmt(format_args!(
                    "[{}, {}, {}], ",
                    field_name,
                    field_type,
                    field_size.unwrap_or(0)
                ))?;
            }

            f.write_str("\n")?;
        }
        Ok(())
    }
}

pub struct Unrealized{}
pub struct Resolved{}

pub type UnresolvedStructTable = StructTable<Unrealized>;
pub type ResolvedStructTable = StructTable<Resolved>;

impl UnresolvedStructTable {
    pub fn new() -> UnresolvedStructTable {
        UnresolvedStructTable{
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

    fn traverse(node: &SemanticNode, table: &mut HashMap<String, StructDefinition>) -> Result<(), String> {
        match node {
            SemanticNode::Module {
                structs, modules, ..
            } => {
                for s in structs.iter() {
                    if let SemanticNode::StructDef(meta, name, fields) = s {
                        let struct_def = StructDefinition::new(name, fields.clone());
                        Self::insert_struct(table, &meta.path, struct_def)?;
                    } else {
                        return Err(format!("Found {} in the structs section of a module", s.root_str()));
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
        let mut resolved = ResolvedStructTable { table: HashMap::new(), state: PhantomData};
        // for each struct in the table
        loop {
            let mut num_resolved = 0;

            for (key, value) in self.table.iter() {
                // If no more structs can be resolved then break
                // If the struct is resolved, then skip
                if resolved.table.contains_key(key) {
                    continue;
                }

                // for each field in the struct
                // check if field has a size
                if let Some(sz) = self.attempt_size_resolution(value, &resolved.table) {
                    let mut sd = StructDefinition{
                        name: value.name.clone(),
                        size: None,
                        fields: value.fields.clone(),
                    };
                    let mut total_sz = 0;
                    for idx in 0..sz.len() {
                        let field_sz = sz[idx];
                        total_sz += field_sz;
                        sd.fields[idx].2 = Some(total_sz);
                    }
                    sd.size = Some(total_sz);
                    // if all fields have sizes, compute the total size
                    // then insert into resolved table
                    resolved.table.insert(key.clone(), sd);
                    num_resolved += 1;
                }
            }

            if num_resolved == 0 {
                break;
            }
        }

        let mut unresolved_structs = self.get_unresolved_structs(&resolved);
        if unresolved_structs.len() == 0 {
            Ok(resolved)
        } else {
            unresolved_structs.sort();
            Err(format!("Could not resolve these structs: {}", unresolved_structs.join(", ")))
        }
    }

    fn attempt_size_resolution(
        &self,
        st: &StructDefinition,
        resolved_structs: &HashMap<String, StructDefinition>,
    ) -> Option<Vec<i32>> {
        fn get_resolved_size(
            ty: &ast::Type,
            resolved_structs: &HashMap<String, StructDefinition>,
        ) -> Option<i32> {
            match ty {
                ast::Type::I32 => Some(4),
                ast::Type::Bool => Some(4),
                ast::Type::Custom(name) => {
                    resolved_structs.get(&name.to_string())?.size
                }
                ast::Type::Coroutine(_) => Some(4),
                ast::Type::StringLiteral => Some(4),
                ast::Type::Unit => None,
                ast::Type::StructDef(_) => None,
                ast::Type::FunctionDef(_, _) => None,
                ast::Type::CoroutineDef(_, _) => None,
                ast::Type::Unknown => None,
            }
        }

        // Loop through each struct in the table and attempt to resolve its size
        st.fields
            .iter()
            .map(|(_, ty, _)| get_resolved_size(ty, resolved_structs))
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
            return Err("Cannot insert empty path into StructTable".into())
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
    pub fn get(&self, name: &str) -> Option<&StructDefinition> {
        self.table.get(name)
    }

    pub fn size_of(&self, ty: &ast::Type) -> Option<i32> {
        match ty {
            ast::Type::I32 => Some(4),
            ast::Type::Bool => Some(4),
            ast::Type::StringLiteral => Some(4),
            ast::Type::Coroutine(_) => Some(4),
            ast::Type::Custom(name) => self.get(name.last().unwrap()).map(|st| st.size).flatten(),
            ast::Type::FunctionDef(..) => Some(0),
            ast::Type::CoroutineDef(..) => Some(0),
            ast::Type::StructDef(..) => Some(0),
            ast::Type::Unit => Some(0),
            ast::Type::Unknown => panic!("Requesting size for a type of Unknown"),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        diagnostics::config::TracingConfig,
        lexer::{lexer::Lexer, tokens::Token},
        semantics::type_checker::type_check,
        syntax::ast::Type,
    };
    use super::*;

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
            assert_eq!(unrealized_st.table["root::test"], StructDefinition::new("test", vec![("i".into(), Type::I32)]));
            assert_eq!(unrealized_st.table["root::test2"], StructDefinition::new("test2", vec![("t".into(), Type::Custom(vec!["root", "test"].into()))]))
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
            assert_eq!(unrealized_st.table["root::my_mod::test"], StructDefinition::new("test", vec![("i".into(), Type::I32)]));
            assert_eq!(unrealized_st.table["root::test2"], StructDefinition::new("test2", vec![("t".into(), Type::Custom(vec!["root", "my_mod", "test"].into()))]))
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
            assert_eq!(unrealized_st.table["root::my_mod::test"], StructDefinition::new("test", vec![("i".into(), Type::I32)]));
            assert_eq!(unrealized_st.table["root::test"], StructDefinition::new("test", vec![("t".into(), Type::Custom(vec!["root", "my_mod", "test"].into()))]))
        }
    }

    #[test]
    pub fn test_duplicate() {
        let mut st = UnresolvedStructTable{
            table: HashMap::new(),
            state: PhantomData,
        };
        let result = UnresolvedStructTable::insert_struct(&mut st.table, &vec!["root", "test"].into(), StructDefinition::new("test", vec![("i".into(), Type::I32)]));
        assert_eq!(result, Ok(()));
        let result = UnresolvedStructTable::insert_struct(&mut st.table, &vec!["root", "test"].into(), StructDefinition::new("test", vec![("i".into(), Type::I32)]));
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
            ("struct test{i: i32}",
            "root::test",
            StructDefinition {
                name: "test".into(),
                size: Some(4),
                fields: vec![("i".into(), Type::I32, Some(4))],
            }),
            ("struct test{y: bool}",
            "root::test",
            StructDefinition {
                name: "test".into(),
                size: Some(4),
                fields: vec![("y".into(), Type::Bool, Some(4))],
            }),
            ("struct test{i: i32, y: bool}",
            "root::test",
            StructDefinition {
                name: "test".into(),
                size: Some(8),
                fields: vec![("i".into(), Type::I32, Some(4)), ("y".into(), Type::Bool, Some(8))],
            }),
            ("struct test{s: string}",
            "root::test",
            StructDefinition {
                name: "test".into(),
                size: Some(4),
                fields: vec![("s".into(), Type::StringLiteral, Some(4))],
            }),
            ("mod my_mod{struct inner_test{i: i32, y: bool}}",
            "root::my_mod::inner_test",
            StructDefinition {
                name: "inner_test".into(),
                size: Some(8),
                fields: vec![("i".into(), Type::I32, Some(4)), ("y".into(), Type::Bool, Some(8))],
            }),
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
            ("
            struct test{i: i32}
            struct test2{t: test}
            ",
            "root::test2",
            StructDefinition {
                name: "test2".into(),
                size: Some(4),
                fields: vec![("t".into(), Type::Custom(vec!["root", "test"].into()), Some(4))],
            }),
            ("
            struct test{i: i32}
            struct test2{x: i32, t: test, b: bool}
            ",
            "root::test2",
            StructDefinition {
                name: "test2".into(),
                size: Some(12),
                fields: vec![("x".into(), Type::I32, Some(4)), ("t".into(), Type::Custom(vec!["root", "test"].into()), Some(8)), ("b".into(), Type::Bool, Some(12))],
            }),
            ("
            struct test{i: i32, j: i32}
            struct test2{x: i32, b: bool, t: test}
            ",
            "root::test2",
            StructDefinition {
                name: "test2".into(),
                size: Some(16),
                fields: vec![("x".into(), Type::I32, Some(4)), ("b".into(), Type::Bool, Some(8)), ("t".into(), Type::Custom(vec!["root", "test"].into()), Some(16))],
            }),
            ("
            struct test{i: i32}
            struct test2{t: test}
            struct test3{t2: test2}
            ",
            "root::test3",
            StructDefinition {
                name: "test3".into(),
                size: Some(4),
                fields: vec![("t2".into(), Type::Custom(vec!["root", "test2"].into()), Some(4))],
            }),
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

            assert_eq!(resolved.err(), Some("Could not resolve these structs: root::test, root::test2".into()));
        }
    }
}
