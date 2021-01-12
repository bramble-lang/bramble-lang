use std::{collections::HashMap, marker::PhantomData};

use crate::{semantics::semanticnode::SemanticNode, syntax::ast::Path};

use super::struct_table::StructDefinition;

#[derive(Debug)]
pub struct StructTable<S> {
    table: HashMap<String, StructDefinition>,
    state: PhantomData<S>,
}

pub struct Unrealized{}
pub struct Realized{}

type UnrealizedStructTable = StructTable<Unrealized>;
type RealizedStructTable = StructTable<Realized>;

impl UnrealizedStructTable {
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

    pub fn from(root: &SemanticNode) -> Result<UnrealizedStructTable, String> {
        let mut table = UnrealizedStructTable {
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

    pub fn realize(&self) -> Result<(), String> {
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::{
        diagnostics::config::TracingConfig,
        lexer::{lexer::Lexer, tokens::Token},
        semantics::type_checker::type_check,
        syntax::{
            ast::{self, Ast, Type},
            parser,
        },
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
            let unrealized_st = UnrealizedStructTable::from(&semantic_ast).unwrap();
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
            let unrealized_st = UnrealizedStructTable::from(&semantic_ast).unwrap();
            assert_eq!(unrealized_st.table["root::my_mod::test"], StructDefinition::new("test", vec![("i".into(), Type::I32)]));
            assert_eq!(unrealized_st.table["root::test2"], StructDefinition::new("test2", vec![("t".into(), Type::Custom(vec!["root", "my_mod", "test"].into()))]))
        }
    }

    #[test]
    pub fn test_duplicate() {
        let mut st = UnrealizedStructTable{
            table: HashMap::new(),
            state: PhantomData,
        };
        let result = UnrealizedStructTable::insert_struct(&mut st.table, &vec!["root", "test"].into(), StructDefinition::new("test", vec![("i".into(), Type::I32)]));
        assert_eq!(result, Ok(()));
        let result = UnrealizedStructTable::insert_struct(&mut st.table, &vec!["root", "test"].into(), StructDefinition::new("test", vec![("i".into(), Type::I32)]));
        assert_eq!(result, Err("Struct root::test already exists".into()));
    }
}
