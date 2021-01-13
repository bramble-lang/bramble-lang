use crate::syntax::ast;

#[derive(Debug, PartialEq)]
pub struct StructDefinition {
    pub name: String,
    pub size: Option<i32>,
    pub fields: Vec<(String, ast::Type, Option<i32>)>,
}

impl StructDefinition {
    pub fn new(name: &str, fields: Vec<(String, ast::Type)>) -> StructDefinition {
        let mut nfields = vec![];
        for (fname, fty) in fields.iter() {
            nfields.push((fname.clone(), fty.clone(), None));
        }

        StructDefinition {
            name: name.into(),
            fields: nfields,
            size: None,
        }
    }

    pub fn get_offset_of(&self, field: &str) -> Option<i32> {
        match self.size {
            None => None,
            Some(sz) => self
                .fields
                .iter()
                .find(|(fname, _, _)| fname == field)
                .map_or(None, |f| f.2)
                .map_or(None, |x| Some(sz - x)),
        }
    }

    pub fn get_fields(&self) -> &Vec<(String, ast::Type, Option<i32>)> {
        &self.fields
    }
}
