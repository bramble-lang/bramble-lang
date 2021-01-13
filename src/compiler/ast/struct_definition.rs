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

    pub fn set_field_sizes(&self, sizes: &Vec<i32>) -> Result<StructDefinition, String> {
        if self.fields.len() != sizes.len() {
            return Err(format!("Cannot set field sizes: expected {} sizes but got {}", self.fields.len(), sizes.len()));
        }
        let mut sd = StructDefinition {
            name: self.name.clone(),
            size: None,
            fields: self.fields.clone(),
        };
        let mut total_sz = 0;
        for idx in 0..sizes.len() {
            let field_sz = sizes[idx];
            total_sz += field_sz;
            sd.fields[idx].2 = Some(total_sz);
        }
        sd.size = Some(total_sz);
        Ok(sd)
    }
}
