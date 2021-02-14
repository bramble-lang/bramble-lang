use crate::ast::Type;
use braid_lang::result::Result;

#[derive(Clone, Debug, PartialEq)]
pub struct FieldInfo {
    pub name: String,
    pub ty: Type,
    pub offset: Option<i32>,
}

impl FieldInfo {
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn ty(&self) -> &Type {
        &self.ty
    }
    pub fn offset(&self) -> Option<i32> {
        self.offset
    }
}

#[derive(Debug, PartialEq)]
pub struct StructDefinition {
    pub name: String,
    pub size: Option<i32>,
    pub fields: Vec<FieldInfo>,
}

impl StructDefinition {
    pub fn new(name: &str, fields: Vec<(String, Type)>) -> StructDefinition {
        let mut nfields = vec![];
        for (fname, fty) in fields.iter() {
            nfields.push(FieldInfo {
                name: fname.clone(),
                ty: fty.clone(),
                offset: None,
            });
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
                .find(|FieldInfo { name, .. }| name == field)
                .map_or(None, |f| f.offset)
                .map_or(None, |x| Some(sz - x)),
        }
    }

    pub fn get_fields(&self) -> &Vec<FieldInfo> {
        &self.fields
    }

    pub fn set_field_sizes(&self, sizes: &Vec<i32>) -> Result<StructDefinition> {
        if self.fields.len() != sizes.len() {
            return Err(format!(
                "Cannot set field sizes: expected {} sizes but got {}",
                self.fields.len(),
                sizes.len()
            ));
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
            sd.fields[idx].offset = Some(total_sz);
        }
        sd.size = Some(total_sz);
        Ok(sd)
    }
}
