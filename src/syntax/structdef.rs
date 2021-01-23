use super::ty::Type;
use braid_lang::result::Result;

#[derive(Clone, Debug, PartialEq)]
pub struct StructDef<M> {
    metadata: M,
    name: String,
    fields: Vec<(String, Type)>,
}

impl<M> StructDef<M> {
    pub fn new(name: &str, metadata: M, fields: Vec<(String, Type)>) -> StructDef<M> {
        StructDef {
            metadata,
            name: name.into(),
            fields,
        }
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_annotations(&self) -> &M {
        &self.metadata
    }

    pub fn get_fields(&self) -> &Vec<(String, Type)> {
        &self.fields
    }

    pub fn get_field(&self, field: &str) -> Option<&Type> {
        self.fields.iter().find(|f| f.0 == field).map(|f| &f.1)
    }

    pub fn add_field(&mut self, field: &str, ty: Type) -> Result<()> {
        if self.get_field(field).is_none() {
            self.fields.push((field.into(), ty));
            Ok(())
        } else {
            Err(format!(
                "Field {} already exists in structure {}",
                field, self.name
            ))
        }
    }

    pub fn root_str(&self) -> String {
        format!("struct {}", self.name)
    }
}
