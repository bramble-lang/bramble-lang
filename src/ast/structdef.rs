use super::{node::{Node, NodeType}, parameter::Parameter, ty::Type};
use braid_lang::result::Result;

#[derive(Clone, Debug, PartialEq)]
pub struct StructDef<M> {
    annotations: M,
    name: String,
    pub(super) fields: Vec<Parameter<M>>,
}

impl<M> Node<M> for StructDef<M> {
    fn annotation(&self) -> &M {
        &self.annotations
    }

    fn annotation_mut(&mut self) -> &mut M {
        &mut self.annotations
    }

    fn node_type(&self) -> NodeType {
        NodeType::StructDef
    }

    fn children(&self) -> Vec<&dyn Node<M>> {
        let mut v:Vec<&dyn Node<M>> = vec![];
        for f in self.fields.iter() {
            v.push(f);
        }
        v
    }

    fn name(&self) -> Option<&str> {
        Some(&self.name)
    }
}

impl<M> std::fmt::Display for StructDef<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.write_str(self.get_name())
    }
}

impl<M> StructDef<M> {
    pub fn new(name: &str, annotations: M, fields: Vec<Parameter<M>>) -> StructDef<M> {
        StructDef {
            annotations,
            name: name.into(),
            fields,
        }
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_fields(&self) -> &Vec<Parameter<M>> {
        &self.fields
    }

    pub fn get_field(&self, field: &str) -> Option<&Type> {
        self.fields.iter().find(|f| f.name == field).map(|f| &f.ty)
    }

    pub fn add_field(&mut self, field: Parameter<M>) -> Result<()> {
        if self.get_field(&field.name).is_none() {
            self.fields.push(field);
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
