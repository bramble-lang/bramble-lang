use serde::{Deserialize, Serialize};

use super::{
    node::{
        Context, Node, NodeType, {PostOrderIter, PreOrderIter},
    },
    parameter::Parameter,
    ty::Type,
};
use crate::result::Result;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct StructDef<M> {
    context: M,
    name: String,
    pub(super) fields: Vec<Parameter<M>>,
}

impl<M: Context> Node<M> for StructDef<M> {
    fn get_context(&self) -> &M {
        &self.context
    }

    fn get_context_mut(&mut self) -> &mut M {
        &mut self.context
    }

    fn node_type(&self) -> NodeType {
        NodeType::StructDef
    }

    fn children(&self) -> Vec<&dyn Node<M>> {
        let mut v: Vec<&dyn Node<M>> = vec![];
        for f in self.fields.iter() {
            v.push(f);
        }
        v
    }

    fn name(&self) -> Option<&str> {
        Some(&self.name)
    }

    fn iter_postorder(&self) -> PostOrderIter<M> {
        PostOrderIter::new(self)
    }

    fn iter_preorder(&self) -> PreOrderIter<M> {
        PreOrderIter::new(self)
    }
}

impl<M> std::fmt::Display for StructDef<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.write_str(self.get_name())
    }
}

impl<M> StructDef<M> {
    pub fn new(name: &str, context: M, fields: Vec<Parameter<M>>) -> StructDef<M> {
        StructDef {
            context,
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

    pub fn get_fields_mut(&mut self) -> &mut Vec<Parameter<M>> {
        &mut self.fields
    }

    pub fn get_field(&self, field: &str) -> Option<&Type> {
        self.fields.iter().find(|f| f.name == field).map(|f| &f.ty)
    }

    pub fn get_field_idx(&self, field: &str) -> Option<usize> {
        self.fields
            .iter()
            .enumerate()
            .find(|(_, f)| field == f.name)
            .map(|(idx, _)| idx)
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
