use std::{collections::HashMap, marker::PhantomData};

use super::{ast::CompilerNode, struct_table::StructDefinition};

pub struct StructTable<S> {
    table: HashMap<String,StructDefinition>,
    state: PhantomData<S>,
}

pub type Unrealized = ();
pub type Realized = ((),());

pub type UnrealizedStructTable = StructTable<Unrealized>;
pub type RealizedStructTable = StructTable<Realized>;

impl<S> StructTable<S> {
    pub fn from(root: &CompilerNode) -> UnrealizedStructTable {
        let mut table = UnrealizedStructTable {
            table: HashMap::new(),
            state: PhantomData,
        };

        table
    }

    fn traverse(root: &CompilerNode, table: &mut HashMap<String, StructDefinition>) {

    }
}