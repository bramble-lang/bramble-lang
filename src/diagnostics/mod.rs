use std::collections::HashMap;

pub mod config;

/**
Records state information about a single Node in an AST and its associated 
Annotation.  Used by traversal operators to generate diagnostic and historical
data about every Compiler Transform that is applied to the AST.
 */
pub struct DiagData {
    ln: u32,
    node_id: u32,
    data: HashMap<String,Vec<String>>,
}

impl DiagData {
    /**
    Create a new unit of DiagData. Each unit of DiagData is associated with 
    the annotation on a single node of the AST.
     */
    pub fn new(ln: u32, node_id: u32) -> DiagData {
        DiagData{
            ln,
            node_id,
            data: HashMap::new(),
        }
    }

    /**
    Adds a value to the diagnostic data under `label`, if a value is already
    associated with `label` then this new value is appended to the history of
    values for `label`. This prevents the loss of any diagnostic data.

    Returns the number of values that are now in the history of `label`.
     */
    pub fn add(&mut self, label: &str, value: &str) -> usize {
        match self.data.get_mut(label) {
            None => {
                self.data.insert(label.into(), vec![value.into()]);
                1
            }
            Some(history) => {
                history.push(value.into());
                history.len()
            }
        }
    }
}