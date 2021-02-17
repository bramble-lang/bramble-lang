use std::{collections::HashMap, marker::PhantomData};

pub mod config;

pub struct DiagRecorder<A, B>
where
    A: Diag,
    B: Diag,
{
    src: String,
    ph_a: PhantomData<A>,
    ph_b: PhantomData<B>,
}

impl<A, B> DiagRecorder<A, B>
where
    A: Diag,
    B: Diag,
{
    pub fn new(src: &str) -> DiagRecorder<A, B> {
        DiagRecorder{
            src: src.into(),
            ph_a: PhantomData,
            ph_b: PhantomData,
        }
    }

    /**
    Begins a new unit of diagnostic recording.  Run this with the value of the current
    node immediately before the Transformation is applied to the node and its Annotation.
    Then immediately after the transformation, run `end` on the result. This will print
    diagnostic output to the screen.
     */
    pub fn begin(&self, annotation: A) {
        let d = annotation.diag();
        print!("{:?} => ", d);
    }

    /**
    Completes a diagnostic unit.
     */
    pub fn end(&self, annotation: B) {
        let d = annotation.diag();
        println!("{:?}", d);
    }
}

pub trait Diag {
    fn diag(&self) -> DiagData;
}

/**
Records state information about a single Node in an AST and its associated
Annotation.  Used by traversal operators to generate diagnostic and historical
data about every Compiler Transform that is applied to the AST.
 */
#[derive(Debug)]
pub struct DiagData {
    ln: u32,
    node_id: u32,
    data: HashMap<String, Vec<String>>,
}

impl DiagData {
    /**
    Create a new unit of DiagData. Each unit of DiagData is associated with
    the annotation on a single node of the AST.
     */
    pub fn new(ln: u32, node_id: u32) -> DiagData {
        DiagData {
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
