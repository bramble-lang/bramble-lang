use std::{collections::HashMap, marker::PhantomData};

use config::TracingConfig;

pub mod config;

pub struct DiagRecorder<A, B>
where
    A: Diag,
    B: Diag,
{
    src: String,
    config: TracingConfig,
    before_transform: Option<DiagData>,
    ph_a: PhantomData<A>,
    ph_b: PhantomData<B>,
}

impl<A, B> DiagRecorder<A, B>
where
    A: Diag,
    B: Diag,
{
    pub fn new(src: &str, config: TracingConfig) -> DiagRecorder<A, B> {
        DiagRecorder {
            src: src.into(),
            config,
            before_transform: None,
            ph_a: PhantomData,
            ph_b: PhantomData,
        }
    }

    /**
    Marks the beginning of a unit of compilation.
     */
    pub fn start_trace(&self) {
        if self.config != TracingConfig::Off {
            println!("Starting {}", self.src);
        }
    }

    /**
    Marks the end of a unit of compilation.
     */
    pub fn end_trace(&self) {
        if self.config != TracingConfig::Off {
            println!("Complete {}", self.src);
        }
    }

    /**
    Begins a new unit of diagnostic recording.  Run this with the value of the current
    node immediately before the Transformation is applied to the node and its Annotation.
    Then immediately after the transformation, run `end` on the result. This will print
    diagnostic output to the screen.
     */
    pub fn begin(&mut self, annotation: &A) {
        let d = annotation.diag();
        if self.config.trace(d.ln as usize) {
            self.before_transform = Some(d);
        }
    }

    /**
    Completes a diagnostic unit.
     */
    pub fn end(&mut self, annotation: &B) {
        let d = annotation.diag();
        if self.config.trace(d.ln as usize) {
            match self.before_transform.take() {
                Some(before_diag) if before_diag.data.len() > 0 => {
                    print!("{} ", before_diag);
                }
                _ => (),
            };
            if d.data.len() > 0 {
                println!("=> {}", d);
            }
        }
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

impl std::fmt::Display for DiagData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("L{}n{}: ", self.ln, self.node_id))?;
        for (k, values) in self.data.iter() {
            let values_text: String = values
                .iter()
                .map(|v| format!("{}", v))
                .collect::<Vec<String>>()
                .join(",");

            if values.len() == 1 {
                f.write_fmt(format_args!("{{{}}}: {}", k, values_text))?;
            } else {
                f.write_fmt(format_args!("{{{}}}: [{}]", k, values_text))?;
            }
        }
        Ok(())
    }
}
