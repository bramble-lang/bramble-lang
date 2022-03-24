//! Traverses the MIR representation of a function and calls the appropriate
//! methods on a value which implements the [`super::transformer::Transformer`] trait.

use std::marker::PhantomData;

use crate::compiler::mir::ir::*;

use super::transformer::Transformer;

pub struct Traverser<'a, L, V, T: Transformer<L, V>> {
    xfmr: &'a mut T,
    _l: PhantomData<L>,
    _v: PhantomData<V>,
}

impl<'a, L, V, T: Transformer<L, V>> Traverser<'a, L, V, T> {
    pub fn basic_block(&mut self, bb: &BasicBlock) {
        // Iterate over all the variables in the block

        // Iterate over the statements in the basic block

        // Convert the terminator
        match bb
            .get_term()
            .expect("Terminator must be defined for a basic block")
            .kind()
        {
            TerminatorKind::Return => self.xfmr.term_return(),
            TerminatorKind::GoTo { target } => todo!(),
            TerminatorKind::CondGoTo { cond, tru, fls } => todo!(),
            TerminatorKind::CallFn {
                func,
                args,
                reentry,
            } => todo!(),
        }
    }
}
