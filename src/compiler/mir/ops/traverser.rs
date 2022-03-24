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
        bb.stm_iter().for_each(|s| self.statement(s));

        // Convert the terminator
        match bb
            .get_term()
            .expect("Terminator must be defined for a basic block")
            .kind()
        {
            TerminatorKind::Return => self.xfmr.term_return(),
            TerminatorKind::GoTo { .. } => todo!(),
            TerminatorKind::CondGoTo { .. } => todo!(),
            TerminatorKind::CallFn { .. } => todo!(),
        }
    }

    fn statement(&mut self, stm: &Statement) {
        let span = stm.span();

        match stm.kind() {
            StatementKind::Assign(lv, rv) => {
                let lv = self.xfmr.lvalue(lv);
                let rv = self.rvalue(rv);
                self.xfmr.assign(span, lv, rv);
            }
        }
    }

    fn rvalue(&mut self, rv: &RValue) -> V {
        match rv {
            RValue::Use(o) => self.operand(o),
            RValue::BinOp(_, _, _) => todo!(),
            RValue::UnOp(_, _) => todo!(),
            RValue::Cast(_, _) => todo!(),
            RValue::AddressOf(_) => todo!(),
        }
    }

    fn operand(&mut self, o: &Operand) -> V {
        match o {
            Operand::Constant(c) => self.xfmr.constant(*c),
            Operand::LValue(lv) => {
                let l = self.xfmr.lvalue(lv);
                self.xfmr.load(l)
            }
        }
    }
}
