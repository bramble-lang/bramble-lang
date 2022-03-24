//! Traverses the MIR representation of a function and calls the appropriate
//! methods on a value which implements the [`super::transformer::Transformer`] trait.

use std::marker::PhantomData;

use crate::compiler::mir::ir::*;

use super::transformer::Transformer;

/// This will traverse the MIR representation of a function and use the given
/// [`Transformer`] implementation to generate a new IR for the function.
///
/// `L` - the type used by the target IR to represent addressable expressions
/// or [`LValue`].
///
/// `V` - the type used by the target IR to represent values such as
/// constants or the value read from a [memory location](LValue).
///
/// `T` - the type that implements the [`Transformer`] trait and will actually handle the
/// conversion to the target IR.
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

    /// Call the [`Transformer`] on a statement
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

    /// Use the [`Transformer`] to convert a MIR [`RValue`] to the target IR value type `V`
    fn rvalue(&mut self, rv: &RValue) -> V {
        match rv {
            RValue::Use(o) => self.operand(o),
            RValue::BinOp(_, _, _) => todo!(),
            RValue::UnOp(_, _) => todo!(),
            RValue::Cast(_, _) => todo!(),
            RValue::AddressOf(_) => todo!(),
        }
    }

    /// Use the [`Transformer`] to convert a Mir [`Operand`] to the target IR value type `V`
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
