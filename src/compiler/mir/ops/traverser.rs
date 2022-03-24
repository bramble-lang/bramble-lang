//! Traverses the MIR representation of a function and calls the appropriate
//! methods on a value which implements the [`super::transformer::Transformer`] trait.

use std::marker::PhantomData;

use crate::compiler::mir::{ir::*, MirProject};

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
    mir: &'a MirProject,
    function: Option<&'a Procedure>,
    _l: PhantomData<L>,
    _v: PhantomData<V>,
}

impl<'a, L, V, T: Transformer<L, V>> Traverser<'a, L, V, T> {
    pub fn new(mir: &'a MirProject, xfmr: &'a mut T) -> Self {
        Self {
            xfmr,
            mir,
            function: None,
            _l: PhantomData,
            _v: PhantomData,
        }
    }

    pub fn map(&mut self) {
        // Iterate over every function in MIR
        for f in self.mir.function_iter() {
            // For each function, iterate over every BB
            self.function = Some(f);
            for bb in f.bb_iter() {
                self.basic_block(bb)
            }
        }
    }

    /// Traverses every variable, [statement](Statement), and the final [terminator](Terminator)
    /// in the given [`BasicBlock`] and calls the appropriate conversion functions on the
    /// given [`Transformer`].
    pub fn basic_block(&mut self, bb: &BasicBlock) {
        self.xfmr.start_bb(BasicBlockId::new(0));
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
                let lv = self.lvalue(lv);
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
                let l = self.lvalue(lv);
                self.xfmr.load(l)
            }
        }
    }

    fn lvalue(&mut self, lv: &LValue) -> L {
        match lv {
            LValue::Static(_) => todo!(),
            LValue::Var(vid) => {
                // Get function which is currently being converted
                // Look up VarId to get the vardecl
                let vd = self.get_var(*vid);

                // Convert VarDecl
                self.xfmr.var(vd)
            }
            LValue::Temp(_) => todo!(),
            LValue::Access(_, _) => todo!(),
            LValue::ReturnPointer => todo!(),
        }
    }

    /// Given a [`VarId`] this will find the associated [`VarDecl`] from the currently
    /// transforming [`Procedure`].  If no [`Procedure`] is currently being transformed this
    /// will panic.
    fn get_var(&self, id: VarId) -> &VarDecl {
        match self.function {
            Some(f) => f.get_var(id),
            None => panic!("Must be converting a fucntion to look up a VarId"),
        }
    }
}
