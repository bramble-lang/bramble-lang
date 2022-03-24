//! Transforms the MIR representation into LLVM

use crate::compiler::mir::{ir::*, Transformer};

struct LlvmTransformer {}

impl<L, V> Transformer<L, V> for LlvmTransformer {
    fn start_bb(&mut self, bb: BasicBlockId) {
        todo!()
    }

    fn add_var(&mut self) {
        todo!()
    }

    fn add_temp(&mut self) {
        todo!()
    }

    fn term_return(&mut self) {
        todo!()
    }

    fn assign(&mut self, span: crate::compiler::Span, l: L, v: V) {
        todo!()
    }

    fn lvalue(&self, l: &LValue) -> L {
        todo!()
    }

    fn constant(&self, c: Constant) -> V {
        todo!()
    }

    fn load(&self, lv: L) -> V {
        todo!()
    }

    fn add(&self, a: V, b: V) -> V {
        todo!()
    }

    fn sub(&self, a: V, b: V) -> V {
        todo!()
    }
}
