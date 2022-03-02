//! Converts the Bramble AST to the CFG MIR representation used for
//! dataflow analyses; such as, lifetime checking, variable initialization,
//! consistency rules checking, and so on.

// Transformer
// This process takes the AST for a compilation unit and transforms it into the
// CFG MIR used for dataflow analysis and LLVM IR generation by the Bramble
// compiler.

use crate::compiler::{ast::RoutineDef, semantics::semanticnode::SemanticContext};

use super::ir::{BasicBlockId, VarId, TempId};

/// Provides a Builder interface for constructing the MIR CFG representation of a 
/// routine. This will keep track of the current [`super::ir::BasicBlock`] and make sure that
/// MIR operations are applied to that [`super::ir::BasicBlock`]. This also provides a simplfied
/// interface for constructing the MIR operands, operations, and statements, to 
/// simplify the code that traverses input ASTs and transforms them into MIR.
pub struct MirGenerator {}

impl MirGenerator {
    pub fn new() -> MirGenerator {
        todo!()
    }

    fn module(&self) {}

    fn new_bb(&mut self) -> BasicBlockId {
        todo!()
    }

    fn set_bb(&mut self, bb: BasicBlockId) {
        todo!()
    }

    fn var(&mut self) -> VarId {
        todo!()
    }

    fn temp(&mut self) -> TempId {
        todo!()
    }

    fn sub(&mut self) {
        todo!()
    }

    fn mul(&mut self) {
        todo!()
    }

    /// Add two operands together
    fn add(&mut self) {
        todo!()
    }

    /// Terminates by returning to the caller function
    fn term_return(&mut self) {
        todo!()
    }

    /// Terminates by going to the destination basic block
    fn term_goto(&mut self, destination: BasicBlockId) {
        todo!()
    }

    /// Terminates with a conditional go to
    fn term_if(&mut self, then_bb: BasicBlockId, else_bb: BasicBlockId) {
        todo!()
    }

    /// Terminates by calling the given function
    fn term_call(&mut self) {
        todo!()
    }
}

/// Transform a single function to the MIR form
struct FuncTransformer {
    gen: MirGenerator,
}

impl FuncTransformer {
    pub fn new() -> FuncTransformer {
        FuncTransformer {
            gen: MirGenerator::new(),
        }
    }

    pub fn transform(&mut self, func: RoutineDef<SemanticContext>) {
        // Create a new MIR Procedure
        // Create a BasicBlock for the function
        let bb = self.gen.new_bb();
        self.gen.set_bb(bb);

        // Iterate over every expression and add it to the basic block

        // Add the return from function as the terminator for the final basic block of the function
        self.gen.term_return();
    }
}
