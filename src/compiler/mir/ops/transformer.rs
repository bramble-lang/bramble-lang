//! Defines the Transformer trait: a set of methods which are called by
//! the Traverser process. The methods correspond to different MIR elements
//! and can be used to convert a single MIR element to an element of a different
//! representation. This trait allows MIR to X conversion to be written
//! independently of the MIR traversal logic.
//!
//! The transformation of a function MIR into LLVM will work by following these steps:
//! 1. Create a vector of LLVM Basic Blocks with 1-1 correspondance to the MIR
//! Basic Blocks.
//! 2. Execute the Traverser and pass it the LLVM Transformer.
//! 3. When start_bb is called use its BasicBlockId to look up the associated LLVM
//! BB in the LLVM BB vector by converting the BasicBlockId to a [`usize`]. Then
//! tell the LLVM Builder that is the current basic block.
//! 4. When a Terminator is conversion happens. Use the BasicBlockId(s) in the
//! [`TerminatorKind`] to look up the associated LLVM BasicBlock objects in
//! the vector.
//! 5. Need to construct the Phi operator in the merge point Basic Block.

use crate::compiler::{mir::ir::*, Span};

/// The MIR Transformer defines an interface between the process which traverses
/// the MIR (which is a Control Flow Graph) and the process which converts a MIR
/// element into the Target Type. The goal of this design is to make it so that
/// all structural and topological elements of the MIR exist only within the
/// Traverser process.  The Transformer only has to care about converting specific
/// MIR values which correspond to Bramble language entities to the target representation.
/// For example: constants and operators get converted by the Transformer. But
/// deconstructing the [`RValue`] enumeration to extract the [`Operands`](Operand)
/// exists only within the Traverser.
///
/// Furhermore, the Traverser will be responsible for managing the intermediate
/// results of converting the [`RValue`] or [`LValue`] prior to the conversion
/// of the [`Statement`]. To allow for the Traverser to manage the intermediate
/// values before the conversion of the [`Statement`] a generic type parameter
/// `V` is used, this will be the type used by the Target IR to represent an expression
/// result. The type parameter `L` will have the type used to represent variables
/// and memory locations (i.e., addressable expressions).
///
/// Even though there will only be one implementation of this trait (LLVM), there
/// is still a reason for this trait to exist. To create a decoupling between the
/// mir module and the LLVM IR module and avoid having bi-directional imports creating
/// a more confusing dependency graph.
pub trait Transformer<L, V> {
    fn create_bb(&mut self, bb: BasicBlockId) -> Result<(), TransformerResult>;
    fn set_bb(&mut self, bb: BasicBlockId) -> Result<(), TransformerResult>;

    /// Allocate space for the given variable declaration
    fn alloc_var(&mut self, id: VarId, vd: &VarDecl) -> Result<(), TransformerResult>;
    fn alloc_temp(&mut self);

    /// Tells the program to exit this [`BasicBlock`] by returning to the calling function
    fn term_return(&mut self);

    /// Tells the program to go to one of two [`BasicBlocks`](BasicBlock) based upon whether
    /// the given conditional is true or false.
    fn term_cond_goto(&mut self, cond: V, then_bb: BasicBlockId, else_bb: BasicBlockId);

    /// Tells the program to go to the given [`BasicBlock`].
    fn term_goto(&mut self, target_bb: BasicBlockId);

    /// Store the given value to the given memory location
    fn assign(&mut self, span: Span, l: L, v: V);

    /// Convert the given variable declaration to a specific location in memory
    fn var(&self, v: VarId) -> L;

    // The following methods correspond to [`RValue`] variants

    /// Create a const [`i64`].
    fn const_i64(&self, i: i64) -> V;

    /// Create a const [`bool`].
    fn const_bool(&self, b: bool) -> V;

    /// Load a value from a memory location
    fn load(&self, lv: L) -> V;

    /// Add two values together
    fn add(&self, a: V, b: V) -> V;

    /// Subtract two values
    fn sub(&self, a: V, b: V) -> V;
}

#[derive(Debug, Clone, Copy)]
pub enum TransformerResult {
    VariableAlreadyAllocated,
    BasicBlockAlreadyCreated,
    BasicBlockNotFound,
}
