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

use std::{collections::VecDeque, fmt::Debug};

use crate::{
    compiler::{
        ast::Path,
        mir::{ir::*, project::DefId, typetable::FieldId, MirTypeDef, TypeId},
        Span,
    },
    StringId,
};

/// Defines the interface used by the [`ProgramTraverser`](super::ProgramTraverser)
/// to convert a MIR program into another IR form.
pub trait ProgramBuilder<'p, L, V, F: FunctionBuilder<L, V>> {
    /// Will attempt to Add the given function to the set of functions in the target
    /// IR.
    fn add_function(
        &mut self,
        func_id: DefId,
        canonical_path: &Path,
        args: &[ArgDecl],
        is_variadic: bool,
        ret_ty: TypeId,
    ) -> Result<(), TransformerError>;

    fn declare_struct(&mut self, id: TypeId, path: &Path) -> Result<(), TransformerError>;
    fn add_type(&mut self, id: TypeId, ty: &MirTypeDef) -> Result<(), TransformerError>;

    /// Creates a new transformer for the given function
    fn get_function_transformer(&'p self, func_id: DefId) -> Result<F, TransformerError>;
}

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
/// Furthermore, the Traverser will be responsible for managing the intermediate
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
pub trait FunctionBuilder<L, V> {
    fn create_bb(&mut self, id: BasicBlockId, bb: &BasicBlock) -> Result<(), TransformerError>;
    fn set_bb(&mut self, bb: BasicBlockId) -> Result<(), TransformerError>;

    /// Allocate space for the given variable declaration
    fn alloc_arg(&mut self, id: ArgId, decl: &ArgDecl) -> Result<(), TransformerError>;
    fn alloc_var(&mut self, id: VarId, vd: &VarDecl) -> Result<(), TransformerError>;
    fn alloc_temp(&mut self, id: TempId, vd: &TempDecl) -> Result<(), TransformerError>;

    /// Tells the program to exit this [`BasicBlock`] by returning to the calling function
    fn term_return(&mut self);

    /// Tells the program to go to one of two [`BasicBlocks`](BasicBlock) based upon whether
    /// the given conditional is true or false.
    fn term_cond_goto(
        &mut self,
        cond: V,
        then_bb: BasicBlockId,
        else_bb: BasicBlockId,
    ) -> Result<(), TransformerError>;

    /// Tells the program to enter into a new function and, when that function is complete,
    /// where to store the result and where to reenter this function.
    fn term_call_fn(
        &mut self,
        span: Span,
        target: L,
        args: VecDeque<V>,
        reentry: (L, BasicBlockId),
    ) -> Result<(), TransformerError>;

    /// Tells the program to go to the given [`BasicBlock`].
    fn term_goto(&mut self, target_bb: BasicBlockId) -> Result<(), TransformerError>;

    /// Load a value from a memory location
    fn load(&self, lv: L) -> Result<V, TransformerError>;

    /// Store the given value to the given memory location
    fn store(&mut self, span: Span, l: L, r: V);

    /// Returns a location value for a specific static item.
    fn static_loc(&self, id: DefId) -> Result<L, TransformerError>;

    /// Convert the given variable declaration to a specific location in memory
    fn var(&self, v: VarId) -> Result<L, TransformerError>;

    /// Convert the given variable declaration to a specific location in memory
    fn temp(&self, v: TempId) -> Result<L, TransformerError>;

    /// Returns the location of the element at an index in an array.
    fn array_access(&self, l: L, idx: V) -> Result<L, TransformerError>;

    /// Returns the location of the field of a structure.
    fn field_access(&self, l: L, field: FieldId) -> Result<L, TransformerError>;

    /// Returns a location that manages passing the return value back
    /// to the calling function.
    fn return_ptr(&self) -> Result<L, TransformerError>;

    // The following methods correspond to [`RValue`] variants

    /// Create a const [`i8`].
    fn const_i8(&self, i: i8) -> V;

    /// Create a const [`i16`].
    fn const_i16(&self, i: i16) -> V;

    /// Create a const [`i32`].
    fn const_i32(&self, i: i32) -> V;

    /// Create a const [`i64`].
    fn const_i64(&self, i: i64) -> V;

    /// Create a const [`u8`].
    fn const_u8(&self, i: u8) -> V;

    /// Create a const [`u16`].
    fn const_u16(&self, i: u16) -> V;

    /// Create a const [`u32`].
    fn const_u32(&self, i: u32) -> V;

    /// Create a const [`u64`].
    fn const_u64(&self, i: u64) -> V;

    /// Create a const [`bool`].
    fn const_bool(&self, b: bool) -> V;

    /// Create a const null pointer value.
    fn const_null(&self) -> V;

    /// Create a const [`f64`].
    fn const_f64(&self, f: f64) -> V;

    /// Returns a pointer to the given string literal
    fn string_literal(&mut self, s: StringId) -> V;

    /// Add two values together
    fn i_add(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Subtract two values
    fn i_sub(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Multiply two values
    fn i_mul(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Divide two values
    fn si_div(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Divide two unsigned integer values
    fn ui_div(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Add two values together
    fn f_add(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Subtract two values
    fn f_sub(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Multiply two values
    fn f_mul(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Divide two values
    fn f_div(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Equal
    fn i_eq(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Not equal
    fn i_neq(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Less than
    fn si_lt(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Less than
    fn ui_lt(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Less than or equal
    fn si_lte(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Less than or equal
    fn ui_lte(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Greater than
    fn si_gt(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Greater than
    fn ui_gt(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Greater than or equal
    fn si_gte(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Greater than or equal
    fn ui_gte(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Equal
    fn f_eq(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Not equal
    fn f_neq(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Less than
    fn f_lt(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Less than or equal
    fn f_lte(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Greater than
    fn f_gt(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Greater than or equal
    fn f_gte(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Negate an integer
    fn i_neg(&self, a: V) -> Result<V, TransformerError>;

    /// Negate an integer
    fn f_neg(&self, a: V) -> Result<V, TransformerError>;

    /// Bitwise not an integer value.
    fn i_not(&self, a: V) -> Result<V, TransformerError>;

    /// Bitwise and two integer values
    fn i_and(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Bitwise or two integer values
    fn i_or(&self, a: V, b: V) -> Result<V, TransformerError>;

    /// Address of a given Location value.
    fn cast(
        &self,
        l: V,
        l_signed: bool,
        l_sz: u64,
        target: TypeId,
        target_signed: bool,
        target_sz: u64,
    ) -> Result<V, TransformerError>;

    /// Generate a constant with the in memory size of the given type
    fn size_of(&self, ty: TypeId) -> V;

    /// Address of a given Location value.
    fn address_of(&self, a: L) -> Result<V, TransformerError>;

    /// Use the location pointed to by an address to create an addressable expression.
    fn deref(&self, a: L) -> Result<L, TransformerError>;

    /// Use the location pointed to by an address to create an addressable expression.
    fn pointer_offset(&self, a: V, o: V) -> Result<V, TransformerError>;
}

#[derive(Debug, Clone, Copy)]
pub enum TransformerError {
    VariableAlreadyAllocated,
    BasicBlockAlreadyCreated,
    BasicBlockNotFound,
    TempNotFound,
    VarNotFound,
    FunctionAlreadyDeclared,
    FunctionNotFound,
    TypeAlreadyDefined,
    StructUndefined,
    TypeNotFound(TypeId),
    ArgNotFound,
    Internal(&'static dyn TransformerInternalError),
}

pub trait TransformerInternalError: Debug {}
