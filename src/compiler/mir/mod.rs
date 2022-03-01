/*!
  The middle intermediate representation for the Bramble compiler.
  This models functions as [Control Flow Graphs](https://en.wikipedia.org/wiki/Control-flow_graph)
  and makes lifetime, consistency, and other analyses easier to 
  implement.

  This module consists of the following major tools:
  
  1. IR Model: a set of types which are used to represent program in CFG form.
  2. MIR Compiler: this will convert an AST into a MIR representation.
  3. Analysis: tools used for traversing and transforming the MIR representation
  that is generated from the MIR compiler.
 */
use crate::StringId;

use super::{ast::Type, Span};

/// Procedure
/// This type represents a single function from the input source code.
struct Procedure {
    /// The set of basic blocks that constitute this procedure
    blocks: Vec<BasicBlock>,
    /// The return type of this function
    ret_ty: Type,
    /// The set of all user declared variables from within this function
    vars: Vec<VarDecl>,
    /// The set of all temporary variables created by the MIR compiler
    temps: Vec<TempDecl>,
    /// The span of input source code that this IR element covers
    span: Span,
}

/// Identifier for a specific basic block in a procedure
struct BasicBlockId(usize);

/// Identifier for a variable or temporary variable.
struct VarId(usize);

/// Identifier for each scope that exists within the function
struct ScopeId(usize);

/// A variable declared by the user.
struct VarDecl {
    /// Name of this variable
    name: StringId,
    /// Whether this variable can be mutated
    mutability: bool,
    /// The type of this variable
    ty: Type,
    /// What scope this variable was declared in
    scope: ScopeId,
}

/// A temporary variable created by the MIR compiler to store
/// results.
struct TempDecl {
    /// The type of this variable
    ty: Type,
}

/// Basic Block
/// A single basic block from a CFG
struct BasicBlock {
    statements: Vec<Statement>,
    terminator: Terminator,

    span: Span,
}

struct StatementId(usize);

/// Statement
/// A single statement, from which basic blocks are constructed
struct Statement {
    kind: StatementKind,

    span: Span,
}

enum StatementKind {
    /// This statement assigns the result of an [`RValue`] operation
    /// to the memory location represented by the [`LValue`].
    Assign(LValue, RValue)
}

/// LValue
/// A physical location in memory where a value can be stored
enum LValue {
    /// A user defined variable.
    Var(VarId),

    /// A temporary variable created by the compiler during MIR construction
    Temp(VarId),

    /// Represents accessing data via some form of transformation (e.g., array index,
    /// dereferencing a raw pointer, or the field of a structure)
    Subdata(Subdata),

    /// Where to store the result of a procedure so that the caller can get the result.
    ReturnPointer,
}

enum Subdata {
}

/// RValue
/// An operation that results in a value which can be
/// stored in some physical location in memory
enum RValue {
    /// Provides a way of reading a specific variable or using a constant in an assignment.
    Use(Operand),

    /// Represents binary insructions that are available on the CPU
    BinOp(Operand,Operand),

    /// Unary instructions that are available on the CPU
    UnOp(Operand),

    /// Casting an operand to a new type
    Cast(Operand, Type),

    /// Getting the address of a variable in memory.
    AddressOf(Operand),
}

/// Operand
/// Value that can be used as the parameters for the RValue operations
enum Operand {
    Constant,
    LValue(LValue),
}

/// Terminator
/// Marks the final statement in a basic block and indicates where the
/// program will go to next
struct Terminator {
    kind: TerminatorKind,

    span: Span,
}

enum TerminatorKind {
    /// Enter a new functions scope.
    CallFn{
        /// The function to enter
        func: Operand,
        /// The arguments for the function being called
        args: Vec<Operand>,
        /// The result of the function and which basic block is the reentry point from the called function
        reentry: (LValue, BasicBlockId),
    },

    /// Return from this function to the calling function.
    Return,

    /// Unconditionally, go to the given basic block
    GoTo{
        target: BasicBlockId,
    },

    /// Takes a boolean value and two basic blocks. On true it will go to to the first
    /// basic block, on false it will go to to the second basic block.
    CondGoTo{
        /// The value used to determine which of the two basic blocks to go to
        cond: Operand,
        /// If `cond` is true, then go to this basic block
        tru: BasicBlockId,
        /// If `cond` is false, then go to this basic block
        fls: BasicBlockId,
    },
}

enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Le,
    Lt,
    Ge,
    Gt,
}

enum UnOp {
    Negate,
    Not,
}