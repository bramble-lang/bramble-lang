//! The IR abstractions used to represent any given Bramble program
//! as a CFG.

use crate::{StringId, compiler::{ast::Type, Span}};

const ROOT_SCOPE: usize = 0;

/// Procedure
/// This type represents a single function from the input source code.
#[derive(Debug, PartialEq, Clone)]
pub struct Procedure {
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

impl Procedure {
    /// Creates a new MIR procedure. When created this will not have any
    /// basic blocks or arguments.
    pub fn new(ret_ty: &Type, span: Span) -> Procedure {
        Procedure {
            blocks: vec![],
            ret_ty: ret_ty.clone(),
            vars: vec![],
            temps: vec![],
            span,
        }
    }

    /// Add an argument to this procedure
    pub fn add_arg(&mut self, name: StringId, ty: &Type) -> VarId {
        let vd = VarDecl::new(name, false, ty, ScopeId::new(ROOT_SCOPE));
        self.vars.push(vd);
        let id = self.vars.len() - 1;
        VarId::new(id)
    }

    /// Get a [`BasicBlock`] for this procedure
    pub fn get_bb(&self, id: BasicBlockId) -> &BasicBlock {
        &self.blocks[id.index()]
    }

    /// Get a mutable [`BasicBlock`] from this procedure.
    pub fn get_bb_mut(&mut self, id: BasicBlockId) -> &mut BasicBlock {
        &mut self.blocks[id.index()]
    }

    /// Create a new, empty, [`BasicBlock`] in this procedure. This will
    /// return the ID of that [`BasicBlock`].
    pub fn new_bb(&mut self) -> BasicBlockId {
        let bb = BasicBlock::new();
        self.blocks.push(bb);
        let id = self.blocks.len() - 1;
        BasicBlockId::new(id)
    }

    /// Get the declaration details of a user defined variable in
    /// this procedure.
    pub fn get_var(&self, id: VarId) -> &VarDecl {
        &self.vars[id.index()]
    }

    /// Get the declaration details of a temporary variable.
    pub fn get_temp(&self, id: TempId) -> &TempDecl {
        &self.temps[id.index()]
    }

    /// Add a new user defined variable to the procedure.
    pub fn add_var(&mut self, name: StringId, mutable: bool, ty: &Type, scope: ScopeId) -> VarId {
        let vd = VarDecl::new(name, mutable, ty, scope);
        self.vars.push(vd);
        let id = self.vars.len() - 1;

        VarId::new(id)
    }

    /// Add a new temporary variable to the procedures stack
    pub fn add_temp(&mut self, ty: &Type) -> TempId {
        let td = TempDecl::new(ty);
        self.temps.push(td);
        let id = self.temps.len() - 1;
        TempId::new(id)
    }
}

/// Identifier for a specific basic block in a procedure
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct BasicBlockId(usize);

impl BasicBlockId {
    pub fn new(id: usize) -> BasicBlockId {
        BasicBlockId(id)
    }

    pub fn index(&self) -> usize {
        self.0
    }
}

/// Identifier for a user declared variable
#[derive(Debug, PartialEq, Clone)]
pub struct VarId(usize);

impl VarId {
    pub fn new(id: usize) -> VarId {
        VarId(id)
    }

    pub fn index(&self) -> usize {
        self.0
    }
}

/// Identifier for a temporary variable.
#[derive(Debug, PartialEq, Clone)]
pub struct TempId(usize);

impl TempId {
    pub fn new(id: usize) -> TempId {
        TempId(id)
    }

    pub fn index(&self) -> usize {
        self.0
    }
}

/// Identifier for each scope that exists within the function
#[derive(Debug, PartialEq, Clone)]
pub struct ScopeId(usize);

impl ScopeId {
    pub fn new(id: usize) -> ScopeId {
        ScopeId(id)
    }

    pub fn index(&self) -> usize {
        self.0
    }
}

/// A variable declared by the user.
#[derive(Debug, PartialEq, Clone)]
pub struct VarDecl {
    /// Name of this variable
    name: StringId,
    /// Whether this variable can be mutated
    mutable: bool,
    /// The type of this variable
    ty: Type,
    /// What scope this variable was declared in
    scope: ScopeId,
}

impl VarDecl {
    pub fn new(name: StringId, mutable: bool, ty: &Type, scope: ScopeId) -> VarDecl {
        VarDecl {
            name,
            mutable,
            ty: ty.clone(),
            scope,
        }
    }
}

/// A temporary variable created by the MIR compiler to store
/// results.
#[derive(Debug, PartialEq, Clone)]
pub struct TempDecl {
    /// The type of this variable
    ty: Type,
}

impl TempDecl {
    pub fn new(ty: &Type) -> TempDecl {
        TempDecl { ty: ty.clone() }
    }
}

/// Basic Block
/// A single basic block from a CFG
#[derive(Debug, PartialEq, Clone)]
pub struct BasicBlock {
    statements: Vec<Statement>,

    /// This dictates how this basic block will terminate. This value is initially set to
    /// [`Option::None`] because the terminator may not be known at the start of the construction
    /// of this [`BasicBlock`].
    terminator: Option<Terminator>,

    /// Marks the span of input source code that this [`BasicBlock`] represents.
    /// This value is initially [`Option::None`] because the Span will be calculated
    /// as [`Statement`]s are added to the [`BasicBlock`] and, therefore, at creation
    /// the Span is not expected to be known.
    span: Option<Span>,
}

impl BasicBlock {
    pub fn new() -> BasicBlock {
        BasicBlock {
            statements: vec![],
            terminator: None,
            span: None,
        }
    }

    /// Append a new statement to this [`BasicBlock`].
    pub fn add_stm(&mut self, stm: Statement) {
        self.add_span(stm.span);
        self.statements.push(stm);
    }

    /// Set the [`Terminator`] of this [`BasicBlock`].
    pub fn set_terminator(&mut self, term: Terminator) {
        self.add_span(term.span);
        self.terminator = Some(term);
    }

    fn add_span(&mut self, span: Span) {
        // Expand the span of this basic block to cover the new statement and the
        // previous statements
        if let Some(ref mut bb_span) = self.span {
            *bb_span = Span::cover(*bb_span, span);
        } else {
            self.span = Some(span);
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
struct StatementId(usize);

/// Statement
/// A single statement, from which basic blocks are constructed
#[derive(Debug, PartialEq, Clone)]
pub struct Statement {
    kind: StatementKind,

    span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum StatementKind {
    /// This statement assigns the result of an [`RValue`] operation
    /// to the memory location represented by the [`LValue`].
    Assign(LValue, RValue),
}

/// LValue
/// A physical location in memory where a value can be stored
#[derive(Debug, PartialEq, Clone)]
pub enum LValue {
    /// A user defined variable.
    Var(VarId),

    /// A temporary variable created by the compiler during MIR construction
    Temp(VarId),

    /// Represents accessing data via some form of transformation (e.g., array index,
    /// dereferencing a raw pointer, or the field of a structure)
    Access(Box<LValue>, Accessor),

    /// Where to store the result of a procedure so that the caller can get the result.
    ReturnPointer,
}

/// Describes the method used to access the data of an indirect data type
/// such as a reference, array, or structure.
#[derive(Debug, PartialEq, Clone)]
pub enum Accessor {
    Index(u64),
    Field(StringId, Type),
    Deref,
}

/// RValue
/// An operation that results in a value which can be
/// stored in some physical location in memory
#[derive(Debug, PartialEq, Clone)]
pub enum RValue {
    /// Provides a way of reading a specific variable or using a constant in an assignment.
    Use(Operand),

    /// Represents binary insructions that are available on the CPU
    BinOp(Operand, Operand),

    /// Unary instructions that are available on the CPU
    UnOp(Operand),

    /// Casting an operand to a new type
    Cast(Operand, Type),

    /// Getting the address of a variable in memory.
    AddressOf(Operand),
}

/// Operand
/// Value that can be used as the parameters for the RValue operations
#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Constant,
    LValue(LValue),
}

/// Terminator
/// Marks the final statement in a basic block and indicates where the
/// program will go to next
#[derive(Debug, PartialEq, Clone)]
pub struct Terminator {
    kind: TerminatorKind,

    span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TerminatorKind {
    /// Enter a new functions scope.
    CallFn {
        /// The function to enter
        func: Operand,
        /// The arguments for the function being called
        args: Vec<Operand>,
        /// The location of the function result and which basic block is the reentry point from the called function
        reentry: (LValue, BasicBlockId),
    },

    /// Return from this function to the calling function.
    Return,

    /// Unconditionally, go to the given basic block
    GoTo { target: BasicBlockId },

    /// Takes a boolean value and two basic blocks. On true it will go to to the first
    /// basic block, on false it will go to to the second basic block.
    CondGoTo {
        /// The value used to determine which of the two basic blocks to go to
        cond: Operand,
        /// If `cond` is true, then go to this basic block
        tru: BasicBlockId,
        /// If `cond` is false, then go to this basic block
        fls: BasicBlockId,
    },
}

/// Binary operators
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum BinOp {
    /// '+' add two primitive numbers together
    Add,
    /// '-' subtract one primitive from another
    Sub,
    /// '*' multiply two primitive numbers
    Mul,
    /// '/' divive a primitive by another primitive
    Div,
    /// '==' compare two primitives and see if they are equal
    Eq,
    /// '!=' check if two primitives are not equal
    Ne,
    /// '<=' check if one primitive is less than or equal to another
    Le,
    /// '<' check if one primitive is less than another
    Lt,
    /// '>=' check if one primitive is greater than or equal to another
    Ge,
    /// '>' check if one primitive is greater than another
    Gt,
    /// '&' bitwise and operation on two primitives
    And,
    /// '|' bitwise or operation on two primitives
    Or,
}

/// Unary operators
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum UnOp {
    /// '-' negate a primitive value
    Negate,
    /// '!' bitwise not a primitive value
    Not,
}