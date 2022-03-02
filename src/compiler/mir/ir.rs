//! The IR abstractions used to represent any given Bramble program
//! as a CFG.

use std::fmt::{Display, Write};

use crate::{
    compiler::{ast::Type, Span},
    StringId,
};

const ROOT_SCOPE: usize = 0;

/// Procedure
/// This type represents a single function from the input source code.
#[derive(Debug, PartialEq, Clone)]
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

impl Display for Procedure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("fn () -> {:?}:\n", self.ret_ty))?;

        // Print out the variables
        for idx in 0..self.vars.len() {
            if self.vars[idx].mutable {
                f.write_fmt(format_args!("let mut %{}: {:?} // {}\n", idx, self.vars[idx].ty, self.vars[idx].name))?
            } else {
                f.write_fmt(format_args!("let %{}: {:?} // {}\n", idx, self.vars[idx].ty, self.vars[idx].name))?
            }
        }
        
        // Print the temporary variables
        for idx in 0..self.temps.len() {
            f.write_fmt(format_args!("let mut %_{}: {:?}\n", idx, self.temps[idx].ty))?
        }

        // Print all the basic blocks
        for bb in 0..self.blocks.len() {
            f.write_fmt(format_args!("BB{}: \n", bb))?;
            f.write_fmt(format_args!("{}", self.blocks[bb]))?
        }

        Ok(())
    }
}

/// Identifier for a specific basic block in a procedure
#[derive(Debug, PartialEq, Copy, Clone)]
struct BasicBlockId(usize);

impl BasicBlockId {
    pub fn new(id: usize) -> BasicBlockId {
        BasicBlockId(id)
    }

    pub fn index(&self) -> usize {
        self.0
    }
}

impl Display for BasicBlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("BB{}", self.0))
    }
}

/// Identifier for a user declared variable
#[derive(Debug, PartialEq, Clone)]
struct VarId(usize);

impl VarId {
    pub fn new(id: usize) -> VarId {
        VarId(id)
    }

    pub fn index(&self) -> usize {
        self.0
    }
}

impl Display for VarId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}

/// Identifier for a temporary variable.
#[derive(Debug, PartialEq, Clone)]
struct TempId(usize);

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
struct ScopeId(usize);

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
struct VarDecl {
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
struct TempDecl {
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
struct BasicBlock {
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

impl Display for BasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stm in &self.statements {
            f.write_fmt(format_args!("{}\n", stm))?
        }
        match &self.terminator {
            Some(term) => f.write_fmt(format_args!("{}\n", term)),
            None => f.write_fmt(format_args!("MISSING TERMINATOR\n")),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
struct StatementId(usize);

/// Statement
/// A single statement, from which basic blocks are constructed
#[derive(Debug, PartialEq, Clone)]
struct Statement {
    kind: StatementKind,

    span: Span,
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.kind))
    }
}

#[derive(Debug, PartialEq, Clone)]
enum StatementKind {
    /// This statement assigns the result of an [`RValue`] operation
    /// to the memory location represented by the [`LValue`].
    Assign(LValue, RValue),
}

impl Display for StatementKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StatementKind::Assign(l, r) => f.write_fmt(format_args!("{} := {}", l, r)),
        }
    }
}

/// LValue
/// A physical location in memory where a value can be stored
#[derive(Debug, PartialEq, Clone)]
enum LValue {
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

impl Display for LValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            LValue::Var(v) => format!("%{}", v),
            LValue::Temp(t) => format!("%_{}", t),
            LValue::Access(lv, acc) => format!("{}{}", lv, acc),
            LValue::ReturnPointer => format!("ReturnPtr"),
        };
        f.write_str(&text)
    }
}

/// Describes the method used to access the data of an indirect data type
/// such as a reference, array, or structure.
#[derive(Debug, PartialEq, Clone)]
enum Accessor {
    Index(u64),
    Field(StringId, Type),
    Deref,
}

impl Display for Accessor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            Accessor::Index(i) => format!("[{}]", i),
            Accessor::Field(f, _) => format!(".{}", f),
            Accessor::Deref => format!("Deref()"),
        };
        f.write_str(&text)
    }
}

/// RValue
/// An operation that results in a value which can be
/// stored in some physical location in memory
#[derive(Debug, PartialEq, Clone)]
enum RValue {
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

impl Display for RValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            RValue::Use(o) => format!("Use({})", o),
            RValue::BinOp(l, r) => format!("({}, {})", l, r),
            RValue::UnOp(o) => format!("({})", o),
            RValue::Cast(v, t) => format!("{} as {:?}", v, t),
            RValue::AddressOf(o) => format!("AddressOf({})", o),
        };
        f.write_str(&text)
    }
}

/// Operand
/// Value that can be used as the parameters for the RValue operations
#[derive(Debug, PartialEq, Clone)]
enum Operand {
    Constant,
    LValue(LValue),
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            Operand::Constant => format!("constant"),
            Operand::LValue(lv) => format!("{}", lv),
        };
        f.write_str(&text)
    }
}

/// Terminator
/// Marks the final statement in a basic block and indicates where the
/// program will go to next
#[derive(Debug, PartialEq, Clone)]
struct Terminator {
    kind: TerminatorKind,

    span: Span,
}

impl Display for Terminator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("term {}\n", self.kind))
    }
}

#[derive(Debug, PartialEq, Clone)]
enum TerminatorKind {
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

impl Display for TerminatorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            TerminatorKind::CallFn {
                func,
                args,
                reentry,
            } => format!("call {}", func),
            TerminatorKind::Return => format!("return"),
            TerminatorKind::GoTo { target } => format!("goto {}", target),
            TerminatorKind::CondGoTo { cond, tru, fls } => {
                format!("if ({}) then {} else {}", cond, tru, fls)
            }
        };
        f.write_str(&text)
    }
}

/// Binary operators
#[derive(Debug, PartialEq, Copy, Clone)]
enum BinOp {
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

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let txt = match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Eq => "==",
            BinOp::Ne => "!=",
            BinOp::Le => "<=",
            BinOp::Lt => "<",
            BinOp::Ge => ">=",
            BinOp::Gt => ">",
            BinOp::And => "&",
            BinOp::Or => "|",
        };
        f.write_str(txt)
    }
}

/// Unary operators
#[derive(Debug, PartialEq, Copy, Clone)]
enum UnOp {
    /// '-' negate a primitive value
    Negate,
    /// '!' bitwise not a primitive value
    Not,
}

impl Display for UnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let txt = match self {
            UnOp::Negate => "-",
            UnOp::Not => "!",
        };
        f.write_str(txt)
    }
}
