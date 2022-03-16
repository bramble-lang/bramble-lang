//! The IR abstractions used to represent any given Bramble program
//! as a CFG.

use std::fmt::Display;

use crate::{
    compiler::{
        ast::{Context, Node, Parameter, Path, Type},
        semantics::semanticnode::SemanticContext,
        Span,
    },
    StringId,
};

use super::{
    project::DefId,
    typetable::{FieldId, TypeId},
};

const ROOT_SCOPE: usize = 0;

/// Procedure
/// This type represents a single function from the input source code.
#[derive(Debug, PartialEq, Clone)]
pub struct Procedure {
    /// The canonical path of the procedure
    path: Path,
    /// The set of basic blocks that constitute this procedure
    blocks: Vec<BasicBlock>,
    /// The return type of this function
    ret_ty: Type,
    /// The argument list for the function
    args: Vec<ArgDecl>,
    /// Does the function have variadic arguments
    has_varargs: bool,
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
    pub fn new(path: &Path, ret_ty: &Type, span: Span) -> Procedure {
        assert!(
            path.is_canonical(),
            "All paths must be canonical to be used in MIR"
        );

        Procedure {
            path: path.clone(),
            blocks: vec![],
            ret_ty: ret_ty.clone(),
            args: vec![],
            has_varargs: false,
            vars: vec![],
            temps: vec![],
            span,
        }
    }

    /// Creates a new MIR procedure. When created this will not have any
    /// basic blocks or arguments.
    pub fn new_extern(
        path: &Path,
        params: &[Parameter<SemanticContext>],
        has_varargs: bool,
        ret_ty: &Type,
        span: Span,
    ) -> Procedure {
        // convert args into MIR args
        let args: Vec<_> = params
            .iter()
            .map(|p| ArgDecl::new(p.name, p.context().ty(), p.context().span()))
            .collect();

        Procedure {
            path: path.clone(),
            blocks: vec![],
            ret_ty: ret_ty.clone(),
            args,
            has_varargs,
            vars: vec![],
            temps: vec![],
            span,
        }
    }

    pub fn set_span(&mut self, span: Span) {
        self.span = span;
    }

    /// Add an argument to this procedure's argument list and make the argument available as a variable.
    pub fn add_arg(&mut self, name: StringId, ty: &Type, span: Span) -> ArgId {
        // Add the given argument to the set of variables
        self.add_var(name, false, ty, ScopeId::new(0), span);

        // Add the argument to the argument set of the function
        let ad = ArgDecl::new(name, ty, span);
        self.args.push(ad);
        let id = self.args.len() - 1;
        ArgId::new(id)
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

    /// Will return the [`VarId`] for the given variable name if it
    /// exists in the function's stack.
    pub fn find_var(&self, name: StringId) -> Option<VarId> {
        for idx in 0..self.vars.len() {
            if self.vars[idx].name == name {
                return Some(VarId::new(idx));
            }
        }

        None
    }

    /// Get the declaration details of a temporary variable.
    pub fn get_temp(&self, id: TempId) -> &TempDecl {
        &self.temps[id.index()]
    }

    /// Add a new user defined variable to the procedure.
    pub fn add_var(
        &mut self,
        name: StringId,
        mutable: bool,
        ty: &Type,
        scope: ScopeId,
        span: Span,
    ) -> VarId {
        let vd = VarDecl::new(name, mutable, ty, scope, span);
        self.vars.push(vd);
        let id = self.vars.len() - 1;

        VarId::new(id)
    }

    /// Add a new temporary variable to the procedures stack
    pub fn add_temp(&mut self, ty: &Type, span: Span) -> TempId {
        let td = TempDecl::new(ty, span);
        self.temps.push(td);
        let id = self.temps.len() - 1;
        TempId::new(id)
    }

    /// Gets the return [type](Type) of this function.
    pub fn ret_ty(&self) -> &Type {
        &self.ret_ty
    }

    /// Returns a reference to the canonical [`path`](Path) of this procedure
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Returns the number of [`BasicBlocks`](BasicBlock) in the procedure
    pub fn len(&self) -> usize {
        self.blocks.len()
    }
}

impl Display for Procedure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("fn {} () -> {:?}:\n", self.path, self.ret_ty))?;

        // Print the arguments
        f.write_str("Arguments: \n")?;

        // Print out the variables
        for idx in 0..self.args.len() {
            f.write_str("arg ")?;

            f.write_fmt(format_args!(
                "{}: {:?} // {} {}\n",
                idx, self.args[idx].ty, self.args[idx].name, self.args[idx].span
            ))?
        }

        if self.has_varargs {
            f.write_str("Is Variadic\n")?;
        }

        f.write_str("\n")?;

        // Print out the stack: vars and temps
        f.write_str("Stack:\n")?;

        // Print out the variables
        for idx in 0..self.vars.len() {
            let vid = VarId::new(idx);
            f.write_str("let ")?;

            if self.vars[idx].mutable {
                f.write_str("mut ")?
            }

            f.write_fmt(format_args!(
                "{}: {:?} // {} {}\n",
                vid, self.vars[idx].ty, self.vars[idx].name, self.vars[idx].span
            ))?
        }
        f.write_str("\n")?;

        // Print the temporary variables
        for idx in 0..self.temps.len() {
            let tid = TempId::new(idx);
            f.write_fmt(format_args!(
                "let mut {}: {:?} // {}\n",
                tid, self.temps[idx].ty, self.temps[idx].span
            ))?
        }

        // Print all the basic blocks
        for bb in 0..self.blocks.len() {
            f.write_str("\n")?;
            f.write_fmt(format_args!("BB{}: \n", bb))?;
            f.write_fmt(format_args!("{}", self.blocks[bb]))?
        }

        Ok(())
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

impl Display for BasicBlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("BB{}", self.0))
    }
}

/// Identifier for a user declared variable
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct VarId(usize);

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
        f.write_fmt(format_args!("%{}", self.0))
    }
}

/// Identifier for a temporary variable.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct TempId(usize);

impl TempId {
    pub fn new(id: usize) -> TempId {
        TempId(id)
    }

    pub fn index(&self) -> usize {
        self.0
    }
}

impl Display for TempId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("%%{}", self.0))
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

/// An argument for a function.  These are always immutable and are always
/// in the root scope: therefore, there is no scope or mutable property.
#[derive(Debug, PartialEq, Clone)]
pub struct ArgDecl {
    /// Name of this variable
    name: StringId,
    /// The type of this variable
    ty: Type,
    /// The span of code where this variable was declared
    span: Span,
}

impl ArgDecl {
    fn new(name: StringId, ty: &Type, span: Span) -> ArgDecl {
        ArgDecl {
            name,
            ty: ty.clone(),
            span,
        }
    }
}

/// Unique identifier for an argument within the MIR of a function
pub struct ArgId(u32);

impl ArgId {
    fn new(id: usize) -> ArgId {
        ArgId(id as u32)
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
    /// The span of code where this variable was declared
    span: Span,
}

impl VarDecl {
    pub fn new(name: StringId, mutable: bool, ty: &Type, scope: ScopeId, span: Span) -> VarDecl {
        VarDecl {
            name,
            mutable,
            ty: ty.clone(),
            scope,
            span,
        }
    }
}

/// A temporary variable created by the MIR compiler to store
/// results.
#[derive(Debug, PartialEq, Clone)]
pub struct TempDecl {
    /// The type of this variable
    ty: Type,
    /// The subexpression that this temp variable represents
    span: Span,
}

impl TempDecl {
    pub fn new(ty: &Type, span: Span) -> TempDecl {
        TempDecl {
            ty: ty.clone(),
            span,
        }
    }

    pub fn ty(&self) -> &Type {
        &self.ty
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

    /// Get the [`Statement`] at the given index
    pub fn get_stm(&self, idx: usize) -> &Statement {
        &self.statements[idx]
    }

    /// Get the [`Terminator`] for this basic block
    pub fn get_term(&self) -> Option<&Terminator> {
        self.terminator.as_ref()
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

    /// Returns the number of [`Statements`](Statement) in this basic block
    pub fn len(&self) -> usize {
        self.statements.len()
    }
}

impl Display for BasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stm in &self.statements {
            f.write_fmt(format_args!("{}  // {}\n", stm, stm.span))?
        }
        match &self.terminator {
            Some(term) => f.write_fmt(format_args!("{} // {}\n", term, term.span)),
            None => f.write_fmt(format_args!("MISSING TERMINATOR\n")),
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

impl Statement {
    /// Creates a new statement that can be added to a [`BasicBlock`].
    pub fn new(kind: StatementKind, span: Span) -> Statement {
        Statement { kind, span }
    }

    /// Returns the [`StatementKind`] of this statement.
    pub fn kind(&self) -> &StatementKind {
        &self.kind
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.kind))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum StatementKind {
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
pub enum LValue {
    /// A static location in the program's memory space
    Static(DefId),

    /// A user defined variable.
    Var(VarId),

    /// A temporary variable created by the compiler during MIR construction
    Temp(TempId),

    /// Represents accessing data via some form of transformation (e.g., array index,
    /// dereferencing a raw pointer, or the field of a structure)
    Access(Box<LValue>, Accessor),

    /// Where to store the result of a procedure so that the caller can get the result.
    ReturnPointer,
}

impl Display for LValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            LValue::Static(s) => format!("Static({})", s),
            LValue::Var(v) => format!("{}", v),
            LValue::Temp(t) => format!("{}", t),
            LValue::Access(lv, acc) => format!("{}{}", lv, acc),
            LValue::ReturnPointer => "ReturnPtr".into(),
        };
        f.write_str(&text)
    }
}

/// Describes the method used to access the data of an indirect data type
/// such as a reference, array, or structure.
#[derive(Debug, PartialEq, Clone)]
pub enum Accessor {
    Index(Box<Operand>),
    Field(FieldId, TypeId),
    Deref,
}

impl Display for Accessor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            Accessor::Index(i) => format!("[{}]", i),
            Accessor::Field(f, _) => format!(".{}", f),
            Accessor::Deref => "Deref()".into(),
        };
        f.write_str(&text)
    }
}

/// RValue
/// An operation that results in a value which can be
/// stored in some physical location in memory
#[derive(Debug, PartialEq, Clone)]
pub enum RValue {
    /// Provides a way of reading a specific variable or using a constant in an assignment.
    Use(Operand),

    /// Represents binary insructions that are available on the CPU
    BinOp(BinOp, Operand, Operand),

    /// Unary instructions that are available on the CPU
    UnOp(UnOp, Operand),

    /// Casting an operand to a new type
    Cast(Operand, Type),

    /// Getting the address of a variable in memory.
    AddressOf(LValue),
}

impl Display for RValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            RValue::Use(o) => format!("Use({})", o),
            RValue::BinOp(op, l, r) => format!("{}({}, {})", op, l, r),
            RValue::UnOp(op, o) => format!("{}({})", op, o),
            RValue::Cast(v, t) => format!("{} as {:?}", v, t),
            RValue::AddressOf(o) => format!("AddressOf({})", o),
        };
        f.write_str(&text)
    }
}

/// Operand
/// Value that can be used as the parameters for the RValue operations
#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Constant(Constant),
    LValue(LValue),
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            Operand::Constant(c) => format!("{}", c),
            Operand::LValue(lv) => format!("{}", lv),
        };
        f.write_str(&text)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Constant {
    Unit,
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    F64(f64),
    Bool(bool),
    StringLiteral(StringId),
    Null,
}

impl Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Unit => f.write_str("()"),
            Constant::Null => f.write_str("null"),
            Constant::I8(i) => f.write_fmt(format_args!("{}i8", i)),
            Constant::I16(i) => f.write_fmt(format_args!("{}i16", i)),
            Constant::I32(i) => f.write_fmt(format_args!("{}i32", i)),
            Constant::I64(i) => f.write_fmt(format_args!("{}i64", i)),
            Constant::U8(i) => f.write_fmt(format_args!("{}u8", i)),
            Constant::U16(i) => f.write_fmt(format_args!("{}u16", i)),
            Constant::U32(i) => f.write_fmt(format_args!("{}u32", i)),
            Constant::U64(i) => f.write_fmt(format_args!("{}u64", i)),
            Constant::F64(v) => f.write_fmt(format_args!("{}f64", v)),
            Constant::Bool(b) => f.write_fmt(format_args!("{}", b)),
            Constant::StringLiteral(sid) => f.write_fmt(format_args!("{}", sid)),
        }
    }
}

/// Terminator
/// Marks the final statement in a basic block and indicates where the
/// program will go to next
#[derive(Debug, PartialEq, Clone)]
pub struct Terminator {
    kind: TerminatorKind,

    span: Span,
}

impl Terminator {
    pub fn new(kind: TerminatorKind, span: Span) -> Terminator {
        Terminator { kind, span }
    }

    /// Returns the [`TerminatorKind`] of this terminator
    pub fn kind(&self) -> &TerminatorKind {
        &self.kind
    }
}

impl Display for Terminator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.kind))
    }
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

impl Display for TerminatorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            TerminatorKind::CallFn {
                func,
                args,
                reentry,
            } => format!(
                "{} := call {} ({:?}); goto {}",
                reentry.0, func, args, reentry.1
            ),
            TerminatorKind::Return => "return".into(),
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

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let txt = match self {
            BinOp::Add => "Add",
            BinOp::Sub => "Sub",
            BinOp::Mul => "Mul",
            BinOp::Div => "Div",
            BinOp::Eq => "Eq",
            BinOp::Ne => "Neq",
            BinOp::Le => "Le",
            BinOp::Lt => "Lt",
            BinOp::Ge => "Ge",
            BinOp::Gt => "Gt",
            BinOp::And => "BitwiseAnd",
            BinOp::Or => "BitwiseOr",
        };
        f.write_str(txt)
    }
}

/// Unary operators
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum UnOp {
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
