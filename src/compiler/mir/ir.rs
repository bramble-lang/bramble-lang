//! The IR abstractions used to represent any given Bramble program
//! as a CFG.

use std::{fmt::Display, slice::Iter};

use crate::{
    compiler::{ast::Path, Span},
    StringId,
};

use super::{
    project::DefId,
    typetable::{FieldId, TypeId},
};

const ROOT_SCOPE: ScopeId = ScopeId(0);

/// Procedure
/// This type represents a single function from the input source code.
#[derive(Debug, PartialEq, Clone)]
pub struct Procedure {
    /// The canonical path of the procedure
    path: Path,
    /// The set of basic blocks that constitute this procedure
    blocks: Vec<BasicBlock>,
    /// The return type of this function
    ret_ty: TypeId,
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
    /// The scope tree
    scopes: ScopeTree,
}

impl Procedure {
    /// Creates a new MIR procedure. When created this will not have any
    /// basic blocks or arguments.
    pub fn new(path: &Path, args: Vec<ArgDecl>, ret_ty: TypeId, span: Span) -> Procedure {
        assert!(
            path.is_canonical(),
            "All paths must be canonical to be used in MIR"
        );

        let mut p = Procedure {
            path: path.clone(),
            blocks: vec![],
            ret_ty,
            args: vec![],
            has_varargs: false,
            vars: vec![],
            temps: vec![],
            span,
            scopes: ScopeTree::default(),
        };

        // For each argument, add it to the local variable stack
        for arg in &args {
            p.add_var(arg.name, false, arg.ty, ROOT_SCOPE, arg.span);
        }

        p.args = args;

        p
    }

    /// Creates a new MIR procedure. When created this will not have any
    /// basic blocks or arguments.
    pub fn new_extern(
        path: &Path,
        args: Vec<ArgDecl>,
        has_varargs: bool,
        ret_ty: TypeId,
        span: Span,
    ) -> Procedure {
        Procedure {
            path: path.clone(),
            blocks: vec![],
            ret_ty,
            args,
            has_varargs,
            vars: vec![],
            temps: vec![],
            span,
            scopes: ScopeTree::default(),
        }
    }

    pub fn set_span(&mut self, span: Span) {
        self.span = span;
    }

    /// Sets the [`TypeId`] of the value this function will return.
    pub fn set_ret_ty(&mut self, ty: TypeId) {
        self.ret_ty = ty;
    }

    /// Add an argument to this procedure's argument list and make the argument available as a variable.
    pub fn add_arg(&mut self, name: StringId, ty: TypeId, span: Span) -> ArgId {
        // Add the given argument to the set of variables
        self.add_var(name, false, ty, ROOT_SCOPE, span);

        // Add the argument to the argument set of the function
        let ad = ArgDecl::new(name, ty, span);
        self.args.push(ad);
        let id = self.args.len() - 1;
        ArgId::new(id)
    }

    pub fn bb_iter(&self) -> impl Iterator<Item = &BasicBlock> {
        self.blocks.iter()
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

    pub fn new_scope(&mut self, parent: ScopeId) -> ScopeId {
        self.scopes.new_scope(parent)
    }

    pub fn parent_scope(&self, id: ScopeId) -> Option<ScopeId> {
        self.scopes.parent_of(id)
    }

    /// Get the declaration details of a user defined variable in
    /// this procedure.
    pub fn get_var(&self, id: VarId) -> &VarDecl {
        &self.vars[id.index()]
    }

    /// Will return the [`VarId`] for the given variable name if it
    /// exists in the function's stack.
    pub fn find_var(&self, name: StringId, start: ScopeId) -> Option<VarId> {
        for scope in self.scopes.iter_from(start) {
            for idx in 0..self.vars.len() {
                if self.vars[idx].name == name && self.vars[idx].scope == scope {
                    return Some(VarId::new(idx));
                }
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
        ty: TypeId,
        scope: ScopeId,
        span: Span,
    ) -> VarId {
        let vd = VarDecl::new(name, mutable, ty, scope, span);
        self.vars.push(vd);
        let id = self.vars.len() - 1;

        VarId::new(id)
    }

    /// Add a new temporary variable to the procedures stack
    pub fn add_temp(&mut self, ty: TypeId, span: Span) -> TempId {
        let td = TempDecl::new(ty, span);
        self.temps.push(td);
        let id = self.temps.len() - 1;
        TempId::new(id)
    }

    /// Returns `true` if this function has a variadic argument
    pub fn has_varargs(&self) -> bool {
        self.has_varargs
    }

    /// Gets the return [type](Type) of this function.
    pub fn ret_ty(&self) -> TypeId {
        self.ret_ty
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
                "{}: {:?}, {:?} // {} {}\n",
                vid,
                self.vars[idx].ty,
                self.vars[idx].scope,
                self.vars[idx].name,
                self.vars[idx].span
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
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct ScopeId(usize);

impl ScopeId {
    pub fn root() -> ScopeId {
        ROOT_SCOPE
    }

    pub fn index(&self) -> usize {
        self.0
    }
}

/// An argument for a function.  These are always immutable and are always
/// in the root scope: therefore, there is no scope or mutable property.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct ArgDecl {
    /// Name of this variable
    name: StringId,
    /// The type of this variable
    ty: TypeId,
    /// The span of code where this variable was declared
    span: Span,
}

impl ArgDecl {
    pub fn new(name: StringId, ty: TypeId, span: Span) -> ArgDecl {
        ArgDecl { name, ty, span }
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
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct VarDecl {
    /// Name of this variable
    name: StringId,
    /// Whether this variable can be mutated
    mutable: bool,
    /// The type of this variable
    ty: TypeId,
    /// What scope this variable was declared in
    scope: ScopeId,
    /// The span of code where this variable was declared
    span: Span,
}

impl VarDecl {
    pub fn new(name: StringId, mutable: bool, ty: TypeId, scope: ScopeId, span: Span) -> VarDecl {
        VarDecl {
            name,
            mutable,
            ty,
            scope,
            span,
        }
    }

    pub fn name(&self) -> StringId {
        self.name
    }

    pub fn ty(&self) -> TypeId {
        self.ty
    }

    pub fn scope(&self) -> ScopeId {
        self.scope
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

/// A temporary variable created by the MIR compiler to store
/// results.
#[derive(Debug, PartialEq, Clone)]
pub struct TempDecl {
    /// The type of this variable
    ty: TypeId,
    /// The subexpression that this temp variable represents
    span: Span,
}

impl TempDecl {
    pub fn new(ty: TypeId, span: Span) -> TempDecl {
        TempDecl { ty, span }
    }

    pub fn ty(&self) -> TypeId {
        self.ty
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

    /// Returns an [`Iter`] over the statements in this Basic Block
    pub fn stm_iter(&self) -> Iter<Statement> {
        self.statements.iter()
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

    /// Returns the [`Span`] that this statement covers.
    pub fn span(&self) -> Span {
        self.span
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
            LValue::Access(lv, acc) => match acc {
                Accessor::Deref => format!("^{}", lv),
                _ => format!("{}{}", lv, acc),
            },
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
    Cast(Operand, TypeId),

    /// Getting the address of a variable in memory.
    AddressOf(LValue),
}

impl Display for RValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            RValue::Use(o) => format!("Use({})", o),
            RValue::BinOp(op, l, r) => format!("{}({}, {})", op, l, r),
            RValue::UnOp(op, o) => format!("{}({})", op, o),
            RValue::Cast(v, ty) => format!("{} as {:?}", v, ty),
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

impl Operand {
    /// If this is an [`Operand::LValue`] then this will extract the underlying
    /// [`LValue`]; otherwise, this will return [`None`](Option::None).
    pub fn into_lvalue(self) -> Option<LValue> {
        match self {
            Self::LValue(lv) => Some(lv),
            Self::Constant(_) => None,
        }
    }
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

#[derive(Debug, PartialEq, Copy, Clone)]
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
    SizeOf(TypeId),
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
            Constant::SizeOf(ty) => f.write_fmt(format_args!("size_of({:?})", ty)),
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

    /// Enter a new functions scope.
    CallFn {
        /// The function to enter
        func: Operand,
        /// The arguments for the function being called
        args: Vec<Operand>,
        /// The location of the function result and which basic block is the reentry point from the called function
        reentry: (LValue, BasicBlockId),
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
    /// '@' compute an offset from a given raw pointer value
    RawPointerOffset,
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
            BinOp::RawPointerOffset => "RawPointerOffset",
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

/// Stores the topology of a function's scope tree.
#[derive(Debug, Clone, PartialEq)]
struct ScopeTree {
    scopes: Vec<Option<ScopeId>>,
}

impl ScopeTree {
    /// Returns the parent of the given [`ScopeId`]. If the given scope is the root
    /// scope then it will return [`None`](Option::None).
    pub fn parent_of(&self, id: ScopeId) -> Option<ScopeId> {
        self.scopes[id.0]
    }

    /// Creates a new scope as a child of the given `parent` scope. Returns the
    /// [`ScopeId`] of the new scope.
    pub fn new_scope(&mut self, parent: ScopeId) -> ScopeId {
        let id = self.scopes.len();
        self.scopes.push(Some(parent));
        ScopeId(id)
    }

    /// Returns an iterator over the scopes from the `start` scope through its ancestors
    /// up to the root scope.
    pub fn iter_from(&self, start: ScopeId) -> ScopeIterator {
        assert!(start.0 < self.scopes.len());
        ScopeIterator {
            next: Some(start),
            tree: self,
        }
    }
}

impl Default for ScopeTree {
    fn default() -> Self {
        Self { scopes: vec![None] } // The root scope as no parent
    }
}

/// An [`Iterator`] that will traverse the ancestors of the given [scope](ScopeId) moving
/// from the initial scope, through its ancestors, to the root scope.
struct ScopeIterator<'a> {
    next: Option<ScopeId>,
    tree: &'a ScopeTree,
}

impl<'a> Iterator for ScopeIterator<'a> {
    type Item = ScopeId;

    fn next(&mut self) -> Option<Self::Item> {
        let s = self.next;
        if let Some(id) = s {
            self.next = self.tree.parent_of(id);
        }
        s
    }
}
