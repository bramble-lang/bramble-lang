use log::debug;

use crate::{
    compiler::{ast::Path, Span},
    StringId,
};

use super::{ir::*, typetable::*};

/// Provides a Builder interface for constructing the MIR CFG representation of a
/// routine. This will keep track of the current [`BasicBlock`] and make sure that
/// MIR operations are applied to that [`BasicBlock`]. This also provides a simplfied
/// interface for constructing the MIR operands, operations, and statements, to
/// simplify the code that traverses input ASTs and transforms them into MIR.
pub struct MirProcedureBuilder {
    /// The [`Procedure`] being built
    proc: Procedure,
    /// All MIR elements will be added to this [`BasicBlock`].
    current_bb: Option<BasicBlockId>,
    /// All variables will be added to this scope.
    current_scope: ScopeId,
}

impl MirProcedureBuilder {
    /// Creates a new [`MirBuilder`], which is used to construct the MIR representation
    /// of a function.
    pub fn new(path: &Path, ret_ty: TypeId) -> MirProcedureBuilder {
        MirProcedureBuilder {
            proc: Procedure::new(path, vec![], ret_ty, Span::zero()),
            current_bb: None,
            current_scope: ScopeId::root(),
        }
    }

    /// Finalizes the construction of a procedure and returns the MIR Procedure value.
    pub fn complete(self) -> Procedure {
        self.proc
    }

    /// Sets the span of input source code that this [`Procedure`] will represent.
    pub fn set_span(&mut self, span: Span) {
        self.proc.set_span(span)
    }

    /// Add a new [`BasicBlock`] to this function.
    pub fn new_bb(&mut self) -> BasicBlockId {
        self.proc.new_bb()
    }

    /// Change the active [`BasicBlock`]. After this call, all instructions added
    /// to the function will be appended to the [`BasicBlock`] specified by `bb`.
    pub fn set_bb(&mut self, bb: BasicBlockId) {
        self.current_bb = Some(bb)
    }

    /// Create a new scope that's the child of the current scope and make it
    /// the current scope.
    pub fn start_scope(&mut self) {
        self.current_scope = self.proc.new_scope(self.current_scope);
    }

    /// Close the current scope and move up to its parent scope. Closing the root
    /// scope will have no effect.
    pub fn close_scope(&mut self) {
        self.current_scope = self
            .proc
            .parent_scope(self.current_scope)
            .unwrap_or_else(ScopeId::root);
    }

    /// Search the procedure's set of local variables for the variable with
    /// the matching name that's in the nearest scope.
    pub fn find_var(&self, name: StringId) -> Option<VarId> {
        self.proc.find_var(name, self.current_scope)
    }

    /// Create an [`i8`] constant
    pub fn const_i8(&self, i: i8) -> Operand {
        Operand::Constant(Constant::I8(i))
    }

    /// Create an [`i16`] constant
    pub fn const_i16(&self, i: i16) -> Operand {
        Operand::Constant(Constant::I16(i))
    }

    /// Create an [`i32`] constant
    pub fn const_i32(&self, i: i32) -> Operand {
        Operand::Constant(Constant::I32(i))
    }

    /// Create an [`i64`] constant
    pub fn const_i64(&self, i: i64) -> Operand {
        Operand::Constant(Constant::I64(i))
    }

    /// Create a [`u8`] constant
    pub fn const_u8(&self, i: u8) -> Operand {
        Operand::Constant(Constant::U8(i))
    }

    /// Create a [`u16`] constant
    pub fn const_u16(&self, i: u16) -> Operand {
        Operand::Constant(Constant::U16(i))
    }

    /// Create a [`u32`] constant
    pub fn const_u32(&self, i: u32) -> Operand {
        Operand::Constant(Constant::U32(i))
    }

    /// Create a [`u64`] constant
    pub fn const_u64(&self, i: u64) -> Operand {
        Operand::Constant(Constant::U64(i))
    }

    /// Create an [`f64`] constant
    pub fn const_f64(&self, f: f64) -> Operand {
        Operand::Constant(Constant::F64(f))
    }

    /// Create a [`bool`] constant
    pub fn const_bool(&self, b: bool) -> Operand {
        Operand::Constant(Constant::Bool(b))
    }

    /// Create a reference to a string literal
    pub fn const_stringliteral(&self, s: StringId) -> Operand {
        Operand::Constant(Constant::StringLiteral(s))
    }

    /// Create a `null` value
    pub fn const_null(&self) -> Operand {
        Operand::Constant(Constant::Null)
    }

    /// Create a constant value of the size of the given type
    pub fn size_of(&self, ty: TypeId) -> Operand {
        Operand::Constant(Constant::SizeOf(ty))
    }

    /// Sets the [`TypeId`] of the value this function will return.
    pub fn set_ret_ty(&mut self, ty: TypeId) {
        self.proc.set_ret_ty(ty)
    }

    /// Add an argument to the signature of a function. Arguments are also added to the
    /// set of variables.
    pub fn arg(&mut self, name: StringId, ty: TypeId, span: Span) -> ArgId {
        self.proc.add_arg(name, ty, span)
    }

    /// Add a new user declared variable to this function's stack
    pub fn var(&mut self, name: StringId, mutable: bool, ty: TypeId, span: Span) -> VarId {
        self.proc
            .add_var(name, mutable, ty, self.current_scope, span)
    }

    /// Add a new temporary variable to this function's stack
    pub fn temp(&mut self, ty: TypeId, span: Span) -> TempId {
        self.proc.add_temp(ty, span)
    }

    /// Create a new temporary variable and store the [`RValue`] in it.
    pub fn temp_store(&mut self, rv: RValue, ty: TypeId, span: Span) -> Operand {
        let tv = LValue::Temp(self.temp(ty, span));
        debug!("Temp store: {:?} := {:?}", tv, rv);

        self.store(tv.clone(), rv, span);

        Operand::LValue(tv)
    }

    /// Store the given [`RValue`] in the location specified by the given
    /// [`LValue`].
    pub fn store(&mut self, lv: LValue, rv: RValue, span: Span) {
        debug!("Store: {:?} := {:?}", lv, rv);

        let cid = self.current_bb.unwrap();
        let bb = self.proc.get_bb_mut(cid);
        bb.add_stm(super::ir::Statement::new(
            StatementKind::Assign(lv, rv),
            span,
        ));
    }

    /// Will construct an [`LValue`] whose location is the address stored in `right`.
    pub fn deref_rawpointer(&self, right: LValue) -> LValue {
        debug!("Deref Raw: {:?}", right);
        LValue::Access(Box::new(right), Accessor::Deref)
    }

    /// Will construct an [`LValue`] whose location is the specified `field` in a given
    /// strucure type. This expects `ty` to be a [`MirTypeDef::Structure`].
    pub fn member_access(&self, base: LValue, def: &MirStructDef, field: StringId) -> LValue {
        debug!("Member Access: {:?}.{:?}", base, def);

        let (field_id, field_mir) = def
            .find_field(field)
            .expect("Could not find field in structure");

        LValue::Access(Box::new(base), Accessor::Field(field_id, field_mir.ty))
    }

    pub fn array_at(&self, array: LValue, index: Operand) -> LValue {
        debug!("Array At: {:?}[{:?}]", array, index);

        LValue::Access(Box::new(array), Accessor::Index(Box::new(index)))
    }

    /// Add a boolean not to the current [`BasicBlock`].
    pub fn not(&self, right: Operand) -> RValue {
        debug!("Not: {:?}", right);

        RValue::UnOp(UnOp::Not, right)
    }

    /// Add a negate to the current [`BasicBlock`].
    pub fn negate(&self, right: Operand) -> RValue {
        debug!("Negate: {:?}", right);
        RValue::UnOp(UnOp::Negate, right)
    }

    /// Add an addition operation to the current [`BasicBlock`].
    pub fn add(&self, left: Operand, right: Operand) -> RValue {
        debug!("Add: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::Add, left, right)
    }

    /// Add a subtraction operation to the current [`BasicBlock`].
    pub fn sub(&self, left: Operand, right: Operand) -> RValue {
        debug!("Sub: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::Sub, left, right)
    }

    /// Add a multiply operation to the current [`BasicBlock`].
    pub fn mul(&self, left: Operand, right: Operand) -> RValue {
        debug!("Mul: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::Mul, left, right)
    }

    /// Add a divide operation to the current [`BasicBlock`].
    pub fn div(&self, left: Operand, right: Operand) -> RValue {
        debug!("Div: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::SIDiv, left, right)
    }

    /// Add a unsigned integer divide operation to the current [`BasicBlock`].
    pub fn ui_div(&self, left: Operand, right: Operand) -> RValue {
        debug!("UIDiv: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::UIDiv, left, right)
    }

    /// Add a bitwise and operation to the current [`BasicBlock`].
    pub fn bitwise_and(&self, left: Operand, right: Operand) -> RValue {
        debug!("And: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::And, left, right)
    }

    /// Add a bitwise and operation to the current [`BasicBlock`].
    pub fn bitwise_or(&self, left: Operand, right: Operand) -> RValue {
        debug!("Or: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::Or, left, right)
    }

    /// Add an equality test operation to the current [`BasicBlock`].
    pub fn eq(&self, left: Operand, right: Operand) -> RValue {
        debug!("Eq: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::Eq, left, right)
    }

    /// Add a not equal test operation to the current [`BasicBlock`].
    pub fn neq(&self, left: Operand, right: Operand) -> RValue {
        debug!("Neq: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::Ne, left, right)
    }

    /// Add a less than test operation to the current [`BasicBlock`].
    pub fn lt(&self, left: Operand, right: Operand) -> RValue {
        debug!("Less Than");
        debug!("Add: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::Lt, left, right)
    }

    /// Add a less than or equal to test operation to the current [`BasicBlock`].
    pub fn le(&self, left: Operand, right: Operand) -> RValue {
        debug!("Less or Equal: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::Le, left, right)
    }

    /// Add a greater than test operation to the current [`BasicBlock`].
    pub fn gt(&self, left: Operand, right: Operand) -> RValue {
        debug!("Greater: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::Gt, left, right)
    }

    /// Add a greater than or equal to test operation to the current [`BasicBlock`].
    pub fn ge(&self, left: Operand, right: Operand) -> RValue {
        debug!("Greater or Equal: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::Ge, left, right)
    }

    /// Add a raw pointer offset operation to the current [`BasicBlock`].
    pub fn offset(&self, left: Operand, right: Operand) -> RValue {
        debug!("Pointer Offset: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::RawPointerOffset, left, right)
    }

    /// Cast the given expression to the given primitive.
    pub fn cast(&self, expr: Operand, target: TypeId) -> RValue {
        debug!("Cast: {:?}, {:?}", expr, target);
        RValue::Cast(expr, target)
    }

    /// Terminates by returning to the caller function
    pub fn term_return(&mut self, span: Span) {
        debug!("Terminator: Return");
        let cid = self.current_bb.unwrap();
        let bb = self.proc.get_bb_mut(cid);
        bb.set_terminator(Terminator::new(TerminatorKind::Return, span));
    }

    /// Terminates by going to the destination basic block
    pub fn term_goto(&mut self, target: BasicBlockId, span: Span) {
        debug!("Goto: {:?}", target);
        let cid = self.current_bb.unwrap();
        let bb = self.proc.get_bb_mut(cid);
        bb.set_terminator(Terminator::new(TerminatorKind::GoTo { target }, span))
    }

    /// Terminates with a conditional go to
    pub fn term_cond_goto(
        &mut self,
        cond: Operand,
        then_bb: BasicBlockId,
        else_bb: BasicBlockId,
        span: Span,
    ) {
        debug!("If {:?} then {:?} else {:?}", cond, then_bb, else_bb);
        let cid = self.current_bb.unwrap();
        let bb = self.proc.get_bb_mut(cid);
        bb.set_terminator(Terminator::new(
            TerminatorKind::CondGoTo {
                cond,
                tru: then_bb,
                fls: else_bb,
            },
            span,
        ));
    }

    /// Terminates by calling the given function
    pub fn term_call(
        &mut self,
        func: Operand,
        args: &[Operand],
        reentry: (LValue, BasicBlockId),
        span: Span,
    ) {
        debug!(
            "Function Call: call({:?}) with {:?} returning to {:?}",
            func, args, reentry
        );

        let cid = self
            .current_bb
            .expect("Cannot set terminator when there is no current BasicBlock");

        // The re-entry BB ID must come after the ID of the BB making the function call
        // Providing this invariant makes transformation operations on the MIR easier.
        assert!(
            cid < reentry.1,
            "The ID of the re-entry BasicBlock must be greater than the caller BasicBlock"
        );

        let bb = self.proc.get_bb_mut(cid);
        bb.set_terminator(Terminator::new(
            TerminatorKind::CallFn {
                func,
                args: args.into(),
                reentry,
            },
            span,
        ))
    }
}
