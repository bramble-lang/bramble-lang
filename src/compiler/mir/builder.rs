use log::debug;

use crate::{
    compiler::{
        ast::{Path, Type},
        Span,
    },
    StringId,
};

use super::{ir::*, typetable::*};

/// Provides a Builder interface for constructing the MIR CFG representation of a
/// routine. This will keep track of the current [`BasicBlock`] and make sure that
/// MIR operations are applied to that [`BasicBlock`]. This also provides a simplfied
/// interface for constructing the MIR operands, operations, and statements, to
/// simplify the code that traverses input ASTs and transforms them into MIR.
pub struct MirProcedureBuilder {
    proc: Procedure,
    current_bb: Option<BasicBlockId>,
}

impl MirProcedureBuilder {
    /// Creates a new [`MirBuilder`], which is used to construct the MIR representation
    /// of a function.
    pub fn new(path: &Path) -> MirProcedureBuilder {
        MirProcedureBuilder {
            proc: Procedure::new(path, &Type::Unit, Span::zero()),
            current_bb: None,
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

    pub fn find_var(&self, name: StringId) -> Option<VarId> {
        self.proc.find_var(name)
    }

    /// Create an [`i8`] constant
    pub fn const_i8(&mut self, i: i8) -> Operand {
        Operand::Constant(Constant::I8(i))
    }

    /// Create an [`i16`] constant
    pub fn const_i16(&mut self, i: i16) -> Operand {
        Operand::Constant(Constant::I16(i))
    }

    /// Create an [`i32`] constant
    pub fn const_i32(&mut self, i: i32) -> Operand {
        Operand::Constant(Constant::I32(i))
    }

    /// Create an [`i64`] constant
    pub fn const_i64(&mut self, i: i64) -> Operand {
        Operand::Constant(Constant::I64(i))
    }

    /// Create a [`u8`] constant
    pub fn const_u8(&mut self, i: u8) -> Operand {
        Operand::Constant(Constant::U8(i))
    }

    /// Create a [`u16`] constant
    pub fn const_u16(&mut self, i: u16) -> Operand {
        Operand::Constant(Constant::U16(i))
    }

    /// Create a [`u32`] constant
    pub fn const_u32(&mut self, i: u32) -> Operand {
        Operand::Constant(Constant::U32(i))
    }

    /// Create a [`u64`] constant
    pub fn const_u64(&mut self, i: u64) -> Operand {
        Operand::Constant(Constant::U64(i))
    }

    /// Create an [`f64`] constant
    pub fn const_f64(&mut self, f: f64) -> Operand {
        Operand::Constant(Constant::F64(f))
    }

    /// Create a [`bool`] constant
    pub fn const_bool(&mut self, b: bool) -> Operand {
        Operand::Constant(Constant::Bool(b))
    }

    /// Create a reference to a string literal
    pub fn const_stringliteral(&mut self, s: StringId) -> Operand {
        Operand::Constant(Constant::StringLiteral(s))
    }

    /// Create a `null` value
    pub fn const_null(&mut self) -> Operand {
        Operand::Constant(Constant::Null)
    }

    /// Add an argument to the signature of a function. Arguments are also added to the
    /// set of variables.
    pub fn arg(&mut self, name: StringId, ty: &Type, span: Span) -> ArgId {
        self.proc.add_arg(name, ty, span)
    }

    /// Add a new user declared variable to this function's stack
    pub fn var(&mut self, name: StringId, mutable: bool, ty: &Type, span: Span) -> VarId {
        self.proc.add_var(name, mutable, ty, ScopeId::new(0), span)
    }

    /// Add a new temporary variable to this function's stack
    pub fn temp(&mut self, ty: &Type, span: Span) -> TempId {
        self.proc.add_temp(ty, span)
    }

    /// Create a new temporary variable and store the [`RValue`] in it.
    pub fn temp_store(&mut self, rv: RValue, ty: &Type, span: Span) -> Operand {
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

    /// Will construct an [`LValue`] whose location is the specified `field` in a given
    /// strucure type. This expects `ty` to be a [`MirTypeDef::Structure`].
    pub fn member_access(&mut self, base: LValue, def: &MirStructDef, field: StringId) -> LValue {
        debug!("Member Access: {:?}.{:?}", base, def);

        let (field_id, field_mir) = def
            .find_field(field)
            .expect("Could not find field in structure");

        LValue::Access(Box::new(base), Accessor::Field(field_id, field_mir.ty))
    }

    pub fn array_at(&mut self, array: LValue, index: Operand) -> LValue {
        debug!("Array At: {:?}[{:?}]", array, index);

        LValue::Access(Box::new(array), Accessor::Index(Box::new(index)))
    }

    /// Add a boolean not to the current [`BasicBlock`].
    pub fn not(&mut self, right: Operand) -> RValue {
        debug!("Not: {:?}", right);

        RValue::UnOp(UnOp::Not, right)
    }

    /// Add a negate to the current [`BasicBlock`].
    pub fn negate(&mut self, right: Operand) -> RValue {
        debug!("Negate: {:?}", right);
        RValue::UnOp(UnOp::Negate, right)
    }

    /// Add an addition operation to the current [`BasicBlock`].
    pub fn add(&mut self, left: Operand, right: Operand) -> RValue {
        debug!("Add: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::Add, left, right)
    }

    /// Add a subtraction operation to the current [`BasicBlock`].
    pub fn sub(&mut self, left: Operand, right: Operand) -> RValue {
        debug!("Sub: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::Sub, left, right)
    }

    /// Add a multiply operation to the current [`BasicBlock`].
    pub fn mul(&mut self, left: Operand, right: Operand) -> RValue {
        debug!("Mul: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::Mul, left, right)
    }

    /// Add a divide operation to the current [`BasicBlock`].
    pub fn div(&mut self, left: Operand, right: Operand) -> RValue {
        debug!("Div: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::Div, left, right)
    }

    /// Add a bitwise and operation to the current [`BasicBlock`].
    pub fn bitwise_and(&mut self, left: Operand, right: Operand) -> RValue {
        debug!("And: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::And, left, right)
    }

    /// Add a bitwise and operation to the current [`BasicBlock`].
    pub fn bitwise_or(&mut self, left: Operand, right: Operand) -> RValue {
        debug!("Or: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::Or, left, right)
    }

    /// Add an equality test operation to the current [`BasicBlock`].
    pub fn eq(&mut self, left: Operand, right: Operand) -> RValue {
        debug!("Eq: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::Eq, left, right)
    }

    /// Add a not equal test operation to the current [`BasicBlock`].
    pub fn neq(&mut self, left: Operand, right: Operand) -> RValue {
        debug!("Neq: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::Ne, left, right)
    }

    /// Add a less than test operation to the current [`BasicBlock`].
    pub fn lt(&mut self, left: Operand, right: Operand) -> RValue {
        debug!("Less Than");
        debug!("Add: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::Lt, left, right)
    }

    /// Add a less than or equal to test operation to the current [`BasicBlock`].
    pub fn le(&mut self, left: Operand, right: Operand) -> RValue {
        debug!("Less or Equal: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::Le, left, right)
    }

    /// Add a greater than test operation to the current [`BasicBlock`].
    pub fn gt(&mut self, left: Operand, right: Operand) -> RValue {
        debug!("Greater: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::Gt, left, right)
    }

    /// Add a greater than or equal to test operation to the current [`BasicBlock`].
    pub fn ge(&mut self, left: Operand, right: Operand) -> RValue {
        debug!("Greater or Equal: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::Ge, left, right)
    }

    /// Add a raw pointer offset operation to the current [`BasicBlock`].
    pub fn offset(&mut self, left: Operand, right: Operand) -> RValue {
        debug!("Pointer Offset: {:?}, {:?}", left, right);
        todo!()
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
    pub fn term_call(&mut self) {
        debug!("Call");
        todo!()
    }
}
