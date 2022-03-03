//! Converts the Bramble AST to the CFG MIR representation used for
//! dataflow analyses; such as, lifetime checking, variable initialization,
//! consistency rules checking, and so on.

// Transformer
// This process takes the AST for a compilation unit and transforms it into the
// CFG MIR used for dataflow analysis and LLVM IR generation by the Bramble
// compiler.

use log::debug;

use crate::{
    compiler::{
        ast::{BinaryOperator, Bind, Expression, Module, Return, RoutineDef, Statement, Type},
        semantics::semanticnode::SemanticContext,
        Span,
    },
    StringId,
};

use super::ir::*;

pub fn module_transform(module: &Module<SemanticContext>) -> Vec<Procedure> {
    let funcs = module.get_functions();
    let mut mirs = vec![];

    for f in funcs {
        match f {
            crate::compiler::ast::Item::Routine(r) => {
                let ft = FuncTransformer::new();
                let p = ft.transform(r);
                mirs.push(p);
            }
            crate::compiler::ast::Item::Struct(_) => todo!(),
            crate::compiler::ast::Item::Extern(_) => todo!(),
        }
    }

    mirs
}

/// Provides a Builder interface for constructing the MIR CFG representation of a
/// routine. This will keep track of the current [`BasicBlock`] and make sure that
/// MIR operations are applied to that [`BasicBlock`]. This also provides a simplfied
/// interface for constructing the MIR operands, operations, and statements, to
/// simplify the code that traverses input ASTs and transforms them into MIR.
struct MirBuilder {
    proc: Procedure,
    current_bb: Option<BasicBlockId>,
}

impl MirBuilder {
    pub fn new() -> MirBuilder {
        MirBuilder {
            proc: Procedure::new(&Type::Unit, Span::zero()),
            current_bb: None,
        }
    }

    fn new_bb(&mut self) -> BasicBlockId {
        self.proc.new_bb()
    }

    fn set_bb(&mut self, bb: BasicBlockId) {
        self.current_bb = Some(bb)
    }

    fn find_var(&self, name: StringId) -> Option<VarId> {
        self.proc.find_var(name)
    }

    fn const_i64(&mut self, i: i64) -> Operand {
        Operand::Constant(Constant::I64(i))
    }

    fn const_bool(&mut self, b: bool) -> Operand {
        Operand::Constant(Constant::Bool(b))
    }

    fn var(&mut self, name: StringId, mutable: bool, ty: &Type) -> VarId {
        self.proc.add_var(name, mutable, ty, ScopeId::new(0))
    }

    fn temp(&mut self, ty: &Type) -> TempId {
        self.proc.add_temp(ty)
    }

    fn temp_store(&mut self, rv: RValue, ty: &Type) -> Operand {
        let tv = LValue::Temp(self.temp(ty));
        debug!("Temp store: {:?} := {:?}", tv, rv);

        self.store(tv.clone(), rv);

        Operand::LValue(tv)
    }

    fn store(&mut self, lv: LValue, rv: RValue) {
        let cid = self.current_bb.unwrap();
        let bb = self.proc.get_bb_mut(cid);
        bb.add_stm(super::ir::Statement::new(StatementKind::Assign(lv, rv)));
    }

    fn sub(&mut self) {
        debug!("Sub");
        todo!()
    }

    fn mul(&mut self) {
        debug!("Mul");
        todo!()
    }

    /// Add two operands together
    fn add(&mut self, left: Operand, right: Operand) -> RValue {
        debug!("Add: {:?}, {:?}", left, right);
        RValue::BinOp(BinOp::Add, left, right)
    }

    /// Terminates by returning to the caller function
    fn term_return(&mut self) {
        debug!("Terminator: Return");
        let cid = self.current_bb.unwrap();
        let bb = self.proc.get_bb_mut(cid);
        bb.set_terminator(Terminator::new(TerminatorKind::Return));
    }

    /// Terminates by going to the destination basic block
    fn term_goto(&mut self, target: BasicBlockId) {
        debug!("Goto: {:?}", target);
        let cid = self.current_bb.unwrap();
        let bb = self.proc.get_bb_mut(cid);
        bb.set_terminator(Terminator::new(TerminatorKind::GoTo{target}))
    }

    /// Terminates with a conditional go to
    fn term_cond_goto(&mut self, cond: Operand, then_bb: BasicBlockId, else_bb: BasicBlockId) {
        debug!("If {:?} then {:?} else {:?}", cond, then_bb, else_bb);
        let cid = self.current_bb.unwrap();
        let bb = self.proc.get_bb_mut(cid);
        bb.set_terminator(Terminator::new(TerminatorKind::CondGoTo {
            cond,
            tru: then_bb,
            fls: else_bb,
        }));
    }

    /// Terminates by calling the given function
    fn term_call(&mut self) {
        debug!("Call");
        todo!()
    }
}

/// Transform a single function to the MIR form
struct FuncTransformer {
    mir: MirBuilder,
}

impl FuncTransformer {
    pub fn new() -> FuncTransformer {
        FuncTransformer {
            mir: MirBuilder::new(),
        }
    }

    pub fn transform(mut self, func: &RoutineDef<SemanticContext>) -> Procedure {
        // Create a new MIR Procedure
        // Create a BasicBlock for the function
        let bb = self.mir.new_bb();
        self.mir.set_bb(bb);

        // Iterate over every statement and add it to the basic block
        func.body.iter().for_each(|stm| self.statement(stm));

        // Add the return from function as the terminator for the final basic block of the function
        self.mir.term_return();
        self.mir.proc
    }

    fn statement(&mut self, stm: &Statement<SemanticContext>) {
        debug!("Transform statement");
        match stm {
            Statement::Bind(bind) => self.bind(bind),
            Statement::Expression(expr) => {
                self.expression(expr);
            }
            Statement::Mutate(_) => todo!(),
            Statement::YieldReturn(_) => todo!(),
            Statement::Return(ret) => self.ret(ret),
        }
    }

    fn bind(&mut self, bind: &Bind<SemanticContext>) {
        debug!("Binding statement");
        let var = bind.get_id();
        let mutable = bind.is_mutable();
        let ty = bind.get_type();
        let vid = self.mir.var(var, mutable, ty);

        let expr = self.expression(bind.get_rhs());

        self.mir.store(LValue::Var(vid), RValue::Use(expr))
    }

    fn ret(&mut self, ret: &Return<SemanticContext>) {
        match ret.get_value() {
            Some(val) => {
                let v = self.expression(val);
                self.mir.store(LValue::ReturnPointer, RValue::Use(v));
            }
            None => (),
        };
        self.mir.term_return();
    }

    /// This can return either an Operand or an RValue, if this is evaluating a constant or an identifier
    /// then this returns an operand.  If this is evaluating an operation then it returns an RValue.
    fn expression(&mut self, expr: &Expression<SemanticContext>) -> Operand {
        match expr {
            Expression::I64(_, i) => self.mir.const_i64(*i),
            Expression::BinaryOp(ctx, op, left, right) => {
                let rv = self.binary_op(*op, left, right);
                self.mir.temp_store(rv, ctx.ty())
            }
            Expression::Null(_) => todo!(),
            Expression::U8(_, _) => todo!(),
            Expression::U16(_, _) => todo!(),
            Expression::U32(_, _) => todo!(),
            Expression::U64(_, _) => todo!(),
            Expression::I8(_, _) => todo!(),
            Expression::I16(_, _) => todo!(),
            Expression::I32(_, _) => todo!(),
            Expression::F64(_, _) => todo!(),
            Expression::Boolean(_, b) => self.mir.const_bool(*b),
            Expression::StringLiteral(_, _) => todo!(),
            Expression::ArrayExpression(_, _, _) => todo!(),
            Expression::ArrayAt {
                context,
                array,
                index,
            } => todo!(),
            Expression::SizeOf(_, _) => todo!(),
            Expression::CustomType(_, _) => todo!(),
            Expression::Identifier(_, id) => {
                // Look up Var ID using the Identifier String ID
                let vid = self.mir.find_var(*id).unwrap();

                // Return a LValue::Var(VarId) as the result of this expression
                Operand::LValue(LValue::Var(vid))
            }
            Expression::Path(_, _) => todo!(),
            Expression::MemberAccess(_, _, _) => todo!(),
            Expression::IdentifierDeclare(_, _, _) => todo!(),
            Expression::RoutineCall(_, _, _, _) => todo!(),
            Expression::StructExpression(_, _, _) => todo!(),
            Expression::If {
                context,
                cond,
                if_arm,
                else_arm,
            } => self.if_expr(cond, if_arm, else_arm),
            Expression::While {
                context,
                cond,
                body,
            } => todo!(),
            Expression::ExpressionBlock(_, block, expr) => {
                for stm in block {
                    self.statement(stm);
                }
                if let Some(expr) = expr {
                    self.expression(expr)
                } else {
                    todo!()
                }
            },
            Expression::TypeCast(_, _, _) => todo!(),
            Expression::UnaryOp(_, _, _) => todo!(),
            Expression::Yield(_, _) => todo!(),
        }
    }

    fn if_expr(
        &mut self,
        cond: &Expression<SemanticContext>,
        then_block: &Expression<SemanticContext>,
        else_block: &Option<Box<Expression<SemanticContext>>>,
    ) -> Operand {
        let then_bb = self.mir.new_bb();
        let else_bb = self.mir.new_bb();
        let merge_bb = self.mir.new_bb();
        let cond_val = self.expression(cond);
        self.mir.term_cond_goto(cond_val, then_bb, else_bb);

        // if the if expression has a type other than unit, then create a temporary
        // variable to store the resolved value.
        let temp = self.mir.temp(then_block.get_type());

        self.mir.set_bb(then_bb);
        let val = self.expression(then_block);
        self.mir.store(LValue::Temp(temp), RValue::Use(val));
        self.mir.term_goto(merge_bb);

        if let Some(else_block) = else_block {
            self.mir.set_bb(else_bb);
            let val = self.expression(else_block);
            self.mir.store(LValue::Temp(temp), RValue::Use(val));
            self.mir.term_goto(merge_bb);
        } else {
            self.mir.set_bb(else_bb);
            self.mir.term_goto(merge_bb);
        }

        self.mir.set_bb(merge_bb);
        Operand::LValue(LValue::Temp(temp))
    }

    fn binary_op(
        &mut self,
        op: BinaryOperator,
        left: &Expression<SemanticContext>,
        right: &Expression<SemanticContext>,
    ) -> RValue {
        match op {
            BinaryOperator::Add => {
                let left = self.expression(left);
                let right = self.expression(right);
                self.mir.add(left, right)
            }
            BinaryOperator::Sub => todo!(),
            BinaryOperator::Mul => todo!(),
            BinaryOperator::Div => todo!(),
            BinaryOperator::BAnd => todo!(),
            BinaryOperator::BOr => todo!(),
            BinaryOperator::Eq => todo!(),
            BinaryOperator::NEq => todo!(),
            BinaryOperator::Ls => todo!(),
            BinaryOperator::LsEq => todo!(),
            BinaryOperator::Gr => todo!(),
            BinaryOperator::GrEq => todo!(),
            BinaryOperator::RawPointerOffset => todo!(),
        }
    }
}