//! Converts the Bramble AST to the CFG MIR representation used for
//! dataflow analyses; such as, lifetime checking, variable initialization,
//! consistency rules checking, and so on.

// Transformer
// This process takes the AST for a compilation unit and transforms it into the
// CFG MIR used for dataflow analysis and LLVM IR generation by the Bramble
// compiler.

use log::debug;

use crate::compiler::{
    ast::{BinaryOperator, Expression, Module, RoutineDef, Statement, Type, Return},
    semantics::semanticnode::SemanticContext,
    Span,
};

use super::ir::*;

pub fn module_transform(module: &Module<SemanticContext>) {
    let funcs = module.get_functions();

    for f in funcs {
        match f {
            crate::compiler::ast::Item::Routine(r) => {
                let ft = FuncTransformer::new();
                let p = ft.transform(r);
                println!("Procedure: {}", p);
            }
            crate::compiler::ast::Item::Struct(_) => todo!(),
            crate::compiler::ast::Item::Extern(_) => todo!(),
        }
    }
}

/// Provides a Builder interface for constructing the MIR CFG representation of a
/// routine. This will keep track of the current [`BasicBlock`] and make sure that
/// MIR operations are applied to that [`BasicBlock`]. This also provides a simplfied
/// interface for constructing the MIR operands, operations, and statements, to
/// simplify the code that traverses input ASTs and transforms them into MIR.
struct MirGenerator {
    proc: Procedure,
    current_bb: Option<BasicBlockId>,
}

impl MirGenerator {
    pub fn new() -> MirGenerator {
        MirGenerator {
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

    fn const_i64(&mut self, i: i64) -> Operand {
        Operand::Constant
    }

    fn var(&mut self) -> VarId {
        todo!()
    }

    fn temp(&mut self, ty: &Type) -> TempId {
        self.proc.add_temp(ty)
    }

    fn temp_store(&mut self, rv: RValue, ty: &Type) -> Operand {
        let tv = LValue::Temp(self.temp(ty));
        debug!("Temp store: {:?} := {:?}", tv, rv);

        let cid = self.current_bb.unwrap();
        let bb = self.proc.get_bb_mut(cid);
        bb.add_stm(super::ir::Statement::new(StatementKind::Assign(
            tv.clone(),
            rv,
        )));

        Operand::LValue(tv)
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
    fn term_goto(&mut self, destination: BasicBlockId) {
        debug!("Goto: {:?}", destination);
        todo!()
    }

    /// Terminates with a conditional go to
    fn term_if(&mut self, then_bb: BasicBlockId, else_bb: BasicBlockId) {
        debug!("If _ then {:?} else {:?}", then_bb, else_bb);
        todo!()
    }

    /// Terminates by calling the given function
    fn term_call(&mut self) {
        debug!("Call");
        todo!()
    }
}

/// Transform a single function to the MIR form
struct FuncTransformer {
    gen: MirGenerator,
}

impl FuncTransformer {
    pub fn new() -> FuncTransformer {
        FuncTransformer {
            gen: MirGenerator::new(),
        }
    }

    pub fn transform(mut self, func: &RoutineDef<SemanticContext>) -> Procedure {
        // Create a new MIR Procedure
        // Create a BasicBlock for the function
        let bb = self.gen.new_bb();
        self.gen.set_bb(bb);

        // Iterate over every statement and add it to the basic block
        func.body.iter().for_each(|stm| self.statement(stm));

        // Add the return from function as the terminator for the final basic block of the function
        self.gen.term_return();
        self.gen.proc
    }

    fn statement(&mut self, stm: &Statement<SemanticContext>) {
        debug!("Transform statement");
        match stm {
            Statement::Bind(bind) => todo!(),
            Statement::Expression(expr) => todo!(),
            Statement::Mutate(_) => todo!(),
            Statement::YieldReturn(_) => todo!(),
            Statement::Return(ret) => self.ret(ret),
        }
    }

    fn ret(&mut self, ret: &Return<SemanticContext>) {
        match ret.get_value() {
            Some(val) => self.expression(val),
            None => todo!(),
        };
        self.gen.term_return();
    }

    /// This can return either an Operand or an RValue, if this is evaluating a constant or an identifier
    /// then this returns an operand.  If this is evaluating an operation then it returns an RValue.
    fn expression(&mut self, expr: &Expression<SemanticContext>) -> ExprResult {
        // TEMP: it might turn out that I don't need ExprResult
        match expr {
            Expression::I64(_, i) => ExprResult::Operand(self.gen.const_i64(*i)),
            Expression::BinaryOp(ctx, op, left, right) => {
                let rv = self.binary_op(*op, left, right);
                let temp = self.gen.temp_store(rv, ctx.ty());
                ExprResult::Operand(temp)
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
            Expression::Boolean(_, _) => todo!(),
            Expression::StringLiteral(_, _) => todo!(),
            Expression::ArrayExpression(_, _, _) => todo!(),
            Expression::ArrayAt {
                context,
                array,
                index,
            } => todo!(),
            Expression::SizeOf(_, _) => todo!(),
            Expression::CustomType(_, _) => todo!(),
            Expression::Identifier(_, _) => todo!(),
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
            } => todo!(),
            Expression::While {
                context,
                cond,
                body,
            } => todo!(),
            Expression::ExpressionBlock(_, _, _) => todo!(),
            Expression::TypeCast(_, _, _) => todo!(),
            Expression::UnaryOp(_, _, _) => todo!(),
            Expression::Yield(_, _) => todo!(),
        }
    }

    fn binary_op(
        &mut self,
        op: BinaryOperator,
        left: &Expression<SemanticContext>,
        right: &Expression<SemanticContext>,
    ) -> RValue {
        match op {
            BinaryOperator::Add => {
                let left = self.expression(left).operand();
                let right = self.expression(right).operand();
                self.gen.add(left, right)
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

enum ExprResult {
    Operand(Operand),
    RValue(RValue),
}

impl ExprResult {
    fn operand(self) -> Operand {
        match self {
            Self::Operand(op) => op,
            Self::RValue(_) => panic!("Expected Operand but got RValue"),
        }
    }

    fn rvalue(self) -> RValue {
        match self {
            Self::RValue(rv) => rv,
            Self::Operand(_) => panic!("Expected RValue but got Operand"),
        }
    }
}
