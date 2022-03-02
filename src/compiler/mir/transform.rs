//! Converts the Bramble AST to the CFG MIR representation used for
//! dataflow analyses; such as, lifetime checking, variable initialization,
//! consistency rules checking, and so on.

// Transformer
// This process takes the AST for a compilation unit and transforms it into the
// CFG MIR used for dataflow analysis and LLVM IR generation by the Bramble
// compiler.

use crate::compiler::{ast::{RoutineDef, Expression, Statement, BinaryOperator}, semantics::semanticnode::SemanticContext};

use super::ir::*;

/// Provides a Builder interface for constructing the MIR CFG representation of a 
/// routine. This will keep track of the current [`BasicBlock`] and make sure that
/// MIR operations are applied to that [`BasicBlock`]. This also provides a simplfied
/// interface for constructing the MIR operands, operations, and statements, to 
/// simplify the code that traverses input ASTs and transforms them into MIR.
pub struct MirGenerator {}

impl MirGenerator {
    pub fn new() -> MirGenerator {
        todo!()
    }

    fn module(&self) {}

    fn new_bb(&mut self) -> BasicBlockId {
        todo!()
    }

    fn set_bb(&mut self, bb: BasicBlockId) {
        todo!()
    }

    fn const_i64(&mut self, i: i64) -> Operand {
        todo!()
    }

    fn var(&mut self) -> VarId {
        todo!()
    }

    fn temp(&mut self) -> TempId {
        todo!()
    }

    fn sub(&mut self) {
        todo!()
    }

    fn mul(&mut self) {
        todo!()
    }

    /// Add two operands together
    fn add(&mut self, left: Operand, right: Operand) -> RValue {
        todo!()
    }

    /// Terminates by returning to the caller function
    fn term_return(&mut self) {
        todo!()
    }

    /// Terminates by going to the destination basic block
    fn term_goto(&mut self, destination: BasicBlockId) {
        todo!()
    }

    /// Terminates with a conditional go to
    fn term_if(&mut self, then_bb: BasicBlockId, else_bb: BasicBlockId) {
        todo!()
    }

    /// Terminates by calling the given function
    fn term_call(&mut self) {
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

    pub fn transform(&mut self, func: RoutineDef<SemanticContext>) {
        // Create a new MIR Procedure
        // Create a BasicBlock for the function
        let bb = self.gen.new_bb();
        self.gen.set_bb(bb);

        // Iterate over every statement and add it to the basic block

        // Add the return from function as the terminator for the final basic block of the function
        self.gen.term_return();
    }

    fn statement(&mut self, stm: Statement<SemanticContext>) {
        match stm {
            Statement::Bind(bind) => todo!(),
            Statement::Expression(expr) => todo!(),
            Statement::Mutate(_) => todo!(),
            Statement::YieldReturn(_) => todo!(),
            Statement::Return(ret) => todo!(),
        }
    }

    /// This can return either an Operand or an RValue, if this is evaluating a constant or an identifier
    /// then this returns an operand.  If this is evaluating an operation then it returns an RValue.
    fn expression(&mut self, expr: Expression<SemanticContext>) -> Result<Operand, RValue> { // TEMP using Result as Rust does not have an Either builtin
        match expr {
            Expression::Null(_) => todo!(),
            Expression::U8(_, _) => todo!(),
            Expression::U16(_, _) => todo!(),
            Expression::U32(_, _) => todo!(),
            Expression::U64(_, _) => todo!(),
            Expression::I8(_, _) => todo!(),
            Expression::I16(_, _) => todo!(),
            Expression::I32(_, _) => todo!(),
            Expression::I64(_, i) => Ok(self.gen.const_i64(i)),
            Expression::F64(_, _) => todo!(),
            Expression::Boolean(_, _) => todo!(),
            Expression::StringLiteral(_, _) => todo!(),
            Expression::ArrayExpression(_, _, _) => todo!(),
            Expression::ArrayAt { context, array, index } => todo!(),
            Expression::SizeOf(_, _) => todo!(),
            Expression::CustomType(_, _) => todo!(),
            Expression::Identifier(_, _) => todo!(),
            Expression::Path(_, _) => todo!(),
            Expression::MemberAccess(_, _, _) => todo!(),
            Expression::IdentifierDeclare(_, _, _) => todo!(),
            Expression::RoutineCall(_, _, _, _) => todo!(),
            Expression::StructExpression(_, _, _) => todo!(),
            Expression::If { context, cond, if_arm, else_arm } => todo!(),
            Expression::While { context, cond, body } => todo!(),
            Expression::ExpressionBlock(_, _, _) => todo!(),
            Expression::BinaryOp(_, _, _, _) => todo!(),
            Expression::TypeCast(_, _, _) => todo!(),
            Expression::UnaryOp(_, _, _) => todo!(),
            Expression::Yield(_, _) => todo!(),
        }
    }

    fn binary_op(&mut self, op: BinaryOperator, left: Expression<SemanticContext>, right: Expression<SemanticContext>) -> RValue {
        match op {
            BinaryOperator::Add => {
                let left = self.expression(left);
                let right = self.expression(right);
                self.gen.add(left, right)
            },
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
