use std::fmt;

use fmt::Debug;

use crate::expression::Expression;
use crate::syntax::module::*;
use crate::syntax::routinedef::*;
use crate::syntax::statement::*;
use crate::syntax::structdef::*;

/**
 * This traverses the AST and determines what size register to
 * assign to each node in the AST: if it makes sense to assign
 * it to a register.
 */

pub struct TraverserMut {
}

impl TraverserMut {
    /**
     * Determine the size of register needed to store a value, based upon the number of bytes
     * the type takes.
     *
     * Custom types are always represented using 64bit registers because they are currently
     * always referred to via addresses.
     */

    pub fn for_module<A, B,F>(
        &self,
        m: &mut Module<A>,
        f: F,
    ) where A: Debug, B: Debug, F: Fn(A)->B + Copy {
        for child_module in m.get_modules_mut().iter_mut() {
            self.for_module(child_module, f);
        }
        self.for_items(m.get_functions_mut(), f);

        self.for_items(m.get_coroutines_mut(), f);

        self.for_items(m.get_structs_mut(), f);
    }

    fn for_items<A, B, F>(
        &self,
        items: &mut Vec<Item<A>>,
    f: F) where A: Debug, B: Debug, F: Fn(A) -> B + Copy {
        for i in items.iter_mut() {
            match i {
                Item::Struct(sd) => {
                    self.for_structdef(sd, f);
                }
                Item::Routine(rd) => {
                    self.for_routine(rd, f);
                }
            };
        }
    }

    fn for_structdef<A, B, F>(
        &self,
        sd: &mut StructDef<A>,
    f: F) where A: Debug, B: Debug, F: Fn(A) -> B + Copy {
    }

    fn for_routine<A, B, F>(
        &self,
        rd: &mut RoutineDef<A>,
    f: F) where A: Debug, B: Debug, F: Fn(A) -> B + Copy {
        // loop through all the params
        for p in rd.get_params() {
        }

        // loop through every statement and analyze the child nodes of the routine definition
        for e in rd.get_body_mut().iter_mut() {
            self.for_statement(e, f);
        }
    }

    fn for_statement<A, B, F>(
        &self,
        statement: &mut Statement<A>,
    f: F) where A: Debug, B: Debug, F: Fn(A) -> B + Copy {
        match statement {
            Statement::Bind(b) => {
                self.for_bind(b, f);
            }
            Statement::Mutate(m) => {
                self.for_mutate(m, f);
            }
            Statement::Return(r) => {
                self.for_return(r, f);
            }
            Statement::YieldReturn(yr) => {
                self.for_yieldreturn(yr, f);
            }
            Statement::Expression(e) => {
                self.for_expression(e, f);
            }
        };
    }

    fn for_bind<A, B, F>(&self, bind: &mut Bind<A>, f: F) where A: Debug, B: Debug, F: Fn(A) -> B + Copy {
        self.for_expression(bind.get_rhs_mut(), f)
    }

    fn for_mutate<A, B, F>(
        &self,
        mutate: &mut Mutate<A>,
    f: F) where A: Debug, B: Debug, F: Fn(A) -> B + Copy {
        self.for_expression(mutate.get_rhs_mut(), f);
    }

    fn for_yieldreturn<A, B, F>(
        &self,
        yr: &mut YieldReturn<A>,
    f: F) where A: Debug, B: Debug, F: Fn(A) -> B + Copy {
        yr.get_value_mut()
            .as_mut()
            .map(|rv| self.for_expression(rv, f));
    }

    fn for_return<A, B, F>(&self, r: &mut Return<A>, f: F) where A: Debug, B: Debug, F: Fn(A) -> B + Copy {
        r.get_value_mut()
            .as_mut()
            .map(|rv| self.for_expression(rv, f));
    }

    fn for_expression<A, B, F>(
        &self,
        exp: &mut Expression<A>,
    f: F) where A: Debug, B: Debug, F: Fn(A) -> B + Copy {
        use Expression::*;

        match exp {
            ExpressionBlock(..) => self.for_expression_block(exp, f),
            Expression::Integer32(_m, _i) => {}
            Expression::Integer64(_m, _i) => {}
            Expression::Boolean(_m, _b) => {}
            Expression::StringLiteral(_m, _s) => {}
            Expression::CustomType(_m, _name) => {}
            Expression::Identifier(_m, _id) => {}
            Path(_m, _path) => {}
            Expression::IdentifierDeclare(_m, _id, _p) => {}
            MemberAccess(..) => self.for_member_access(exp, f),
            UnaryOp(..) => self.for_unary_op(exp, f),
            BinaryOp(..) => self.for_binary_op(exp, f),
            If { .. } => self.for_if(exp, f),
            Yield(..) => self.for_yield(exp, f),
            RoutineCall(..) => self.for_routine_call(exp, f),
            StructExpression(..) => self.for_struct_expression(exp, f),
        }
    }

    fn for_expression_block<A, B, F>(
        &self,
        block: &mut Expression<A>,
    f: F) where A: Debug, B: Debug, F: Fn(A) -> B + Copy {
        if let Expression::ExpressionBlock(_m, ref mut body, ref mut final_exp) = block {
            for e in body.iter_mut() {
                self.for_statement(e, f);
            }

            final_exp
                .as_mut()
                .map(|fe| self.for_expression(fe, f));
        } else {
            panic!("Expected ExpressionBlock, but got {:?}", block)
        }
    }

    fn for_member_access<A, B, F>(
        &self,
        access: &mut Expression<A>,
    f: F) where A: Debug, B: Debug, F: Fn(A) -> B + Copy {
        if let Expression::MemberAccess(_m, src, _member) = access {
            self.for_expression(src, f);
        } else {
            panic!("Expected MemberAccess, but got {:?}", access)
        }
    }

    fn for_unary_op<A, B, F>(
        &self,
        un_op: &mut Expression<A>,
    f: F) where A: Debug, B: Debug, F: Fn(A) -> B + Copy {
        if let Expression::UnaryOp(_m, _op, operand) = un_op {
            self.for_expression(operand, f);
        } else {
            panic!("Expected UnaryOp, but got {:?}", un_op)
        }
    }

    fn for_binary_op<A, B, F>(
        &self,
        bin_op: &mut Expression<A>,
    f: F) where A: Debug, B: Debug, F: Fn(A) -> B + Copy {
        if let Expression::BinaryOp(_m, _op, l, r) = bin_op {
            self.for_expression(l, f);
            self.for_expression(r, f);
        } else {
            panic!("Expected BinaryOp, but got {:?}", bin_op)
        }
    }

    fn for_if<A, B, F>(
        &self,
        if_exp: &mut Expression<A>,
    f: F) where A: Debug, B: Debug, F: Fn(A) -> B + Copy {
        if let Expression::If {
            annotation: _m,
            cond,
            if_arm,
            else_arm,
        } = if_exp
        {
            self.for_expression(cond, f);
            self.for_expression(if_arm, f);
            else_arm
                .as_mut()
                .map(|ea| self.for_expression(ea, f));
        } else {
            panic!("Expected IfExpression, but got {:?}", if_exp)
        }
    }

    fn for_routine_call<A, B, F>(
        &self,
        rc: &mut Expression<A>,
    f: F) where A: Debug, B: Debug, F: Fn(A) -> B + Copy {
        if let Expression::RoutineCall(_m, _call, _name, params) = rc {
            for p in params {
                self.for_expression(p, f);
            }
        } else {
            panic!("Expected RoutineCall, but got {:?}", rc)
        }
    }

    fn for_yield<A, B, F>(
        &self,
        yield_exp: &mut Expression<A>,
    f: F) where A: Debug, B: Debug, F: Fn(A) -> B + Copy {
        if let Expression::Yield(_m, e) = yield_exp {
            self.for_expression(e, f);
        } else {
            panic!("Expected Yield, but got {:?}", yield_exp)
        }
    }

    fn for_struct_expression<A, B, F>(
        &self,
        se: &mut Expression<A>,
    f: F) where A: Debug, B: Debug, F: Fn(A) -> B + Copy {
        if let Expression::StructExpression(_annotations, _struct_name, fields) = se {
            for (_, fe) in fields {
                self.for_expression(fe, f);
            }
        } else {
            panic!("Expected StructExpression, but got {:?}", se)
        }
    }
}
