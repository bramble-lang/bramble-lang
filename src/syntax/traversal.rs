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

pub struct TraverserMut {}

impl TraverserMut {
    /**
     * Determine the size of register needed to store a value, based upon the number of bytes
     * the type takes.
     *
     * Custom types are always represented using 64bit registers because they are currently
     * always referred to via addresses.
     */

    pub fn for_module<A, F>(&self, m: &mut Module<A>, mut f: F)
    where
        A: Debug,
        F: FnMut(&mut A) + Copy,
    {
        f(m.get_annotations_mut());

        for child_module in m.get_modules_mut().iter_mut() {
            self.for_module(child_module, f);
        }
        self.for_items(m.get_functions_mut(), f);

        self.for_items(m.get_coroutines_mut(), f);

        self.for_items(m.get_structs_mut(), f);
    }

    fn for_items<A, F>(&self, items: &mut Vec<Item<A>>, f: F)
    where
        A: Debug,
        F: FnMut(&mut A) + Copy,
    {
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

    fn for_structdef<A, F>(&self, sd: &mut StructDef<A>, mut f: F)
    where
        A: Debug,
        F: FnMut(&mut A) + Copy,
    {
        f(sd.get_annotations_mut())
    }

    fn for_routine<A, F>(&self, rd: &mut RoutineDef<A>, mut f: F)
    where
        A: Debug,
        F: FnMut(&mut A) + Copy,
    {
        f(rd.get_annotations_mut());
        // loop through all the params
        for _p in rd.get_params() {}

        // loop through every statement and analyze the child nodes of the routine definition
        for e in rd.get_body_mut().iter_mut() {
            self.for_statement(e, f);
        }
    }

    fn for_statement<A, F>(&self, statement: &mut Statement<A>, f: F)
    where
        A: Debug,
        F: FnMut(&mut A) + Copy,
    {
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

    fn for_bind<A, F>(&self, bind: &mut Bind<A>, mut f: F)
    where
        A: Debug,
        F: FnMut(&mut A) + Copy,
    {
        f(bind.get_annotations_mut());
        self.for_expression(bind.get_rhs_mut(), f)
    }

    fn for_mutate<A, F>(&self, mutate: &mut Mutate<A>, mut f: F)
    where
        A: Debug,
        F: FnMut(&mut A) + Copy,
    {
        f(mutate.get_annotations_mut());
        self.for_expression(mutate.get_rhs_mut(), f);
    }

    fn for_yieldreturn<A, F>(&self, yr: &mut YieldReturn<A>, mut f: F)
    where
        A: Debug,
        F: FnMut(&mut A) + Copy,
    {
        f(yr.get_annotations_mut());
        yr.get_value_mut()
            .as_mut()
            .map(|rv| self.for_expression(rv, f));
    }

    fn for_return<A, F>(&self, r: &mut Return<A>, mut f: F)
    where
        A: Debug,
        F: FnMut(&mut A) + Copy,
    {
        f(r.get_annotations_mut());
        r.get_value_mut()
            .as_mut()
            .map(|rv| self.for_expression(rv, f));
    }

    fn for_expression<A, F>(&self, exp: &mut Expression<A>, mut f: F)
    where
        A: Debug,
        F: FnMut(&mut A) + Copy,
    {
        use Expression::*;

        match exp {
            ExpressionBlock(..) => self.for_expression_block(exp, f),
            Expression::Integer32(ref mut annotation, _i) => f(annotation),
            Expression::Integer64(ref mut annotation, _i) => f(annotation),
            Expression::Boolean(ref mut annotation, _b) => f(annotation),
            Expression::StringLiteral(ref mut annotation, _s) => f(annotation),
            Expression::CustomType(ref mut annotation, _name) => f(annotation),
            Expression::Identifier(ref mut annotation, _id) => f(annotation),
            Path(ref mut annotation, _path) => f(annotation),
            Expression::IdentifierDeclare(ref mut annotation, _id, _p) => f(annotation),
            MemberAccess(..) => self.for_member_access(exp, f),
            UnaryOp(..) => self.for_unary_op(exp, f),
            BinaryOp(..) => self.for_binary_op(exp, f),
            If { .. } => self.for_if(exp, f),
            Yield(..) => self.for_yield(exp, f),
            RoutineCall(..) => self.for_routine_call(exp, f),
            StructExpression(..) => self.for_struct_expression(exp, f),
        }
    }

    fn for_expression_block<A, F>(&self, block: &mut Expression<A>, mut f: F)
    where
        A: Debug,
        F: FnMut(&mut A) + Copy,
    {
        if let Expression::ExpressionBlock(ref mut annotation, ref mut body, ref mut final_exp) = block {
            f(annotation);

            for e in body.iter_mut() {
                self.for_statement(e, f);
            }

            final_exp.as_mut().map(|fe| self.for_expression(fe, f));
        } else {
            panic!("Expected ExpressionBlock, but got {:?}", block)
        }
    }

    fn for_member_access<A, F>(&self, access: &mut Expression<A>, mut f: F)
    where
        A: Debug,
        F: FnMut(&mut A) + Copy,
    {
        if let Expression::MemberAccess(ref mut annotation, src, _member) = access {
            f(annotation);
            self.for_expression(src, f);
        } else {
            panic!("Expected MemberAccess, but got {:?}", access)
        }
    }

    fn for_unary_op<A, F>(&self, un_op: &mut Expression<A>, mut f: F)
    where
        A: Debug,
        F: FnMut(&mut A) + Copy,
    {
        if let Expression::UnaryOp(ref mut annotation, _op, operand) = un_op {
            f(annotation);
            self.for_expression(operand, f);
        } else {
            panic!("Expected UnaryOp, but got {:?}", un_op)
        }
    }

    fn for_binary_op<A, F>(&self, bin_op: &mut Expression<A>, mut f: F)
    where
        A: Debug,
        F: FnMut(&mut A) + Copy,
    {
        if let Expression::BinaryOp(ref mut annotation, _op, l, r) = bin_op {
            f(annotation);
            self.for_expression(l, f);
            self.for_expression(r, f);
        } else {
            panic!("Expected BinaryOp, but got {:?}", bin_op)
        }
    }

    fn for_if<A, F>(&self, if_exp: &mut Expression<A>, mut f: F)
    where
        A: Debug,
        F: FnMut(&mut A) + Copy,
    {
        if let Expression::If {
            ref mut annotation,
            cond,
            if_arm,
            else_arm,
        } = if_exp
        {
            f(annotation);
            self.for_expression(cond, f);
            self.for_expression(if_arm, f);
            else_arm.as_mut().map(|ea| self.for_expression(ea, f));
        } else {
            panic!("Expected IfExpression, but got {:?}", if_exp)
        }
    }

    fn for_routine_call<A, F>(&self, rc: &mut Expression<A>, mut f: F)
    where
        A: Debug,
        F: FnMut(&mut A) + Copy,
    {
        if let Expression::RoutineCall(ref mut annotation, _call, _name, params) = rc {
            f(annotation);
            for p in params {
                self.for_expression(p, f);
            }
        } else {
            panic!("Expected RoutineCall, but got {:?}", rc)
        }
    }

    fn for_yield<A, F>(&self, yield_exp: &mut Expression<A>, mut f: F)
    where
        A: Debug,
        F: FnMut(&mut A) + Copy,
    {
        if let Expression::Yield(ref mut annotation, e) = yield_exp {
            f(annotation);
            self.for_expression(e, f);
        } else {
            panic!("Expected Yield, but got {:?}", yield_exp)
        }
    }

    fn for_struct_expression<A, F>(&self, se: &mut Expression<A>, mut f: F)
    where
        A: Debug,
        F: FnMut(&mut A) + Copy,
    {
        if let Expression::StructExpression(ref mut annotation, _struct_name, fields) = se {
            f(annotation);
            for (_, fe) in fields {
                self.for_expression(fe, f);
            }
        } else {
            panic!("Expected StructExpression, but got {:?}", se)
        }
    }
}
