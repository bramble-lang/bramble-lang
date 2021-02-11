use std::{fmt, marker::PhantomData};
use fmt::Debug;
use stdext::function_name;

use crate::{diagnostics::config::TracingConfig, expression::Expression};
use crate::syntax::module::*;
use crate::syntax::routinedef::*;
use crate::syntax::statement::*;
use crate::syntax::structdef::*;

use super::{annotation::Annotation, parameter::Parameter};

/**
 * This traverses the AST and determines what size register to
 * assign to each node in the AST: if it makes sense to assign
 * it to a register.
 */

pub struct TraverserMut<A, T> where A: Debug + Annotation, T: Fn (&A) -> String {
    pub tracing: TracingConfig,
    pub trace: T,
    pub(crate) shit: PhantomData<A>,
}

impl<A, T> TraverserMut<A, T> where A: Debug + Annotation, T: Fn(&A) -> String {
    /**
     * Determine the size of register needed to store a value, based upon the number of bytes
     * the type takes.
     *
     * Custom types are always represented using 64bit registers because they are currently
     * always referred to via addresses.
     */

    pub fn for_module<F>(&self, m: &mut Module<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.tracer(&m.get_annotations());
        f(m.get_annotations_mut());

        for child_module in m.get_modules_mut().iter_mut() {
            self.for_module(child_module, f);
        }

        self.for_items(m.get_functions_mut(), f);

        self.for_items(m.get_coroutines_mut(), f);

        self.for_items(m.get_structs_mut(), f);
    }

    fn for_items<F>(&self, items: &mut Vec<Item<A>>, f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        for i in items.iter_mut() {
            self.tracer(&i.get_annotations());
            match i {
                Item::Struct(sd) => {
                    self.for_structdef(sd, f);
                }
                Item::Routine(rd) => {
                    self.for_routinedef(rd, f);
                }
            };
        }
    }

    fn for_structdef<F>(&self, sd: &mut StructDef<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.tracer(&sd.get_annotations());
        f(sd.get_annotations_mut());
        self.for_parameters(&mut sd.fields, f);
    }

    fn for_routinedef<F>(&self, rd: &mut RoutineDef<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.tracer(&rd.get_annotations());
        f(rd.get_annotations_mut());
        // loop through all the params
        self.for_parameters(&mut rd.params, f);

        // loop through every statement and analyze the child nodes of the routine definition
        for e in rd.get_body_mut().iter_mut() {
            self.for_statement(e, f);
        }
    }

    fn for_parameters<F>(&self, params: &mut Vec<Parameter<A>>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        for p in params {
        self.tracer(&p.get_annotations());
            f(p.get_annotations_mut())
        }
    }

    fn for_statement<F>(&self, statement: &mut Statement<A>, f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.tracer(&statement.get_annotations());
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

    fn for_bind<F>(&self, bind: &mut Bind<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.tracer(&bind.get_annotations());
        f(bind.get_annotations_mut());
        self.for_expression(bind.get_rhs_mut(), f)
    }

    fn for_mutate<F>(&self, mutate: &mut Mutate<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.tracer(&mutate.get_annotations());
        f(mutate.get_annotations_mut());
        self.for_expression(mutate.get_rhs_mut(), f);
    }

    fn for_yieldreturn<F>(&self, yr: &mut YieldReturn<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.tracer(&yr.get_annotations());
        f(yr.get_annotations_mut());
        yr.get_value_mut()
            .as_mut()
            .map(|rv| self.for_expression(rv, f));
    }

    fn for_return<F>(&self, r: &mut Return<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.tracer(&r.get_annotations());
        f(r.get_annotations_mut());
        r.get_value_mut()
            .as_mut()
            .map(|rv| self.for_expression(rv, f));
    }

    fn for_expression<F>(&self, exp: &mut Expression<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        use Expression::*;

        self.tracer(&exp.get_annotations());
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

    fn for_expression_block<F>(&self, block: &mut Expression<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.tracer(&block.get_annotations());
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

    fn for_member_access<F>(&self, access: &mut Expression<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.tracer(&access.get_annotations());
        if let Expression::MemberAccess(ref mut annotation, src, _member) = access {
            f(annotation);
            self.for_expression(src, f);
        } else {
            panic!("Expected MemberAccess, but got {:?}", access)
        }
    }

    fn for_unary_op<F>(&self, un_op: &mut Expression<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.tracer(&un_op.get_annotations());
        if let Expression::UnaryOp(ref mut annotation, _op, operand) = un_op {
            f(annotation);
            self.for_expression(operand, f);
        } else {
            panic!("Expected UnaryOp, but got {:?}", un_op)
        }
    }

    fn for_binary_op<F>(&self, bin_op: &mut Expression<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.tracer(&bin_op.get_annotations());
        if let Expression::BinaryOp(ref mut annotation, _op, l, r) = bin_op {
            f(annotation);
            self.for_expression(l, f);
            self.for_expression(r, f);
        } else {
            panic!("Expected BinaryOp, but got {:?}", bin_op)
        }
    }

    fn for_if<F>(&self, if_exp: &mut Expression<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.tracer(&if_exp.get_annotations());
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

    fn for_routine_call<F>(&self, rc: &mut Expression<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.tracer(&rc.get_annotations());
        if let Expression::RoutineCall(ref mut annotation, _call, _name, params) = rc {
            f(annotation);
            for p in params {
                self.for_expression(p, f);
            }
        } else {
            panic!("Expected RoutineCall, but got {:?}", rc)
        }
    }

    fn for_yield<F>(&self, yield_exp: &mut Expression<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.tracer(&yield_exp.get_annotations());
        if let Expression::Yield(ref mut annotation, e) = yield_exp {
            f(annotation);
            self.for_expression(e, f);
        } else {
            panic!("Expected Yield, but got {:?}", yield_exp)
        }
    }

    fn for_struct_expression<F>(&self, se: &mut Expression<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.tracer(&se.get_annotations());
        if let Expression::StructExpression(ref mut annotation, _struct_name, fields) = se {
            f(annotation);
            for (_, fe) in fields {
                self.for_expression(fe, f);
            }
        } else {
            panic!("Expected StructExpression, but got {:?}", se)
        }
    }

    fn tracer(&self, annotation: &A) {
        let line = annotation.line() as usize;

        let print_trace = match &self.tracing {
            &TracingConfig::Only(ln) => (line) == ln,
            &TracingConfig::Before(ln) => (line) <= ln,
            &TracingConfig::After(ln) => (line) >= ln,
            &TracingConfig::Between(start, end) => (line) >= start && (line) <= end,
            &TracingConfig::All => true,
            &TracingConfig::Off => false,
        };
        let m = (self.trace)(annotation);
        if print_trace {
            println!(
                "{} L{}: {}",
                function_name!(),
                line,
                m,
            )
        }
    }
}
