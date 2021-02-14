use fmt::{Debug, Display};
use std::{fmt, marker::PhantomData};

use crate::ast::Expression;
use crate::ast::module::*;
use crate::ast::routinedef::*;
use crate::ast::statement::*;
use crate::ast::structdef::*;
use crate::diagnostics::config::TracingConfig;

use super::{super::node::Node, super::parameter::Parameter, Annotation};

/**
Traverse through each node, in pre-order DFS, and apply a function to mutate the
annotation on that node.

This does not mutate or alter the topology of the AST nor does it mutate the
AST node or its immediate value, only the Annotation on the visted nodes may
be mutated. The Annotation type can also not be changed.

This is for use with intermediate and smaller steps within a larger compiler
AST transformation; when specific fields within an Annotation need to be updated
with new values. This avoids the need to generate an entirely new AST with a
new Annotation type.
*/
pub struct ForEachPreOrderMut<A, T>
where
    A: Debug + Annotation,
    T: Fn(&A) -> String,
{
    pub name: String,
    pub tracing: TracingConfig,
    pub format: T,
    ph: PhantomData<A>,
}

impl<A, T> ForEachPreOrderMut<A, T>
where
    A: Debug + Annotation,
    T: Fn(&A) -> String,
{
    pub fn new(name: &str, tracing: TracingConfig, format: T) -> ForEachPreOrderMut<A, T> {
        ForEachPreOrderMut {
            name: name.into(),
            tracing,
            format,
            ph: PhantomData,
        }
    }

    pub fn for_module<F>(&self, m: &mut Module<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        if self.tracing != TracingConfig::Off {
            println!("{}", self.name);
        }

        f(m.annotation_mut());

        for child_module in m.get_modules_mut().iter_mut() {
            self.for_module(child_module, f);
        }

        self.for_items(m.get_functions_mut(), f);

        self.for_items(m.get_coroutines_mut(), f);

        self.for_items(m.get_structs_mut(), f);
        self.tracer(m);
    }

    fn for_items<F>(&self, items: &mut Vec<Item<A>>, f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        for i in items.iter_mut() {
            match i {
                Item::Struct(sd) => {
                    self.for_structdef(sd, f);
                }
                Item::Routine(rd) => {
                    self.for_routinedef(rd, f);
                }
            };
            self.tracer(i);
        }
    }

    fn for_structdef<F>(&self, sd: &mut StructDef<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        f(sd.annotation_mut());
        self.tracer(sd);
        self.for_parameters(&mut sd.fields, f);
    }

    fn for_routinedef<F>(&self, rd: &mut RoutineDef<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        f(rd.annotation_mut());
        self.tracer(rd);
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
            f(p.annotation_mut());
            self.tracer(p);
        }
    }

    fn for_statement<F>(&self, statement: &mut Statement<A>, f: F)
    where
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
        self.tracer(statement);
    }

    fn for_bind<F>(&self, bind: &mut Bind<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        f(bind.annotation_mut());
        self.tracer(bind);
        self.for_expression(bind.get_rhs_mut(), f)
    }

    fn for_mutate<F>(&self, mutate: &mut Mutate<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        f(mutate.annotation_mut());
        self.tracer(mutate);
        self.for_expression(mutate.get_rhs_mut(), f);
    }

    fn for_yieldreturn<F>(&self, yr: &mut YieldReturn<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        f(yr.annotation_mut());
        self.tracer(yr);
        yr.get_value_mut()
            .as_mut()
            .map(|rv| self.for_expression(rv, f));
    }

    fn for_return<F>(&self, r: &mut Return<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        f(r.annotation_mut());
        self.tracer(r);
        r.get_value_mut()
            .as_mut()
            .map(|rv| self.for_expression(rv, f));
    }

    fn for_expression<F>(&self, exp: &mut Expression<A>, mut f: F)
    where
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
        self.tracer(exp);
    }

    fn for_expression_block<F>(&self, block: &mut Expression<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        if let Expression::ExpressionBlock(ref mut annotation, ref mut body, ref mut final_exp) =
            block
        {
            f(annotation);

            for e in body.iter_mut() {
                self.for_statement(e, f);
            }

            final_exp.as_mut().map(|fe| self.for_expression(fe, f));
        } else {
            panic!("Expected ExpressionBlock, but got {:?}", block)
        }
        self.tracer(block);
    }

    fn for_member_access<F>(&self, access: &mut Expression<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        if let Expression::MemberAccess(ref mut annotation, src, _member) = access {
            f(annotation);
            self.for_expression(src, f);
        } else {
            panic!("Expected MemberAccess, but got {:?}", access)
        }
        self.tracer(access);
    }

    fn for_unary_op<F>(&self, un_op: &mut Expression<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        if let Expression::UnaryOp(ref mut annotation, _op, operand) = un_op {
            f(annotation);
            self.for_expression(operand, f);
        } else {
            panic!("Expected UnaryOp, but got {:?}", un_op)
        }
        self.tracer(un_op);
    }

    fn for_binary_op<F>(&self, bin_op: &mut Expression<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        if let Expression::BinaryOp(ref mut annotation, _op, l, r) = bin_op {
            f(annotation);
            self.for_expression(l, f);
            self.for_expression(r, f);
        } else {
            panic!("Expected BinaryOp, but got {:?}", bin_op)
        }
        self.tracer(bin_op);
    }

    fn for_if<F>(&self, if_exp: &mut Expression<A>, mut f: F)
    where
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
        self.tracer(if_exp);
    }

    fn for_routine_call<F>(&self, rc: &mut Expression<A>, mut f: F)
    where
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
        self.tracer(rc);
    }

    fn for_yield<F>(&self, yield_exp: &mut Expression<A>, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        if let Expression::Yield(ref mut annotation, e) = yield_exp {
            f(annotation);
            self.for_expression(e, f);
        } else {
            panic!("Expected Yield, but got {:?}", yield_exp)
        }
        self.tracer(yield_exp);
    }

    fn for_struct_expression<F>(&self, se: &mut Expression<A>, mut f: F)
    where
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
        self.tracer(se);
    }

    fn tracer<N>(&self, node: &N)
    where
        N: Node<A> + Display,
    {
        let annotation = node.annotation();
        let line = annotation.line() as usize;

        let print_trace = match &self.tracing {
            &TracingConfig::Only(ln) => (line) == ln,
            &TracingConfig::Before(ln) => (line) <= ln,
            &TracingConfig::After(ln) => (line) >= ln,
            &TracingConfig::Between(start, end) => (line) >= start && (line) <= end,
            &TracingConfig::All => true,
            &TracingConfig::Off => false,
        };
        let msg = (self.format)(annotation);
        if print_trace {
            println!("L{}n{}[{}]: {}", line, annotation.id(), node, msg,)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{module::Module, ty::Type};

    #[test]
    fn empty_module() {
        let mut m = Module::new("m", 1);

        let t = ForEachPreOrderMut::new("test", TracingConfig::Off, |_| "test".into());
        t.for_module(&mut m, |n| *n = 2);

        assert_eq!(*m.annotation(), 2);
    }

    #[test]
    fn module_with_items() {
        let mut m = Module::new("m", 1);
        m.add_function(RoutineDef::new_coroutine(
            "cor",
            1,
            vec![],
            Type::Unit,
            vec![Statement::Expression(box Expression::Integer64(1, 2))],
        ))
        .unwrap();
        m.add_function(RoutineDef::new_function(
            "func",
            1,
            vec![Parameter {
                annotation: 1,
                name: "p".into(),
                ty: Type::Bool,
            }],
            Type::Unit,
            vec![Statement::Expression(box Expression::Integer64(1, 2))],
        ))
        .unwrap();
        m.add_module(Module::new("m2", 1));
        m.add_struct(StructDef::new("sd", 1, vec![])).unwrap();

        let t = ForEachPreOrderMut::new("test", TracingConfig::Off, |_| "test".into());
        t.for_module(&mut m, |n| *n = 2);

        for n in m.iter_preorder() {
            assert_eq!(*n.annotation(), 2);
        }
    }
}
