use fmt::{Debug, Display};
use std::{fmt, marker::PhantomData};

use crate::ast::module::*;
use crate::ast::routinedef::*;
use crate::ast::statement::*;
use crate::ast::structdef::*;
use crate::{diagnostics::config::TracingConfig, expression::Expression};

use super::{super::node::Node, super::parameter::Parameter, Annotation};

/**
 * This traverses the AST and determines what size register to
 * assign to each node in the AST: if it makes sense to assign
 * it to a register.
 */

pub struct Map<A, B, T>
where
    A: Debug + Annotation,
    T: Fn(&A) -> String,
{
    pub name: String,
    pub tracing: TracingConfig,
    pub format: T,
    ph: PhantomData<A>,
    ph2: PhantomData<B>,
}

impl<A, B, T> Map<A, B, T>
where
    A: Debug + Annotation,
    T: Fn(&A) -> String,
{
    pub fn new(name: &str, tracing: TracingConfig, format: T) -> Map<A, B, T> {
        Map {
            name: name.into(),
            tracing,
            format,
            ph: PhantomData,
            ph2: PhantomData,
        }
    }

    pub fn for_module<F>(&self, m: &Module<A>, mut f: F) -> Module<B>
    where
        F: FnMut(&A) -> B + Copy,
    {
        if self.tracing != TracingConfig::Off {
            println!("{}", self.name);
        }

        let b = f(m.annotation());
        let mut m2 = Module::new(m.get_name(), b);

        for child_module in m.get_modules().iter() {
            m2.add_module(self.for_module(child_module, f));
        }

        m2.get_functions_mut().append(&mut self.for_items(m.get_functions(), f));
        m2.get_coroutines_mut().append(&mut self.for_items(m.get_coroutines(), f));
        m2.get_structs_mut().append(&mut self.for_items(m.get_structs(), f));

        m2
    }

    fn for_items<F>(&self, items: &Vec<Item<A>>, f: F) -> Vec<Item<B>>
    where
        F: FnMut(&A) -> B + Copy,
    {
        let mut v = vec![];
        for i in items.iter() {
            v.push(match i {
                Item::Struct(sd) => {
                    Item::Struct(self.for_structdef(sd, f))
                }
                Item::Routine(rd) => {
                    Item::Routine(self.for_routinedef(rd, f))
                }
            });
            self.tracer(i);
        }
        v
    }

    fn for_structdef<F>(&self, sd: &StructDef<A>, mut f: F) -> StructDef<B>
    where
        F: FnMut(&A) -> B + Copy,
    {
        let b = f(sd.annotation());
        self.tracer(sd);
        let fields = self.for_parameters(&sd.fields, f);
        StructDef::new(sd.get_name(), b, fields)
    }

    fn for_routinedef<F>(&self, rd: &RoutineDef<A>, mut f: F) -> RoutineDef<B>
    where
        F: FnMut(&A) -> B + Copy,
    {
        let b = f(rd.annotation());
        // loop through all the params
        let params = self.for_parameters(&rd.params, f);

        // loop through every statement and analyze the child nodes of the routine definition
        let mut body = vec![];
        for e in rd.get_body().iter() {
            body.push(self.for_statement(e, f));
        }

        self.tracer(rd);
        RoutineDef{
            name: rd.name.clone(),
            def: rd.def,
            annotations: b,
            params,
            ty: rd.ty.clone(),
            body,
        }
    }

    fn for_parameters<F>(&self, params: &Vec<Parameter<A>>, mut f: F) -> Vec<Parameter<B>>
    where
        F: FnMut(&A) -> B + Copy,
    {
        let mut nparams = vec![];
        for p in params {
            let b = f(p.annotation());
            nparams.push(Parameter::new(b, &p.name, &p.ty));
            self.tracer(p);
        }
        nparams
    }

    fn for_statement<F>(&self, statement: &Statement<A>, f: F) -> Statement<B>
    where
        F: FnMut(&A) -> B + Copy,
    {
        let s = match statement {
            Statement::Bind(b) => {
                Statement::Bind(box self.for_bind(b, f))
            }
            Statement::Mutate(m) => {
                Statement::Mutate(box self.for_mutate(m, f))
            }
            Statement::Return(r) => {
                Statement::Return(box self.for_return(r, f))
            }
            Statement::YieldReturn(yr) => {
                Statement::YieldReturn(box self.for_yieldreturn(yr, f))
            }
            Statement::Expression(e) => {
                Statement::Expression(box self.for_expression(e, f))
            }
        };
        self.tracer(statement);
        s
    }

    fn for_bind<F>(&self, bind: &Bind<A>, mut f: F) -> Bind<B>
    where
        F: FnMut(&A) -> B + Copy,
    {
        let b = f(bind.annotation());
        let rhs = self.for_expression(bind.get_rhs(), f);
        Bind::new(b, bind.get_id(), bind.get_type().clone(), bind.is_mutable(), rhs)
    }

    fn for_mutate<F>(&self, mutate: &Mutate<A>, mut f: F) -> Mutate<B>
    where
        F: FnMut(&A) -> B + Copy,
    {
        let b = f(mutate.annotation());
        let rhs = self.for_expression(mutate.get_rhs(), f);
        Mutate::new(b, mutate.get_id(), rhs)
    }

    fn for_yieldreturn<F>(&self, yr: &YieldReturn<A>, mut f: F) -> YieldReturn<B>
    where
        F: FnMut(&A) -> B + Copy,
    {
        let b = f(yr.annotation());
        let value = yr.get_value()
            .as_ref()
            .map(|rv| self.for_expression(rv, f));
        YieldReturn::new(b, value)
    }

    fn for_return<F>(&self, r: &Return<A>, mut f: F) -> Return<B>
    where
        F: FnMut(&A) -> B + Copy,
    {
        let b = f(r.annotation());
        let value = r.get_value()
            .as_ref()
            .map(|rv| self.for_expression(rv, f));
        Return::new(b, value)
    }

    fn for_expression<F>(&self, exp: &Expression<A>, mut f: F) -> Expression<B>
    where
        F: FnMut(&A) -> B + Copy,
    {
        use Expression::*;

        match exp {
            Expression::Integer32(annotation, i) => Integer32(f(annotation), *i),
            Expression::Integer64(annotation, i) => Integer64(f(annotation), *i),
            Expression::Boolean(annotation, b) => Boolean(f(annotation), *b),
            Expression::StringLiteral(annotation, s) => StringLiteral(f(annotation), s.clone()),
            Expression::CustomType(annotation, name) => CustomType(f(annotation), name.clone()),
            Expression::Identifier(annotation, id) => Identifier(f(annotation), id.clone()),
            Path(annotation, path) => Path(f(annotation), path.clone()),
            Expression::IdentifierDeclare(annotation, id, p) => IdentifierDeclare(f(annotation), id.clone(), p.clone()),
            MemberAccess(..) => self.for_member_access(exp, f),
            UnaryOp(..) => self.for_unary_op(exp, f),
            BinaryOp(..) => self.for_binary_op(exp, f),
            If { .. } => self.for_if(exp, f),
            Yield(..) => self.for_yield(exp, f),
            RoutineCall(..) => self.for_routine_call(exp, f),
            StructExpression(..) => self.for_struct_expression(exp, f),
            ExpressionBlock(..) => self.for_expression_block(exp, f),
        }
    }

    fn for_expression_block<F>(&self, block: &Expression<A>, mut f: F) -> Expression<B>
    where
        F: FnMut(&A) -> B + Copy,
    {
        if let Expression::ExpressionBlock(annotation, body, final_exp) =
            block
        {
            let b = f(annotation);

            let mut nbody = vec![];
            for e in body.iter() {
                nbody.push(self.for_statement(e, f));
            }

            let final_exp = final_exp.as_ref().map(|fe| box self.for_expression(fe, f));
            Expression::ExpressionBlock(b, nbody, final_exp)
        } else {
            panic!("Expected ExpressionBlock, but got {:?}", block)
        }
    }

    fn for_member_access<F>(&self, access: &Expression<A>, mut f: F) -> Expression<B>
    where
        F: FnMut(&A) -> B + Copy,
    {
        if let Expression::MemberAccess(annotation, src, member) = access {
            let b = f(annotation);
            let src = self.for_expression(src, f);
            Expression::MemberAccess(b, box src, member.clone())
        } else {
            panic!("Expected MemberAccess, but got {:?}", access)
        }
    }

    fn for_unary_op<F>(&self, un_op: &Expression<A>, mut f: F) -> Expression<B>
    where
        F: FnMut(&A) -> B + Copy,
    {
        if let Expression::UnaryOp(annotation, op, operand) = un_op {
            let b = f(annotation);
            let operand = self.for_expression(operand, f);
            Expression::UnaryOp(b, *op, box operand)
        } else {
            panic!("Expected UnaryOp, but got {:?}", un_op)
        }
    }

    fn for_binary_op<F>(&self, bin_op: &Expression<A>, mut f: F) -> Expression<B>
    where
        F: FnMut(&A) -> B + Copy,
    {
        if let Expression::BinaryOp(annotation, op, l, r) = bin_op {
            let b = f(annotation);
            let l = self.for_expression(l, f);
            let r = self.for_expression(r, f);
            Expression::BinaryOp(b, *op, box l, box r)
        } else {
            panic!("Expected BinaryOp, but got {:?}", bin_op)
        }
    }

    fn for_if<F>(&self, if_exp: &Expression<A>, mut f: F) -> Expression<B>
    where
        F: FnMut(&A) -> B + Copy,
    {
        if let Expression::If {
            annotation,
            cond,
            if_arm,
            else_arm,
        } = if_exp
        {
            let b = f(annotation);
            let cond = self.for_expression(cond, f);
            let if_arm = self.for_expression(if_arm, f);
            let else_arm = else_arm.as_ref().map(|ea| box self.for_expression(ea, f));
            Expression::If{annotation: b, cond: box cond,if_arm:  box if_arm, else_arm}
        } else {
            panic!("Expected IfExpression, but got {:?}", if_exp)
        }
    }

    fn for_routine_call<F>(&self, rc: &Expression<A>, mut f: F) -> Expression<B>
    where
        F: FnMut(&A) -> B + Copy,
    {
        if let Expression::RoutineCall(annotation, call, name, params) = rc {
            let b = f(annotation);
            let mut nparams = vec![];
            for p in params {
                nparams.push(self.for_expression(p, f));
            }
            Expression::RoutineCall(b, *call, name.clone(), nparams)
        } else {
            panic!("Expected RoutineCall, but got {:?}", rc)
        }
    }

    fn for_yield<F>(&self, yield_exp: &Expression<A>, mut f: F) -> Expression<B>
    where
        F: FnMut(&A) -> B + Copy,
    {
        if let Expression::Yield(annotation, e) = yield_exp {
            let b = f(annotation);
            let e = self.for_expression(e, f);
            Expression::Yield(b, box e)
        } else {
            panic!("Expected Yield, but got {:?}", yield_exp)
        }
    }

    fn for_struct_expression<F>(&self, se: &Expression<A>, mut f: F) -> Expression<B>
    where
        F: FnMut(&A) -> B + Copy,
    {
        if let Expression::StructExpression(annotation, struct_name, fields) = se {
            let b = f(annotation);
            let mut nfields = vec![];
            for (fname, fe) in fields {
                nfields.push((fname.clone(), self.for_expression(fe, f)));
            }
            Expression::StructExpression(b, struct_name.clone(), nfields)
        } else {
            panic!("Expected StructExpression, but got {:?}", se)
        }
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
