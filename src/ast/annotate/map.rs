use fmt::Debug;
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

pub struct Map<A, B, F>
where
    A: Debug + Annotation,
    F: FnMut(&dyn Node<A>) -> B + Copy,
{
    pub name: String,
    pub tracing: TracingConfig,
    ph: PhantomData<A>,
    ph2: PhantomData<B>,
    ph3: PhantomData<F>,
}

impl<A, B, F> Map<A, B, F>
where
    A: Debug + Annotation,
    B: Debug + Annotation,
    F: FnMut(&dyn Node<A>) -> B + Copy,
{
    pub fn new(name: &str, tracing: TracingConfig, f: F) -> Map<A, B, F> {
        Map {
            name: name.into(),
            tracing,
            ph: PhantomData,
            ph2: PhantomData,
            ph3: PhantomData,
        }
    }

    pub fn for_module(&self, m: &Module<A>, mut f: F) -> Module<B> {
        if self.tracing != TracingConfig::Off {
            println!("{}", self.name);
        }

        let b = f(m);
        let mut m2 = Module::new(m.get_name(), b);

        for child_module in m.get_modules().iter() {
            m2.add_module(self.for_module(child_module, f));
        }

        m2.get_functions_mut()
            .append(&mut self.for_items(m.get_functions(), f));
        m2.get_coroutines_mut()
            .append(&mut self.for_items(m.get_coroutines(), f));
        m2.get_structs_mut()
            .append(&mut self.for_items(m.get_structs(), f));

        m2
    }

    fn for_items(&self, items: &Vec<Item<A>>, f: F) -> Vec<Item<B>> {
        let mut v = vec![];
        for i in items.iter() {
            v.push(match i {
                Item::Struct(sd) => Item::Struct(self.for_structdef(sd, f)),
                Item::Routine(rd) => Item::Routine(self.for_routinedef(rd, f)),
            });
        }
        v
    }

    fn for_structdef(&self, sd: &StructDef<A>, mut f: F) -> StructDef<B> {
        let b = f(sd);
        let fields = self.for_parameters(&sd.fields, f);
        StructDef::new(sd.get_name(), b, fields)
    }

    fn for_routinedef(&self, rd: &RoutineDef<A>, mut f: F) -> RoutineDef<B> {
        let b = f(rd);
        // loop through all the params
        let params = self.for_parameters(&rd.params, f);

        // loop through every statement and analyze the child nodes of the routine definition
        let mut body = vec![];
        for e in rd.get_body().iter() {
            body.push(self.for_statement(e, f));
        }

        RoutineDef {
            name: rd.name.clone(),
            def: rd.def,
            annotations: b,
            params,
            ty: rd.ty.clone(),
            body,
        }
    }

    fn for_parameters(&self, params: &Vec<Parameter<A>>, mut f: F) -> Vec<Parameter<B>> {
        let mut nparams = vec![];
        for p in params {
            let b = f(p);
            nparams.push(Parameter::new(b, &p.name, &p.ty));
        }
        nparams
    }

    fn for_statement(&self, statement: &Statement<A>, f: F) -> Statement<B> {
        let s = match statement {
            Statement::Bind(b) => Statement::Bind(box self.for_bind(b, f)),
            Statement::Mutate(m) => Statement::Mutate(box self.for_mutate(m, f)),
            Statement::Return(r) => Statement::Return(box self.for_return(r, f)),
            Statement::YieldReturn(yr) => Statement::YieldReturn(box self.for_yieldreturn(yr, f)),
            Statement::Expression(e) => Statement::Expression(box self.for_expression(e, f)),
        };
        s
    }

    fn for_bind(&self, bind: &Bind<A>, mut f: F) -> Bind<B> {
        let b = f(bind);
        let rhs = self.for_expression(bind.get_rhs(), f);
        Bind::new(
            b,
            bind.get_id(),
            bind.get_type().clone(),
            bind.is_mutable(),
            rhs,
        )
    }

    fn for_mutate(&self, mutate: &Mutate<A>, mut f: F) -> Mutate<B> {
        let b = f(mutate);
        let rhs = self.for_expression(mutate.get_rhs(), f);
        Mutate::new(b, mutate.get_id(), rhs)
    }

    fn for_yieldreturn(&self, yr: &YieldReturn<A>, mut f: F) -> YieldReturn<B> {
        let b = f(yr);
        let value = yr.get_value().as_ref().map(|rv| self.for_expression(rv, f));
        YieldReturn::new(b, value)
    }

    fn for_return(&self, r: &Return<A>, mut f: F) -> Return<B> {
        let b = f(r);
        let value = r.get_value().as_ref().map(|rv| self.for_expression(rv, f));
        Return::new(b, value)
    }

    fn for_expression(&self, exp: &Expression<A>, mut f: F) -> Expression<B> {
        use Expression::*;

        match exp {
            Expression::Integer32(_, i) => Integer32(f(exp), *i),
            Expression::Integer64(_, i) => Integer64(f(exp), *i),
            Expression::Boolean(_, b) => Boolean(f(exp), *b),
            Expression::StringLiteral(_, s) => StringLiteral(f(exp), s.clone()),
            Expression::CustomType(_, name) => CustomType(f(exp), name.clone()),
            Expression::Identifier(_, id) => Identifier(f(exp), id.clone()),
            Path(_, path) => Path(f(exp), path.clone()),
            Expression::IdentifierDeclare(_, id, p) => {
                IdentifierDeclare(f(exp), id.clone(), p.clone())
            }
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

    fn for_expression_block(&self, block: &Expression<A>, mut f: F) -> Expression<B> {
        if let Expression::ExpressionBlock(_, body, final_exp) = block {
            let b = f(block);

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

    fn for_member_access(&self, access: &Expression<A>, mut f: F) -> Expression<B> {
        if let Expression::MemberAccess(_, src, member) = access {
            let b = f(access);
            let src = self.for_expression(src, f);
            Expression::MemberAccess(b, box src, member.clone())
        } else {
            panic!("Expected MemberAccess, but got {:?}", access)
        }
    }

    fn for_unary_op(&self, un_op: &Expression<A>, mut f: F) -> Expression<B> {
        if let Expression::UnaryOp(_, op, operand) = un_op {
            let b = f(un_op);
            let operand = self.for_expression(operand, f);
            Expression::UnaryOp(b, *op, box operand)
        } else {
            panic!("Expected UnaryOp, but got {:?}", un_op)
        }
    }

    fn for_binary_op(&self, bin_op: &Expression<A>, mut f: F) -> Expression<B> {
        if let Expression::BinaryOp(_, op, l, r) = bin_op {
            let b = f(bin_op);
            let l = self.for_expression(l, f);
            let r = self.for_expression(r, f);
            Expression::BinaryOp(b, *op, box l, box r)
        } else {
            panic!("Expected BinaryOp, but got {:?}", bin_op)
        }
    }

    fn for_if(&self, if_exp: &Expression<A>, mut f: F) -> Expression<B> {
        if let Expression::If {
            cond,
            if_arm,
            else_arm,
            ..
        } = if_exp
        {
            let b = f(if_exp);
            let cond = self.for_expression(cond, f);
            let if_arm = self.for_expression(if_arm, f);
            let else_arm = else_arm.as_ref().map(|ea| box self.for_expression(ea, f));
            Expression::If {
                annotation: b,
                cond: box cond,
                if_arm: box if_arm,
                else_arm,
            }
        } else {
            panic!("Expected IfExpression, but got {:?}", if_exp)
        }
    }

    fn for_routine_call(&self, rc: &Expression<A>, mut f: F) -> Expression<B> {
        if let Expression::RoutineCall(_, call, name, params) = rc {
            let b = f(rc);
            let mut nparams = vec![];
            for p in params {
                nparams.push(self.for_expression(p, f));
            }
            Expression::RoutineCall(b, *call, name.clone(), nparams)
        } else {
            panic!("Expected RoutineCall, but got {:?}", rc)
        }
    }

    fn for_yield(&self, yield_exp: &Expression<A>, mut f: F) -> Expression<B> {
        if let Expression::Yield(_, e) = yield_exp {
            let b = f(yield_exp);
            let e = self.for_expression(e, f);
            Expression::Yield(b, box e)
        } else {
            panic!("Expected Yield, but got {:?}", yield_exp)
        }
    }

    fn for_struct_expression(&self, se: &Expression<A>, mut f: F) -> Expression<B> {
        if let Expression::StructExpression(_, struct_name, fields) = se {
            let b = f(se);
            let mut nfields = vec![];
            for (fname, fe) in fields {
                nfields.push((fname.clone(), self.for_expression(fe, f)));
            }
            Expression::StructExpression(b, struct_name.clone(), nfields)
        } else {
            panic!("Expected StructExpression, but got {:?}", se)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::module::Module;

    impl Annotation for i32 {
        fn id(&self) -> u32 {
            0
        }

        fn line(&self) -> u32 {
            0
        }
    }

    impl Annotation for i64 {
        fn id(&self) -> u32 {
            0
        }

        fn line(&self) -> u32 {
            0
        }
    }

    #[test]
    fn test() {
        let module1 = Module::new("m", 1i32);
        fn convert(n: &dyn Node<i32>) -> i64 {
            let i = n.annotation();
            *i as i64
        }
        let mut f = |n| convert(n);
        let mp = Map::new("test", TracingConfig::Off, f);
    }
}