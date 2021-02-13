use fmt::Debug;
use std::{fmt, marker::PhantomData};

use crate::ast::module::*;
use crate::ast::routinedef::*;
use crate::ast::statement::*;
use crate::ast::structdef::*;
use crate::expression::Expression;

use super::{super::node::Node, super::parameter::Parameter, Annotation};

/**
 * This traverses the AST and determines what size register to
 * assign to each node in the AST: if it makes sense to assign
 * it to a register.
 */

pub struct MapPreOrder<A, B, F>
where
    A: Debug + Annotation,
    F: FnMut(&dyn Node<A>) -> B,
{
    pub name: String,
    f: PhantomData<F>,
    ph: PhantomData<A>,
    ph2: PhantomData<B>,
}

impl<A, B, F> MapPreOrder<A, B, F>
where
    A: Debug + Annotation,
    B: Debug + Annotation,
    F: FnMut(&dyn Node<A>) -> B,
{
    pub fn new(name: &str) -> MapPreOrder<A, B, F> {
        MapPreOrder {
            name: name.into(),
            f: PhantomData,
            ph: PhantomData,
            ph2: PhantomData,
        }
    }

    pub fn for_module(&self, m: &Module<A>, f: &mut F) -> Module<B> {
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

    fn for_items(&self, items: &Vec<Item<A>>, f: &mut F) -> Vec<Item<B>> {
        let mut v = vec![];
        for i in items.iter() {
            v.push(match i {
                Item::Struct(sd) => Item::Struct(self.for_structdef(sd, f)),
                Item::Routine(rd) => Item::Routine(self.for_routinedef(rd, f)),
            });
        }
        v
    }

    fn for_structdef(&self, sd: &StructDef<A>, f: &mut F) -> StructDef<B> {
        let b = f(sd);
        let fields = self.for_parameters(&sd.fields, f);
        StructDef::new(sd.get_name(), b, fields)
    }

    fn for_routinedef(&self, rd: &RoutineDef<A>, f: &mut F) -> RoutineDef<B> {
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

    fn for_parameters(&self, params: &Vec<Parameter<A>>, f: &mut F) -> Vec<Parameter<B>> {
        let mut nparams = vec![];
        for p in params {
            let b = f(p);
            nparams.push(Parameter::new(b, &p.name, &p.ty));
        }
        nparams
    }

    fn for_statement(&self, statement: &Statement<A>, f: &mut F) -> Statement<B> {
        let s = match statement {
            Statement::Bind(b) => Statement::Bind(box self.for_bind(b, f)),
            Statement::Mutate(m) => Statement::Mutate(box self.for_mutate(m, f)),
            Statement::Return(r) => Statement::Return(box self.for_return(r, f)),
            Statement::YieldReturn(yr) => Statement::YieldReturn(box self.for_yieldreturn(yr, f)),
            Statement::Expression(e) => Statement::Expression(box self.for_expression(e, f)),
        };
        s
    }

    fn for_bind(&self, bind: &Bind<A>, f: &mut F) -> Bind<B> {
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

    fn for_mutate(&self, mutate: &Mutate<A>, f: &mut F) -> Mutate<B> {
        let b = f(mutate);
        let rhs = self.for_expression(mutate.get_rhs(), f);
        Mutate::new(b, mutate.get_id(), rhs)
    }

    fn for_yieldreturn(&self, yr: &YieldReturn<A>, f: &mut F) -> YieldReturn<B> {
        let b = f(yr);
        let value = yr.get_value().as_ref().map(|rv| self.for_expression(rv, f));
        YieldReturn::new(b, value)
    }

    fn for_return(&self, r: &Return<A>, f: &mut F) -> Return<B> {
        let b = f(r);
        let value = r.get_value().as_ref().map(|rv| self.for_expression(rv, f));
        Return::new(b, value)
    }

    fn for_expression(&self, exp: &Expression<A>, f: &mut F) -> Expression<B> {
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

    fn for_expression_block(&self, block: &Expression<A>, f: &mut F) -> Expression<B> {
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

    fn for_member_access(&self, access: &Expression<A>, f: &mut F) -> Expression<B> {
        if let Expression::MemberAccess(_, src, member) = access {
            let b = f(access);
            let src = self.for_expression(src, f);
            Expression::MemberAccess(b, box src, member.clone())
        } else {
            panic!("Expected MemberAccess, but got {:?}", access)
        }
    }

    fn for_unary_op(&self, un_op: &Expression<A>, f: &mut F) -> Expression<B> {
        if let Expression::UnaryOp(_, op, operand) = un_op {
            let b = f(un_op);
            let operand = self.for_expression(operand, f);
            Expression::UnaryOp(b, *op, box operand)
        } else {
            panic!("Expected UnaryOp, but got {:?}", un_op)
        }
    }

    fn for_binary_op(&self, bin_op: &Expression<A>, f: &mut F) -> Expression<B> {
        if let Expression::BinaryOp(_, op, l, r) = bin_op {
            let b = f(bin_op);
            let l = self.for_expression(l, f);
            let r = self.for_expression(r, f);
            Expression::BinaryOp(b, *op, box l, box r)
        } else {
            panic!("Expected BinaryOp, but got {:?}", bin_op)
        }
    }

    fn for_if(&self, if_exp: &Expression<A>, f: &mut F) -> Expression<B> {
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

    fn for_routine_call(&self, rc: &Expression<A>, f: &mut F) -> Expression<B> {
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

    fn for_yield(&self, yield_exp: &Expression<A>, f: &mut F) -> Expression<B> {
        if let Expression::Yield(_, e) = yield_exp {
            let b = f(yield_exp);
            let e = self.for_expression(e, f);
            Expression::Yield(b, box e)
        } else {
            panic!("Expected Yield, but got {:?}", yield_exp)
        }
    }

    fn for_struct_expression(&self, se: &Expression<A>, f: &mut F) -> Expression<B> {
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

    fn convert(n: &dyn Node<i32>) -> i64 {
        let i = n.annotation();
        2 * (*i as i64)
    }

    #[test]
    fn empty_module() {
        let module1 = Module::new("m", 1i32);
        let mut count: i32 = 0;
        let mut f = |n: &dyn Node<i32>| {
            count += 1;
            convert(n)
        };

        let mp = MapPreOrder::new("test");
        let module2 = mp.for_module(&module1, &mut f);

        assert_eq!(*module2.annotation(), 2i64);
        assert_eq!(count, 1);
    }

    #[test]
    fn nested_module() {
        let mut module1 = Module::new("m", 1i32);
        module1.add_module(Module::new("m2", 2i32));

        let mut count: i32 = 0;
        let mut f = |n: &dyn Node<i32>| {
            count += 1;
            convert(n)
        };

        let mp = MapPreOrder::new("test");
        let module2 = mp.for_module(&module1, &mut f);

        assert_eq!(*module2.annotation(), 2i64);
        assert_eq!(*module2.get_module("m2").unwrap().annotation(), 4i64);
        assert_eq!(count, 2);
    }
}
