use std::fmt::Debug;

use crate::ast::routinedef::*;
use crate::ast::statement::*;
use crate::ast::structdef::*;
use crate::ast::Expression;
use crate::diagnostics::config::TracingConfig;
use crate::{
    ast::module::*,
    diagnostics::{Diag, DiagRecorder},
};

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
    A: Debug + Annotation + Diag,
    T: Fn(&A) -> String,
{
    pub name: String,
    pub tracing: TracingConfig,
    pub format: T,
    diag: DiagRecorder<A, A>,
}

impl<A, T> ForEachPreOrderMut<A, T>
where
    A: Debug + Annotation + Diag,
    T: Fn(&A) -> String,
{
    pub fn new(name: &str, tracing: TracingConfig, format: T) -> ForEachPreOrderMut<A, T> {
        ForEachPreOrderMut {
            name: name.into(),
            tracing,
            format,
            diag: DiagRecorder::new(name, tracing),
        }
    }

    fn transform<F>(&self, a: &mut A, mut f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.diag.begin(a);
        f(a);
        self.diag.end(a);
    }

    pub fn for_each<F>(&self, m: &mut Module<A>, f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.diag.start_trace();
        self.for_module(m, f);
        self.diag.end_trace();
    }

    fn for_module<F>(&self, m: &mut Module<A>, f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        if self.tracing != TracingConfig::Off {
            println!("{}", self.name);
        }

        self.transform(m.annotation_mut(), f);

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

    fn for_structdef<F>(&self, sd: &mut StructDef<A>, f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.transform(sd.annotation_mut(), f);
        self.for_parameters(&mut sd.fields, f);
    }

    fn for_routinedef<F>(&self, rd: &mut RoutineDef<A>, f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.transform(rd.annotation_mut(), f);
        // loop through all the params
        self.for_parameters(&mut rd.params, f);

        // loop through every statement and analyze the child nodes of the routine definition
        for e in rd.get_body_mut().iter_mut() {
            self.for_statement(e, f);
        }
    }

    fn for_parameters<F>(&self, params: &mut Vec<Parameter<A>>, f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        for p in params {
            self.transform(p.annotation_mut(), f);
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
    }

    fn for_bind<F>(&self, bind: &mut Bind<A>, f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.transform(bind.annotation_mut(), f);
        self.for_expression(bind.get_rhs_mut(), f)
    }

    fn for_mutate<F>(&self, mutate: &mut Mutate<A>, f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.transform(mutate.annotation_mut(), f);
        self.for_expression(mutate.get_rhs_mut(), f);
    }

    fn for_yieldreturn<F>(&self, yr: &mut YieldReturn<A>, f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.transform(yr.annotation_mut(), f);
        yr.get_value_mut()
            .as_mut()
            .map(|rv| self.for_expression(rv, f));
    }

    fn for_return<F>(&self, r: &mut Return<A>, f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        self.transform(r.annotation_mut(), f);
        r.get_value_mut()
            .as_mut()
            .map(|rv| self.for_expression(rv, f));
    }

    fn for_expression<F>(&self, exp: &mut Expression<A>, f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        use Expression::*;

        match exp {
            ExpressionBlock(..) => self.for_expression_block(exp, f),
            Expression::Integer32(ref mut annotation, _i) => self.transform(annotation, f),
            Expression::Integer64(ref mut annotation, _i) => self.transform(annotation, f),
            Expression::Boolean(ref mut annotation, _b) => self.transform(annotation, f),
            Expression::StringLiteral(ref mut annotation, _s) => self.transform(annotation, f),
            Expression::CustomType(ref mut annotation, _name) => self.transform(annotation, f),
            Expression::Identifier(ref mut annotation, _id) => self.transform(annotation, f),
            Path(ref mut annotation, _path) => self.transform(annotation, f),
            Expression::IdentifierDeclare(ref mut annotation, _id, _p) => {
                self.transform(annotation, f)
            }
            MemberAccess(..) => self.for_member_access(exp, f),
            UnaryOp(..) => self.for_unary_op(exp, f),
            BinaryOp(..) => self.for_binary_op(exp, f),
            If { .. } => self.for_if(exp, f),
            Yield(..) => self.for_yield(exp, f),
            RoutineCall(..) => self.for_routine_call(exp, f),
            StructExpression(..) => self.for_struct_expression(exp, f),
        }
    }

    fn for_expression_block<F>(&self, block: &mut Expression<A>, f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        if let Expression::ExpressionBlock(ref mut annotation, ref mut body, ref mut final_exp) =
            block
        {
            self.transform(annotation, f);

            for e in body.iter_mut() {
                self.for_statement(e, f);
            }

            final_exp.as_mut().map(|fe| self.for_expression(fe, f));
        } else {
            panic!("Expected ExpressionBlock, but got {:?}", block)
        }
    }

    fn for_member_access<F>(&self, access: &mut Expression<A>, f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        if let Expression::MemberAccess(ref mut annotation, src, _member) = access {
            self.transform(annotation, f);
            self.for_expression(src, f);
        } else {
            panic!("Expected MemberAccess, but got {:?}", access)
        }
    }

    fn for_unary_op<F>(&self, un_op: &mut Expression<A>, f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        if let Expression::UnaryOp(ref mut annotation, _op, operand) = un_op {
            self.transform(annotation, f);
            self.for_expression(operand, f);
        } else {
            panic!("Expected UnaryOp, but got {:?}", un_op)
        }
    }

    fn for_binary_op<F>(&self, bin_op: &mut Expression<A>, f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        if let Expression::BinaryOp(ref mut annotation, _op, l, r) = bin_op {
            self.transform(annotation, f);
            self.for_expression(l, f);
            self.for_expression(r, f);
        } else {
            panic!("Expected BinaryOp, but got {:?}", bin_op)
        }
    }

    fn for_if<F>(&self, if_exp: &mut Expression<A>, f: F)
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
            self.transform(annotation, f);
            self.for_expression(cond, f);
            self.for_expression(if_arm, f);
            else_arm.as_mut().map(|ea| self.for_expression(ea, f));
        } else {
            panic!("Expected IfExpression, but got {:?}", if_exp)
        }
    }

    fn for_routine_call<F>(&self, rc: &mut Expression<A>, f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        if let Expression::RoutineCall(ref mut annotation, _call, _name, params) = rc {
            self.transform(annotation, f);
            for p in params {
                self.for_expression(p, f);
            }
        } else {
            panic!("Expected RoutineCall, but got {:?}", rc)
        }
    }

    fn for_yield<F>(&self, yield_exp: &mut Expression<A>, f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        if let Expression::Yield(ref mut annotation, e) = yield_exp {
            self.transform(annotation, f);
            self.for_expression(e, f);
        } else {
            panic!("Expected Yield, but got {:?}", yield_exp)
        }
    }

    fn for_struct_expression<F>(&self, se: &mut Expression<A>, f: F)
    where
        F: FnMut(&mut A) + Copy,
    {
        if let Expression::StructExpression(ref mut annotation, _struct_name, fields) = se {
            self.transform(annotation, f);
            for (_, fe) in fields {
                self.for_expression(fe, f);
            }
        } else {
            panic!("Expected StructExpression, but got {:?}", se)
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
