use fmt::Debug;
use std::{fmt, marker::PhantomData};

use crate::compiler::ast::module::*;
use crate::compiler::ast::routinedef::*;
use crate::compiler::ast::statement::*;
use crate::compiler::ast::structdef::*;
use crate::compiler::ast::Expression;
use crate::compiler::ast::Extern;

use super::{super::node::Node, super::parameter::Parameter, Context};

/**
Traverses an AST in using a PreOrder Depth First Search and transforms the AST by
applying a function, `f` to each node. This construct allows a user to transform
the value and type of the Annotation on an AST while preserving the AST topology.

Only Annotation data will be transformed.

Each instance of a construct is given a name so that diagnostic and investigative
data which is collected (to aid a developer with debugging the Braid compiler and
to aid users with debugging their Braid code) can be identified in the Braid
Compiler UI.
*/
pub struct MapPreOrder<A, B, F>
where
    A: Debug + Context,
    F: FnMut(&dyn Node<A>) -> B,
{
    pub name: String,
    f: F,
    ph: PhantomData<A>,
    ph2: PhantomData<B>,
}

impl<A, B, F> MapPreOrder<A, B, F>
where
    A: Debug + Context,
    F: FnMut(&dyn Node<A>) -> B,
    B: Context,
{
    pub fn new(name: &str, f: F) -> MapPreOrder<A, B, F> {
        MapPreOrder {
            name: name.into(),
            f,
            ph: PhantomData,
            ph2: PhantomData,
        }
    }

    fn transform(&mut self, n: &dyn Node<A>) -> B {
        //self.diag.begin(n.context());
        let b = (self.f)(n);
        //self.diag.end(&b);
        b
    }

    /**
    Applies the transformation function given to the constructor to each
    node in the given AST, in PreOrder DFS ordering, and creates a new
    AST using the output of the transformation function as the context
    of the new nodes.
    */
    pub fn apply(&mut self, m: &Module<A>) -> Module<B> {
        //self.diag.start_trace();
        let m = self.for_module(m);
        //self.diag.end_trace();
        m
    }

    fn for_module(&mut self, m: &Module<A>) -> Module<B> {
        let b = self.transform(m);
        let mut m2 = Module::new(m.get_name(), b);

        for child_module in m.get_modules().iter() {
            m2.add_module(self.for_module(child_module));
        }

        m2.get_functions_mut()
            .append(&mut self.for_items(m.get_functions()));
        m2.get_coroutines_mut()
            .append(&mut self.for_items(m.get_coroutines()));
        m2.get_structs_mut()
            .append(&mut self.for_items(m.get_structs()));
        m2.get_externs_mut()
            .append(&mut self.for_items(m.get_externs()));

        m2
    }

    fn for_items(&mut self, items: &Vec<Item<A>>) -> Vec<Item<B>> {
        let mut v = vec![];
        for i in items.iter() {
            v.push(match i {
                Item::Struct(sd) => Item::Struct(self.for_structdef(sd)),
                Item::Routine(rd) => Item::Routine(self.for_routinedef(rd)),
                Item::Extern(ex) => Item::Extern(self.for_extern(ex)),
            });
        }
        v
    }

    fn for_extern(&mut self, ex: &Extern<A>) -> Extern<B> {
        let b = self.transform(ex);
        let params = self.for_parameters(&ex.params);
        Extern::new(ex.get_name(), b, params, ex.has_varargs, ex.ty.clone())
    }

    fn for_structdef(&mut self, sd: &StructDef<A>) -> StructDef<B> {
        let b = self.transform(sd);
        let fields = self.for_parameters(&sd.fields);
        StructDef::new(sd.get_name(), b, fields)
    }

    fn for_routinedef(&mut self, rd: &RoutineDef<A>) -> RoutineDef<B> {
        let b = self.transform(rd);
        // loop through all the params
        let params = self.for_parameters(&rd.params);

        // loop through every statement and analyze the child nodes of the routine definition
        let mut body = vec![];
        for e in rd.get_body().iter() {
            body.push(self.for_statement(e));
        }

        RoutineDef {
            name: rd.name.clone(),
            def: rd.def,
            context: b,
            params,
            ret_ty: rd.ret_ty.clone(),
            body,
        }
    }

    fn for_parameters(&mut self, params: &Vec<Parameter<A>>) -> Vec<Parameter<B>> {
        let mut nparams = vec![];
        for p in params {
            let b = self.transform(p);
            nparams.push(Parameter::new(b, p.name, &p.ty));
        }
        nparams
    }

    fn for_statement(&mut self, statement: &Statement<A>) -> Statement<B> {
        let s = match statement {
            Statement::Bind(b) => Statement::Bind(Box::new(self.for_bind(b))),
            Statement::Mutate(m) => Statement::Mutate(Box::new(self.for_mutate(m))),
            Statement::Return(r) => Statement::Return(Box::new(self.for_return(r))),
            Statement::YieldReturn(yr) => {
                Statement::YieldReturn(Box::new(self.for_yieldreturn(yr)))
            }
            Statement::Expression(e) => Statement::Expression(Box::new(self.for_expression(e))),
        };
        s
    }

    fn for_bind(&mut self, bind: &Bind<A>) -> Bind<B> {
        let b = self.transform(bind);
        let rhs = self.for_expression(bind.get_rhs());
        Bind::new(
            b,
            bind.get_id(),
            bind.get_type().clone(),
            bind.is_mutable(),
            rhs,
        )
    }

    fn for_mutate(&mut self, mutate: &Mutate<A>) -> Mutate<B> {
        let b = self.transform(mutate);
        let rhs = self.for_expression(mutate.get_rhs());
        Mutate::new(b, mutate.get_id(), rhs)
    }

    fn for_yieldreturn(&mut self, yr: &YieldReturn<A>) -> YieldReturn<B> {
        let b = self.transform(yr);
        let value = yr.get_value().as_ref().map(|rv| self.for_expression(rv));
        YieldReturn::new(b, value)
    }

    fn for_return(&mut self, r: &Return<A>) -> Return<B> {
        let b = self.transform(r);
        let value = r.get_value().as_ref().map(|rv| self.for_expression(rv));
        Return::new(b, value)
    }

    fn for_expression(&mut self, exp: &Expression<A>) -> Expression<B> {
        use Expression::*;

        match exp {
            U8(_, i) => U8(self.transform(exp), *i),
            U16(_, i) => U16(self.transform(exp), *i),
            U32(_, i) => U32(self.transform(exp), *i),
            U64(_, i) => U64(self.transform(exp), *i),
            I8(_, i) => I8(self.transform(exp), *i),
            I16(_, i) => I16(self.transform(exp), *i),
            I32(_, i) => I32(self.transform(exp), *i),
            I64(_, i) => I64(self.transform(exp), *i),
            F64(_, f) => F64(self.transform(exp), *f),
            Boolean(_, b) => Boolean(self.transform(exp), *b),
            StringLiteral(_, s) => StringLiteral(self.transform(exp), s.clone()),
            ArrayExpression(_, _, _) => self.for_array_expression(exp),
            ArrayAt { .. } => self.for_array_at(exp),
            CustomType(_, name) => CustomType(self.transform(exp), name.clone()),
            Identifier(_, id) => Identifier(self.transform(exp), id.clone()),
            Path(_, path) => Path(self.transform(exp), path.clone()),
            IdentifierDeclare(_, id, p) => {
                IdentifierDeclare(self.transform(exp), id.clone(), p.clone())
            }
            MemberAccess(..) => self.for_member_access(exp),
            UnaryOp(..) => self.for_unary_op(exp),
            BinaryOp(..) => self.for_binary_op(exp),
            If { .. } => self.for_if(exp),
            While { .. } => self.for_while(exp),
            Yield(..) => self.for_yield(exp),
            RoutineCall(..) => self.for_routine_call(exp),
            StructExpression(..) => self.for_struct_expression(exp),
            ExpressionBlock(..) => self.for_expression_block(exp),
        }
    }

    fn for_expression_block(&mut self, block: &Expression<A>) -> Expression<B> {
        if let Expression::ExpressionBlock(_, body, final_exp) = block {
            let b = self.transform(block);

            let mut nbody = vec![];
            for e in body.iter() {
                nbody.push(self.for_statement(e));
            }

            let final_exp = final_exp
                .as_ref()
                .map(|fe| Box::new(self.for_expression(fe)));
            Expression::ExpressionBlock(b, nbody, final_exp)
        } else {
            panic!("Expected ExpressionBlock, but got {:?}", block)
        }
    }

    fn for_member_access(&mut self, access: &Expression<A>) -> Expression<B> {
        if let Expression::MemberAccess(_, src, member) = access {
            let b = self.transform(access);
            let src = self.for_expression(src);
            Expression::MemberAccess(b, Box::new(src), member.clone())
        } else {
            panic!("Expected MemberAccess, but got {:?}", access)
        }
    }

    fn for_unary_op(&mut self, un_op: &Expression<A>) -> Expression<B> {
        if let Expression::UnaryOp(_, op, operand) = un_op {
            let b = self.transform(un_op);
            let operand = self.for_expression(operand);
            Expression::UnaryOp(b, *op, Box::new(operand))
        } else {
            panic!("Expected UnaryOp, but got {:?}", un_op)
        }
    }

    fn for_binary_op(&mut self, bin_op: &Expression<A>) -> Expression<B> {
        if let Expression::BinaryOp(_, op, l, r) = bin_op {
            let b = self.transform(bin_op);
            let l = self.for_expression(l);
            let r = self.for_expression(r);
            Expression::BinaryOp(b, *op, Box::new(l), Box::new(r))
        } else {
            panic!("Expected BinaryOp, but got {:?}", bin_op)
        }
    }

    fn for_if(&mut self, if_exp: &Expression<A>) -> Expression<B> {
        if let Expression::If {
            cond,
            if_arm,
            else_arm,
            ..
        } = if_exp
        {
            let b = self.transform(if_exp);
            let cond = self.for_expression(cond);
            let if_arm = self.for_expression(if_arm);
            let else_arm = else_arm
                .as_ref()
                .map(|ea| Box::new(self.for_expression(ea)));
            Expression::If {
                context: b,
                cond: Box::new(cond),
                if_arm: Box::new(if_arm),
                else_arm,
            }
        } else {
            panic!("Expected IfExpression, but got {:?}", if_exp)
        }
    }

    fn for_while(&mut self, while_exp: &Expression<A>) -> Expression<B> {
        if let Expression::While { cond, body, .. } = while_exp {
            let b = self.transform(while_exp);
            let cond = self.for_expression(cond);
            let body = self.for_expression(body);
            Expression::While {
                context: b,
                cond: Box::new(cond),
                body: Box::new(body),
            }
        } else {
            panic!("Expected WhileExpression, but got {:?}", while_exp)
        }
    }

    fn for_routine_call(&mut self, rc: &Expression<A>) -> Expression<B> {
        if let Expression::RoutineCall(_, call, name, params) = rc {
            let b = self.transform(rc);
            let mut nparams = vec![];
            for p in params {
                nparams.push(self.for_expression(p));
            }
            Expression::RoutineCall(b, *call, name.clone(), nparams)
        } else {
            panic!("Expected RoutineCall, but got {:?}", rc)
        }
    }

    fn for_yield(&mut self, yield_exp: &Expression<A>) -> Expression<B> {
        if let Expression::Yield(_, e) = yield_exp {
            let b = self.transform(yield_exp);
            let e = self.for_expression(e);
            Expression::Yield(b, Box::new(e))
        } else {
            panic!("Expected Yield, but got {:?}", yield_exp)
        }
    }

    fn for_struct_expression(&mut self, se: &Expression<A>) -> Expression<B> {
        if let Expression::StructExpression(_, struct_name, fields) = se {
            let b = self.transform(se);
            let mut nfields = vec![];
            for (fname, fe) in fields {
                nfields.push((fname.clone(), self.for_expression(fe)));
            }
            Expression::StructExpression(b, struct_name.clone(), nfields)
        } else {
            panic!("Expected StructExpression, but got {:?}", se)
        }
    }

    fn for_array_expression(&mut self, av: &Expression<A>) -> Expression<B> {
        if let Expression::ArrayExpression(_, elements, len) = av {
            let b = self.transform(av);
            let mut nelements = vec![];
            for e in elements {
                nelements.push(self.for_expression(e));
            }
            Expression::ArrayExpression(b, nelements, *len)
        } else {
            panic!("Expected ArrayValue but got {:?}", av)
        }
    }

    fn for_array_at(&mut self, ar_at: &Expression<A>) -> Expression<B> {
        if let Expression::ArrayAt { array, index, .. } = ar_at {
            let b = self.transform(ar_at);
            let array = self.for_expression(array);
            let index = self.for_expression(index);
            Expression::ArrayAt {
                context: b,
                array: Box::new(array),
                index: Box::new(index),
            }
        } else {
            panic!("Expected ArrayAt, but got {:?}", ar_at)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::compiler::ast::{module::Module, ty::Type};
    use crate::compiler::Span;
    use crate::StringTable;

    impl Context for i32 {
        fn id(&self) -> u32 {
            0
        }

        fn span(&self) -> Span {
            Span::zero()
        }
    }

    impl Context for i64 {
        fn id(&self) -> u32 {
            0
        }

        fn span(&self) -> Span {
            Span::zero()
        }
    }

    impl Context for String {
        fn id(&self) -> u32 {
            0
        }

        fn span(&self) -> Span {
            Span::zero()
        }
    }

    fn convert(n: &dyn Node<i32>) -> i64 {
        let i = n.context();
        2 * (*i as i64)
    }

    #[test]
    fn empty_module() {
        let table = StringTable::new();
        let m = table.insert("m".into());

        let module1 = Module::new(m, 1i32);
        let mut count: i32 = 0;
        let f = |n: &dyn Node<i32>| {
            count += 1;
            convert(n)
        };

        let mut mp = MapPreOrder::new("test", f);
        let module2 = mp.apply(&module1);

        assert_eq!(*module2.context(), 2i64);
        assert_eq!(count, 1);
    }

    #[test]
    fn nested_module() {
        let table = StringTable::new();
        let m = table.insert("m".into());
        let m2 = table.insert("m2".into());

        let mut module1 = Module::new(m, 1i32);
        module1.add_module(Module::new(m2, 2i32));

        let mut count: i32 = 0;
        let f = |n: &dyn Node<i32>| {
            count += 1;
            convert(n)
        };

        let mut mp = MapPreOrder::new("test", f);
        let module2 = mp.apply(&module1);

        assert_eq!(*module2.context(), 2i64);
        assert_eq!(*module2.get_module(m2).unwrap().context(), 4i64);
        assert_eq!(count, 2);
    }

    #[test]
    fn module_with_items() {
        let table = StringTable::new();
        let m = table.insert("m".into());
        let m2 = table.insert("m2".into());
        let p = table.insert("p".into());
        let func = table.insert("func".into());
        let cor = table.insert("cor".into());
        let sd = table.insert("sd".into());

        let mut m = Module::new(m, 1);
        m.add_function(RoutineDef::new_coroutine(
            cor,
            1,
            vec![],
            Type::Unit,
            vec![Statement::Expression(Box::new(Expression::I64(1, 2)))],
        ))
        .unwrap();
        m.add_function(RoutineDef::new_function(
            func,
            1,
            vec![Parameter {
                context: 1,
                name: p,
                ty: Type::Bool,
            }],
            Type::Unit,
            vec![Statement::Expression(Box::new(Expression::I64(1, 2)))],
        ))
        .unwrap();
        m.add_module(Module::new(m2, 1));
        m.add_struct(StructDef::new(sd, 1, vec![])).unwrap();

        // test
        let mut count = 0;
        let f = |n: &dyn Node<i64>| {
            count += 1;
            format!("{}", n.context())
        };
        let mut mapper = MapPreOrder::new("test", f);

        let m_prime = mapper.apply(&m);

        for n in m_prime.iter_preorder() {
            assert_eq!(*n.context(), "1");
        }
        assert_eq!(count, 8);
    }
}
