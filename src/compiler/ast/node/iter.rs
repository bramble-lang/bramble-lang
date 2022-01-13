use crate::compiler::ast::node::Node;

use super::Context;

pub struct PostOrderIter<'a, A>
where
    A: Context,
{
    out: Vec<&'a dyn Node<A>>,
}

/**
Performs a Post Order traversal of an AST and applies the given function,
`f`, on each node.  This construct exists to allow a user to easily traverse
through an AST using `for` loops and print out diagnostic or debugging
information. Using this iterator will transform the AST into an ordered
list of nodes and does not preserve the AST topology

For Compiler transformations, use the `MapPostOrder` construct.
*/
impl<'a, A> PostOrderIter<'a, A>
where
    A: Context,
{
    /**
    Create a new Iterator which will perform a PostOrder DFS traversal of an AST
    starting at the given node as its root.
    */
    pub fn new(node: &'a dyn Node<A>) -> PostOrderIter<'a, A> {
        let mut stack = vec![node];
        let mut out = vec![];

        // Build a Post Ordered vector of references to Nodes
        while let Some(n) = stack.pop() {
            out.push(n);
            let children = n.children().into_iter();
            for c in children {
                stack.push(c);
            }
        }

        PostOrderIter { out }
    }
}

impl<'a, A> Iterator for PostOrderIter<'a, A>
where
    A: std::fmt::Debug + Context,
{
    type Item = &'a dyn Node<A>;

    fn next(&mut self) -> Option<Self::Item> {
        self.out.pop()
    }
}

/**
Performs a Pre Order traversal of an AST and applies the given function,
`f`, on each node.  This construct exists to allow a user to easily traverse
through an AST using `for` loops and print out diagnostic or debugging
information. Using this iterator will transform the AST into an ordered
list of nodes and does not preserve the AST topology.

For Compiler transformations, use the `MapPostOrder` construct.
*/
pub struct PreOrderIter<'a, A>
where
    A: Context,
{
    out: Vec<&'a dyn Node<A>>,
}

impl<'a, A> PreOrderIter<'a, A>
where
    A: Context,
{
    /**
    Create a new Iterator which will perform a PreOrder DFS traversal of an AST
    starting at the given node as its root.
    */
    pub fn new(node: &'a dyn Node<A>) -> PreOrderIter<'a, A> {
        let mut stack = vec![node];
        let mut out = vec![];

        // Build a Post Ordered vector of references to Nodes
        while let Some(n) = stack.pop() {
            out.push(n);
            let children = n.children().into_iter().rev();
            for c in children {
                stack.push(c);
            }
        }

        out.reverse();

        PreOrderIter { out }
    }
}

impl<'a, A> Iterator for PreOrderIter<'a, A>
where
    A: std::fmt::Debug + Context,
{
    type Item = &'a dyn Node<A>;

    fn next(&mut self) -> Option<Self::Item> {
        self.out.pop()
    }
}

#[cfg(test)]
mod test_preorder {
    use super::*;
    use crate::{
        compiler::ast::{
            expression::{self, Expression, UnaryOperator},
            module::Module,
            parameter::Parameter,
            routinedef::RoutineDef,
            statement::{Bind, Statement, YieldReturn},
            structdef::StructDef,
            ty::Type,
            Element,
        },
        StringId, StringTable,
    };

    fn convert(n: &dyn Node<i32>) -> i64 {
        let i = n.context();
        2 * (*i as i64)
    }

    #[test]
    fn empty_module() {
        let table = StringTable::new();
        let m = table.insert("m".into());

        let module1 = Module::new(m, 1i32);

        let expected = vec![1];
        let mut iter = module1.iter_preorder();
        for e in expected {
            let t = iter.next().unwrap();
            assert_eq!(*t.context(), e);
        }
    }

    #[test]
    fn nested_module() {
        let table = StringTable::new();
        let m = table.insert("m".into());
        let m2 = table.insert("m2".into());

        let mut module1 = Module::new(m, 1i32);
        module1.add_module(Module::new(m2, 2i32));

        let expected = vec![1, 2];
        let mut iter = module1.iter_preorder();
        for e in expected {
            let t = iter.next().unwrap();
            assert_eq!(*t.context(), e);
        }
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
        m.add_coroutine(RoutineDef::new_coroutine(
            cor,
            2,
            vec![],
            Type::Unit,
            vec![Statement::Expression(Box::new(Expression::I64(3, 2)))],
        ))
        .unwrap();
        m.add_function(RoutineDef::new_function(
            func,
            4,
            vec![Parameter {
                context: 5,
                name: p,
                ty: Type::Bool,
            }],
            Type::Unit,
            vec![Statement::Expression(Box::new(Expression::I64(6, 2)))],
        ))
        .unwrap();
        m.add_module(Module::new(m2, 7));
        m.add_struct(StructDef::new(sd, 8, vec![])).unwrap();

        let expected = vec![1, 7, 4, 5, 6, 2, 3, 8];
        let test: Vec<i64> = m.iter_preorder().map(|n| *n.context()).collect();
        assert_eq!(test, expected);
    }

    #[test]
    fn function() {
        let table = StringTable::new();
        let p = table.insert("p".into());
        let x = table.insert("x".into());
        let y = table.insert("y".into());
        let c = table.insert("c".into());
        let func = table.insert("func".into());
        let test = table.insert("test".into());

        let mut f = RoutineDef::new_function(func, 1, vec![], Type::Unit, vec![]);
        f.body.push(Statement::Bind(Box::new(Bind::new(
            2,
            x,
            Type::I32,
            false,
            Expression::If {
                context: 3,
                cond: Box::new(Expression::BinaryOp(
                    4,
                    crate::compiler::ast::expression::BinaryOperator::Eq,
                    Box::new(Expression::I64(5, 1)),
                    Box::new(Expression::I64(6, 1)),
                )),
                if_arm: Box::new(Expression::Identifier(7, y)),
                else_arm: Some(Box::new(Expression::StringLiteral(8, StringId::new()))),
            },
        ))));
        f.body
            .push(Statement::YieldReturn(Box::new(YieldReturn::new(
                9,
                Some(Expression::UnaryOp(
                    10,
                    UnaryOperator::Negate,
                    Box::new(Expression::ExpressionBlock(
                        11,
                        vec![Statement::Expression(Box::new(Expression::Yield(
                            12,
                            Box::new(Expression::Identifier(13, c)),
                        )))],
                        Some(Box::new(Expression::RoutineCall(
                            14,
                            expression::RoutineCall::Function,
                            vec![Element::Id(test)].into(),
                            vec![Expression::Identifier(15, p)],
                        ))),
                    )),
                )),
            ))));

        let expected = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
        let test: Vec<i64> = f.iter_preorder().map(|n| *n.context()).collect();
        assert_eq!(test, expected);
    }
}

#[cfg(test)]
mod test_postorder {
    use super::*;
    use crate::{
        compiler::ast::{
            expression::Expression,
            module::Module,
            parameter::Parameter,
            routinedef::RoutineDef,
            statement::{Bind, Statement},
            structdef::StructDef,
            ty::Type,
            BinaryOperator, Element, RoutineCall, UnaryOperator, YieldReturn,
        },
        StringId, StringTable,
    };

    fn convert(n: &dyn Node<i32>) -> i64 {
        let i = n.context();
        2 * (*i as i64)
    }

    #[test]
    fn empty_module() {
        let table = StringTable::new();
        let m = table.insert("m".into());

        let module1 = Module::new(m, 1i32);

        let expected = vec![1];
        let mut iter = module1.iter_postorder();
        for e in expected {
            let t = iter.next().unwrap();
            assert_eq!(*t.context(), e);
        }
    }

    #[test]
    fn nested_module() {
        let table = StringTable::new();
        let m = table.insert("m".into());
        let m2 = table.insert("m2".into());

        let mut module1 = Module::new(m, 1i32);
        module1.add_module(Module::new(m2, 2i32));

        let expected = vec![2, 1];
        let mut iter = module1.iter_postorder();
        for e in expected {
            let t = iter.next().unwrap();
            assert_eq!(*t.context(), e);
        }
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
        m.add_coroutine(RoutineDef::new_coroutine(
            cor,
            2,
            vec![],
            Type::Unit,
            vec![Statement::Expression(Box::new(Expression::I64(3, 2)))],
        ))
        .unwrap();
        m.add_function(RoutineDef::new_function(
            func,
            4,
            vec![Parameter {
                context: 5,
                name: p,
                ty: Type::Bool,
            }],
            Type::Unit,
            vec![Statement::Expression(Box::new(Expression::I64(6, 2)))],
        ))
        .unwrap();
        m.add_module(Module::new(m2, 7));
        m.add_struct(StructDef::new(sd, 8, vec![])).unwrap();

        let expected = vec![7, 5, 6, 4, 3, 2, 8, 1];
        let test: Vec<i64> = m.iter_postorder().map(|n| *n.context()).collect();
        assert_eq!(test, expected);
    }

    #[test]
    fn function() {
        let table = StringTable::new();
        let x = table.insert("x".into());
        let y = table.insert("y".into());
        let func = table.insert("func".into());

        let mut f = RoutineDef::new_function(func, 1, vec![], Type::Unit, vec![]);
        f.body.push(Statement::Bind(Box::new(Bind::new(
            2,
            x,
            Type::I32,
            false,
            Expression::If {
                context: 3,
                cond: Box::new(Expression::BinaryOp(
                    4,
                    BinaryOperator::Eq,
                    Box::new(Expression::I64(5, 1)),
                    Box::new(Expression::I64(6, 1)),
                )),
                if_arm: Box::new(Expression::Identifier(7, y)),
                else_arm: Some(Box::new(Expression::StringLiteral(8, StringId::new()))),
            },
        ))));

        f.body
            .push(Statement::YieldReturn(Box::new(YieldReturn::new(
                9,
                Some(Expression::UnaryOp(
                    10,
                    UnaryOperator::Negate,
                    Box::new(Expression::ExpressionBlock(
                        11,
                        vec![Statement::Expression(Box::new(Expression::Yield(
                            12,
                            Box::new(Expression::Identifier(13, x)),
                        )))],
                        Some(Box::new(Expression::RoutineCall(
                            14,
                            RoutineCall::Function,
                            vec![Element::Id(func)].into(),
                            vec![Expression::Identifier(15, y)],
                        ))),
                    )),
                )),
            ))));

        let expected = vec![5, 6, 4, 7, 8, 3, 2, 13, 12, 15, 14, 11, 10, 9, 1];
        let test: Vec<i64> = f.iter_postorder().map(|n| *n.context()).collect();
        assert_eq!(test, expected);
    }
}
