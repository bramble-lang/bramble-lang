use crate::ast::*;
use crate::parser::PNode;
use crate::semantics::symbol_table::*;

#[derive(Clone, Debug, PartialEq)]
pub struct SemanticMetadata {
    pub ln: u32,
    pub ty: Primitive,
    pub sym: SymbolTable,
}

pub type SemanticNode = Ast<SemanticMetadata>;

impl SemanticNode {
    pub fn from_parser_ast(ast: &PNode) -> Result<Box<SemanticNode>, String> {
        use Ast::*;
        match ast {
            Integer(ln, val) => Ok(Box::new(Integer(sm_from(*ln), *val))),
            Boolean(ln, val) => Ok(Box::new(Boolean(sm_from(*ln), *val))),
            IdentifierDeclare(ln, name, p) => {
                Ok(Box::new(IdentifierDeclare(sm_from(*ln), name.clone(), *p)))
            }
            Identifier(l, id) => Ok(Box::new(Identifier(sm_from(*l), id.clone()))),
            BinaryOp(ln, op, ref l, ref r) => Ok(Box::new(BinaryOp(
                sm_from(*ln),
                *op,
                SemanticNode::from_parser_ast(l)?,
                SemanticNode::from_parser_ast(r)?,
            ))),
            If(ln, cond, true_arm, false_arm) => Ok(Box::new(If(
                sm_from(*ln),
                SemanticNode::from_parser_ast(cond)?,
                SemanticNode::from_parser_ast(true_arm)?,
                SemanticNode::from_parser_ast(false_arm)?,
            ))),
            Bind(ln, name, p, ref exp) => Ok(Box::new(Bind(
                sm_from(*ln),
                name.clone(),
                *p,
                SemanticNode::from_parser_ast(exp)?,
            ))),
            Return(l, None) => Ok(Box::new(Return(sm_from(*l), None))),
            Return(l, Some(exp)) => Ok(Box::new(Return(
                sm_from(*l),
                Some(SemanticNode::from_parser_ast(exp)?),
            ))),
            Yield(l, box exp) => Ok(Box::new(Yield(
                sm_from(*l),
                SemanticNode::from_parser_ast(exp)?,
            ))),
            YieldReturn(l, None) => Ok(Box::new(YieldReturn(sm_from(*l), None))),
            YieldReturn(l, Some(exp)) => Ok(Box::new(YieldReturn(
                sm_from(*l),
                Some(SemanticNode::from_parser_ast(exp)?),
            ))),
            ExpressionBlock(ln, body) => {
                let mut nbody = vec![];
                for stmt in body.iter() {
                    let r = SemanticNode::from_parser_ast(stmt)?;
                    nbody.push(*r);
                }
                Ok(Box::new(ExpressionBlock(sm_from(*ln), nbody)))
            }
            Statement(_, stmt) => Ok(SemanticNode::from_parser_ast(stmt)?),
            RoutineDef(ln, def, fname, params, p, body) => {
                let mut nbody = vec![];
                for stmt in body.iter() {
                    let r = SemanticNode::from_parser_ast(stmt)?;
                    nbody.push(*r);
                }
                Ok(Box::new(RoutineDef(
                    sm_from(*ln),
                    *def,
                    fname.clone(),
                    params.clone(),
                    *p,
                    nbody,
                )))
            }
            RoutineCall(l, call, name, params) => {
                // test that the expressions passed to the function match the functions
                // parameter types
                let mut nparams = vec![];
                for param in params.iter() {
                    let np = SemanticNode::from_parser_ast(param)?;
                    nparams.push(*np);
                }
                Ok(Box::new(RoutineCall(sm_from(*l), *call, name.clone(), nparams)))
            }
            FunctionCall(l, fname, params) => {
                // test that the expressions passed to the function match the functions
                // parameter types
                let mut nparams = vec![];
                for param in params.iter() {
                    let np = SemanticNode::from_parser_ast(param)?;
                    nparams.push(*np);
                }
                Ok(Box::new(FunctionCall(sm_from(*l), fname.clone(), nparams)))
            }
            CoroutineInit(l, coname, params) => {
                // test that the expressions passed to the function match the functions
                // parameter types
                let mut nparams = vec![];
                for param in params.iter() {
                    let np = SemanticNode::from_parser_ast(param)?;
                    nparams.push(*np);
                }
                Ok(Box::new(CoroutineInit(
                    sm_from(*l),
                    coname.clone(),
                    nparams,
                )))
            }
            Printi(l, exp) => Ok(Box::new(Printi(
                sm_from(*l),
                SemanticNode::from_parser_ast(exp)?,
            ))),
            Printiln(l, exp) => Ok(Box::new(Printiln(
                sm_from(*l),
                SemanticNode::from_parser_ast(exp)?,
            ))),
            Printbln(l, exp) => Ok(Box::new(Printbln(
                sm_from(*l),
                SemanticNode::from_parser_ast(exp)?,
            ))),
            Module(ln, funcs, cors) => {
                let mut nfuncs = vec![];
                for func in funcs.iter() {
                    nfuncs.push(*SemanticNode::from_parser_ast(func)?);
                }
                let mut ncors = vec![];
                for cor in cors.iter() {
                    ncors.push(*SemanticNode::from_parser_ast(cor)?);
                }
                Ok(Box::new(Module(sm_from(*ln), nfuncs, ncors)))
            }
        }
    }

    /// Analyze the AST and add functions to symbol tables
    pub fn extract_routines(&mut self) {}
}

fn sm(ln: u32, ty: Primitive) -> SemanticMetadata {
    SemanticMetadata {
        ln,
        ty,
        sym: SymbolTable::new(),
    }
}

fn sm_from(l: u32) -> SemanticMetadata {
    sm(l, Primitive::Unknown)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_node() {
        for (node, expected) in [
            (
                Ast::Integer(1, 3),
                Ast::Integer(sm(1, Primitive::Unknown), 3),
            ),
            (
                Ast::Boolean(1, true),
                Ast::Boolean(sm(1, Primitive::Unknown), true),
            ),
            (
                Ast::Identifier(1, "x".into()),
                Ast::Identifier(sm(1, Primitive::Unknown), "x".into()),
            ),
        ]
        .iter()
        {
            let snode = SemanticNode::from_parser_ast(&node).unwrap();
            assert_eq!(*snode, *expected);
        }
    }

    #[test]
    fn test_multi_nodes() {
        for ((l, r), (el, er)) in [
            (
                (Ast::Integer(1, 3), Ast::Integer(1, 3)),
                (
                    Ast::Integer(sm(1, Primitive::Unknown), 3),
                    Ast::Integer(sm(1, Primitive::Unknown), 3),
                ),
            ),
            (
                (
                    Ast::Identifier(1, "x".into()),
                    Ast::Identifier(1, "y".into()),
                ),
                (
                    Ast::Identifier(sm(1, Primitive::Unknown), "x".into()),
                    Ast::Identifier(sm(1, Primitive::Unknown), "y".into()),
                ),
            ),
            (
                (Ast::Boolean(1, true), Ast::Boolean(1, false)),
                (
                    Ast::Boolean(sm(1, Primitive::Unknown), true),
                    Ast::Boolean(sm(1, Primitive::Unknown), false),
                ),
            ),
        ]
        .iter()
        {
            for (tree, expected) in [
                (
                    Ast::BinaryOp(1, BinaryOperator::Mul, Box::new(l.clone()), Box::new(r.clone())),
                    Ast::BinaryOp(
                        sm(1, Primitive::Unknown),
                        BinaryOperator::Mul,
                        Box::new(el.clone()),
                        Box::new(er.clone()),
                    ),
                ),
            ]
            .iter()
            {
                let snode = SemanticNode::from_parser_ast(tree).unwrap();
                assert_eq!(*snode, *expected);
            }
        }
    }
}
