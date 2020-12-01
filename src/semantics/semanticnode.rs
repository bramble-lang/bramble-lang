use crate::ast;
use crate::ast::*;
use crate::parser::PNode;
use crate::semantics::symbol_table::*;

#[derive(Clone, Debug, PartialEq)]
pub struct SemanticMetadata {
    pub ln: u32,
    pub ty: ast::Type,
    pub sym: SymbolTable,
}

pub type SemanticNode = Ast<SemanticMetadata>;

impl SemanticNode {
    pub fn from_parser_ast(ast: &PNode) -> Result<Box<SemanticNode>, String> {
        use Ast::*;
        match ast {
            Integer(ln, val) => Ok(Box::new(Integer(sm_from(*ln), *val))),
            Boolean(ln, val) => Ok(Box::new(Boolean(sm_from(*ln), *val))),
            CustomType(ln, val) => Ok(Box::new(CustomType(sm_from(*ln), val.clone()))),
            IdentifierDeclare(ln, name, p) => {
                Ok(Box::new(IdentifierDeclare(sm_from(*ln), name.clone(), p.clone())))
            }
            Identifier(l, id) => Ok(Box::new(Identifier(sm_from(*l), id.clone()))),
            BinaryOp(ln, op, ref l, ref r) => Ok(Box::new(BinaryOp(
                sm_from(*ln),
                *op,
                SemanticNode::from_parser_ast(l)?,
                SemanticNode::from_parser_ast(r)?,
            ))),
            UnaryOp(ln, op, ref operand) => Ok(Box::new(UnaryOp(
                sm_from(*ln),
                *op,
                SemanticNode::from_parser_ast(operand)?,
            ))),
            If(ln, cond, true_arm, false_arm) => Ok(Box::new(If(
                sm_from(*ln),
                SemanticNode::from_parser_ast(cond)?,
                SemanticNode::from_parser_ast(true_arm)?,
                SemanticNode::from_parser_ast(false_arm)?,
            ))),
            Mutate(ln, name, ref exp) => Ok(Box::new(Mutate(
                sm_from(*ln),
                name.clone(),
                SemanticNode::from_parser_ast(exp)?,
            ))),
            Bind(ln, name, mutable, p, ref exp) => Ok(Box::new(Bind(
                sm_from(*ln),
                name.clone(),
                *mutable,
                p.clone(),
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
                    p.clone(),
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
            Module{meta:ln, functions, coroutines, structs} => {
                let mut nfuncs = vec![];
                for func in functions.iter() {
                    nfuncs.push(*SemanticNode::from_parser_ast(func)?);
                }
                let mut ncors = vec![];
                for cor in coroutines.iter() {
                    ncors.push(*SemanticNode::from_parser_ast(cor)?);
                }
                let mut nstructs = vec![];
                for st in structs.iter() {
                    nstructs.push(*SemanticNode::from_parser_ast(st)?);
                }
                Ok(Box::new(Module{meta: sm_from(*ln), functions: nfuncs, coroutines: ncors, structs: nstructs}))
            }
            StructDef(l, name, fields) => Ok(Box::new(StructDef(sm_from(*l), name.clone(), fields.clone()))),
            StructInit(l, name, fields) => {
                let mut nfields = vec![];
                for (fname, fvalue) in fields.iter() {
                    let fvalue2 = SemanticNode::from_parser_ast(fvalue)?;
                    nfields.push((fname.clone(), fvalue2));
                }
                Ok(Box::new(StructInit(sm_from(*l), name.clone(), nfields)))
            }
        }
    }
}

fn sm(ln: u32, ty: ast::Type) -> SemanticMetadata {
    SemanticMetadata {
        ln,
        ty,
        sym: SymbolTable::new(),
    }
}

fn sm_from(l: u32) -> SemanticMetadata {
    sm(l, ast::Type::Unknown)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_node() {
        for (node, expected) in [
            (
                Ast::Integer(1, 3),
                Ast::Integer(sm(1, ast::Type::Unknown), 3),
            ),
            (
                Ast::Boolean(1, true),
                Ast::Boolean(sm(1, ast::Type::Unknown), true),
            ),
            (
                Ast::Identifier(1, "x".into()),
                Ast::Identifier(sm(1, ast::Type::Unknown), "x".into()),
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
                    Ast::Integer(sm(1, ast::Type::Unknown), 3),
                    Ast::Integer(sm(1, ast::Type::Unknown), 3),
                ),
            ),
            (
                (
                    Ast::Identifier(1, "x".into()),
                    Ast::Identifier(1, "y".into()),
                ),
                (
                    Ast::Identifier(sm(1, ast::Type::Unknown), "x".into()),
                    Ast::Identifier(sm(1, ast::Type::Unknown), "y".into()),
                ),
            ),
            (
                (Ast::Boolean(1, true), Ast::Boolean(1, false)),
                (
                    Ast::Boolean(sm(1, ast::Type::Unknown), true),
                    Ast::Boolean(sm(1, ast::Type::Unknown), false),
                ),
            ),
        ]
        .iter()
        {
            for (tree, expected) in [
                (
                    Ast::BinaryOp(1, BinaryOperator::Mul, Box::new(l.clone()), Box::new(r.clone())),
                    Ast::BinaryOp(
                        sm(1, ast::Type::Unknown),
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
