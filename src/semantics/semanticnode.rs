use crate::ast;
use crate::ast::*;
use crate::semantics::symbol_table::*;
use crate::syntax::pnode::PNode;

#[derive(Clone, Debug, PartialEq)]
pub struct SemanticMetadata {
    pub id: u32,
    pub ln: u32,
    pub ty: ast::Type,
    pub sym: SymbolTable,
}

pub type SemanticNode = Ast<SemanticMetadata>;

pub struct SemanticAst {
    next_id: u32,
}
impl SemanticAst {
    pub fn new() -> SemanticAst {
        SemanticAst { next_id: 0 }
    }

    pub fn from_parser_ast(&mut self, ast: &PNode) -> Result<Box<SemanticNode>, String> {
        use Ast::*;
        match ast {
            Integer(ln, val) => Ok(Box::new(Integer(self.sm_from(*ln), *val))),
            Boolean(ln, val) => Ok(Box::new(Boolean(self.sm_from(*ln), *val))),
            StringLiteral(ln, val) => Ok(Box::new(StringLiteral(self.sm_from(*ln), val.clone()))),
            CustomType(ln, val) => Ok(Box::new(CustomType(self.sm_from(*ln), val.clone()))),
            IdentifierDeclare(ln, name, p) => Ok(Box::new(IdentifierDeclare(
                self.sm_from(*ln),
                name.clone(),
                p.clone(),
            ))),
            Identifier(ln, id) => Ok(Box::new(Identifier(self.sm_from(*ln), id.clone()))),
            MemberAccess(ln, src, member) => Ok(Box::new(MemberAccess(
                self.sm_from(*ln),
                self.from_parser_ast(src)?,
                member.clone(),
            ))),
            BinaryOp(ln, op, ref l, ref r) => Ok(Box::new(BinaryOp(
                self.sm_from(*ln),
                *op,
                self.from_parser_ast(l)?,
                self.from_parser_ast(r)?,
            ))),
            UnaryOp(ln, op, ref operand) => Ok(Box::new(UnaryOp(
                self.sm_from(*ln),
                *op,
                self.from_parser_ast(operand)?,
            ))),
            If(ln, cond, true_arm, false_arm) => Ok(Box::new(If(
                self.sm_from(*ln),
                self.from_parser_ast(cond)?,
                self.from_parser_ast(true_arm)?,
                self.from_parser_ast(false_arm)?,
            ))),
            Mutate(ln, name, ref exp) => Ok(Box::new(Mutate(
                self.sm_from(*ln),
                name.clone(),
                self.from_parser_ast(exp)?,
            ))),
            Bind(ln, name, mutable, p, ref exp) => Ok(Box::new(Bind(
                self.sm_from(*ln),
                name.clone(),
                *mutable,
                p.clone(),
                self.from_parser_ast(exp)?,
            ))),
            Return(l, None) => Ok(Box::new(Return(self.sm_from(*l), None))),
            Return(l, Some(exp)) => Ok(Box::new(Return(
                self.sm_from(*l),
                Some(self.from_parser_ast(exp)?),
            ))),
            Yield(l, box exp) => Ok(Box::new(Yield(
                self.sm_from(*l),
                self.from_parser_ast(exp)?,
            ))),
            YieldReturn(l, None) => Ok(Box::new(YieldReturn(self.sm_from(*l), None))),
            YieldReturn(l, Some(exp)) => Ok(Box::new(YieldReturn(
                self.sm_from(*l),
                Some(self.from_parser_ast(exp)?),
            ))),
            ExpressionBlock(ln, body) => {
                let mut nbody = vec![];
                for stmt in body.iter() {
                    let r = self.from_parser_ast(stmt)?;
                    nbody.push(*r);
                }
                Ok(Box::new(ExpressionBlock(self.sm_from(*ln), nbody)))
            }
            Statement(_, stmt) => Ok(self.from_parser_ast(stmt)?),
            RoutineDef(ln, def, fname, params, p, body) => {
                let mut nbody = vec![];
                for stmt in body.iter() {
                    let r = self.from_parser_ast(stmt)?;
                    nbody.push(*r);
                }
                Ok(Box::new(RoutineDef(
                    self.sm_from(*ln),
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
                    let np = self.from_parser_ast(param)?;
                    nparams.push(*np);
                }
                Ok(Box::new(RoutineCall(
                    self.sm_from(*l),
                    *call,
                    name.clone(),
                    nparams,
                )))
            }
            Printi(l, exp) => Ok(Box::new(Printi(
                self.sm_from(*l),
                self.from_parser_ast(exp)?,
            ))),
            Printiln(l, exp) => Ok(Box::new(Printiln(
                self.sm_from(*l),
                self.from_parser_ast(exp)?,
            ))),
            Prints(l, exp) => Ok(Box::new(Prints(
                self.sm_from(*l),
                self.from_parser_ast(exp)?,
            ))),
            Printbln(l, exp) => Ok(Box::new(Printbln(
                self.sm_from(*l),
                self.from_parser_ast(exp)?,
            ))),
            Module {
                meta: ln,
                functions,
                coroutines,
                structs,
            } => {
                let mut nfuncs = vec![];
                for func in functions.iter() {
                    nfuncs.push(*self.from_parser_ast(func)?);
                }
                let mut ncors = vec![];
                for cor in coroutines.iter() {
                    ncors.push(*self.from_parser_ast(cor)?);
                }
                let mut nstructs = vec![];
                for st in structs.iter() {
                    nstructs.push(*self.from_parser_ast(st)?);
                }
                Ok(Box::new(Module {
                    meta: self.sm_from(*ln),
                    functions: nfuncs,
                    coroutines: ncors,
                    structs: nstructs,
                }))
            }
            StructDef(l, name, fields) => Ok(Box::new(StructDef(
                self.sm_from(*l),
                name.clone(),
                fields.clone(),
            ))),
            StructExpression(l, name, fields) => {
                let mut nfields = vec![];
                for (fname, fvalue) in fields.iter() {
                    let fvalue2 = self.from_parser_ast(fvalue)?;
                    nfields.push((fname.clone(), *fvalue2));
                }
                Ok(Box::new(StructExpression(
                    self.sm_from(*l),
                    name.clone(),
                    nfields,
                )))
            }
        }
    }

    fn sm_from(&mut self, l: u32) -> SemanticMetadata {
        let sm_data = sm(self.next_id, l, ast::Type::Unknown);
        self.next_id += 1;
        sm_data
    }
}

fn sm(id: u32, ln: u32, ty: ast::Type) -> SemanticMetadata {
    SemanticMetadata {
        id,
        ln,
        ty,
        sym: SymbolTable::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_node() {
        for (node, expected) in [
            (
                Ast::Integer(1, 3),
                Ast::Integer(sm(0, 1, ast::Type::Unknown), 3),
            ),
            (
                Ast::Boolean(1, true),
                Ast::Boolean(sm(0, 1, ast::Type::Unknown), true),
            ),
            (
                Ast::Identifier(1, "x".into()),
                Ast::Identifier(sm(0, 1, ast::Type::Unknown), "x".into()),
            ),
        ]
        .iter()
        {
            let mut sa = SemanticAst { next_id: 0 };
            let snode = sa.from_parser_ast(&node).unwrap();
            assert_eq!(*snode, *expected);
        }
    }

    #[test]
    fn test_multi_nodes() {
        for ((l, r), (el, er)) in [
            (
                (Ast::Integer(1, 3), Ast::Integer(1, 3)),
                (
                    Ast::Integer(sm(1, 1, ast::Type::Unknown), 3),
                    Ast::Integer(sm(2, 1, ast::Type::Unknown), 3),
                ),
            ),
            (
                (
                    Ast::Identifier(1, "x".into()),
                    Ast::Identifier(1, "y".into()),
                ),
                (
                    Ast::Identifier(sm(1, 1, ast::Type::Unknown), "x".into()),
                    Ast::Identifier(sm(2, 1, ast::Type::Unknown), "y".into()),
                ),
            ),
            (
                (Ast::Boolean(1, true), Ast::Boolean(1, false)),
                (
                    Ast::Boolean(sm(1, 1, ast::Type::Unknown), true),
                    Ast::Boolean(sm(2, 1, ast::Type::Unknown), false),
                ),
            ),
        ]
        .iter()
        {
            for (tree, expected) in [(
                Ast::BinaryOp(
                    1,
                    BinaryOperator::Mul,
                    Box::new(l.clone()),
                    Box::new(r.clone()),
                ),
                Ast::BinaryOp(
                    sm(0, 1, ast::Type::Unknown),
                    BinaryOperator::Mul,
                    Box::new(el.clone()),
                    Box::new(er.clone()),
                ),
            )]
            .iter()
            {
                let mut sa = SemanticAst { next_id: 0 };
                let snode = sa.from_parser_ast(tree).unwrap();
                assert_eq!(*snode, *expected);
            }
        }
    }
}
