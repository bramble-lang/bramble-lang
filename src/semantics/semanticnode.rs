use crate::parser::{Ast, PNode, Primitive};
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
            Mul(ln, ref l, ref r) => Ok(Box::new(Mul(
                sm_from(*ln),
                SemanticNode::from_parser_ast(l)?,
                SemanticNode::from_parser_ast(r)?,
            ))),
            Add(ln, ref l, ref r) => Ok(Box::new(Add(
                sm_from(*ln),
                SemanticNode::from_parser_ast(l)?,
                SemanticNode::from_parser_ast(r)?,
            ))),
            BAnd(ln, ref l, ref r) => Ok(Box::new(BAnd(
                sm_from(*ln),
                SemanticNode::from_parser_ast(l)?,
                SemanticNode::from_parser_ast(r)?,
            ))),
            BOr(ln, ref l, ref r) => Ok(Box::new(BOr(
                sm_from(*ln),
                SemanticNode::from_parser_ast(l)?,
                SemanticNode::from_parser_ast(r)?,
            ))),
            Eq(ln, ref l, ref r) => Ok(Box::new(Eq(
                sm_from(*ln),
                SemanticNode::from_parser_ast(l)?,
                SemanticNode::from_parser_ast(r)?,
            ))),
            NEq(ln, ref l, ref r) => Ok(Box::new(NEq(
                sm_from(*ln),
                SemanticNode::from_parser_ast(l)?,
                SemanticNode::from_parser_ast(r)?,
            ))),
            Gr(ln, ref l, ref r) => Ok(Box::new(Gr(
                sm_from(*ln),
                SemanticNode::from_parser_ast(l)?,
                SemanticNode::from_parser_ast(r)?,
            ))),
            GrEq(ln, ref l, ref r) => Ok(Box::new(GrEq(
                sm_from(*ln),
                SemanticNode::from_parser_ast(l)?,
                SemanticNode::from_parser_ast(r)?,
            ))),
            Ls(ln, ref l, ref r) => Ok(Box::new(Ls(
                sm_from(*ln),
                SemanticNode::from_parser_ast(l)?,
                SemanticNode::from_parser_ast(r)?,
            ))),
            LsEq(ln, ref l, ref r) => Ok(Box::new(LsEq(
                sm_from(*ln),
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
            FunctionDef(ln, fname, params, p, body) => {
                let mut nbody = vec![];
                for stmt in body.iter() {
                    let r = SemanticNode::from_parser_ast(stmt)?;
                    nbody.push(*r);
                }
                Ok(Box::new(FunctionDef(
                    sm_from(*ln),
                    fname.clone(),
                    params.clone(),
                    *p,
                    nbody,
                )))
            }
            CoroutineDef(ln, coname, params, p, body) => {
                let mut nbody = vec![];
                for stmt in body.iter() {
                    let r = SemanticNode::from_parser_ast(stmt)?;
                    nbody.push(*r);
                }
                Ok(Box::new(CoroutineDef(
                    sm_from(*ln),
                    coname.clone(),
                    params.clone(),
                    *p,
                    nbody,
                )))
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
