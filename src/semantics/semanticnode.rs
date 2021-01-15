use crate::{ast::*, syntax::path::Path, syntax::{module, ty::Type}};
use crate::semantics::symbol_table::*;
use crate::syntax::pnode::PNode;
use crate::diagnostics::config::TracingConfig;

#[derive(Clone, Debug, PartialEq)]
pub struct SemanticMetadata {
    pub id: u32,
    pub ln: u32,
    pub ty: Type,
    pub sym: SymbolTable,
    pub canonical_path: Path,
}

pub type SemanticNode = Ast<SemanticMetadata>;

impl SemanticNode {
    pub fn get_type(&self) -> &Type {
        let meta = self.get_metadata();
        &meta.ty
    }
}

impl SemanticMetadata {
    pub fn new(id: u32, ln: u32, ty: Type) -> SemanticMetadata {
        SemanticMetadata {
            id,
            ln,
            ty,
            sym: SymbolTable::new(),
            canonical_path: Path::new(),
        }
    }

    pub fn get_canonical_path(&self) -> &Path {
        &self.canonical_path
    }

    pub fn set_canonical_path(&mut self, path: Path) {
        self.canonical_path = path;
    }
}

impl std::fmt::Display for SemanticNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.root_str())
    }
}

pub struct SemanticAst {
    next_id: u32,
    tracing: TracingConfig,
}

impl SemanticAst {
    pub fn new() -> SemanticAst {
        SemanticAst {
            next_id: 0,
            tracing: TracingConfig::Off,
        }
    }

    pub fn from_parser_ast(&mut self, ast: &PNode) -> Result<Box<SemanticNode>, String> {
        use Ast::*;
        let node = match ast {
            Integer(ln, val) => Ok(Box::new(Integer(self.semantic_metadata_from(*ln), *val))),
            Boolean(ln, val) => Ok(Box::new(Boolean(self.semantic_metadata_from(*ln), *val))),
            StringLiteral(ln, val) => Ok(Box::new(StringLiteral(
                self.semantic_metadata_from(*ln),
                val.clone(),
            ))),
            CustomType(ln, val) => Ok(Box::new(CustomType(
                self.semantic_metadata_from(*ln),
                val.clone(),
            ))),
            IdentifierDeclare(ln, name, p) => Ok(Box::new(IdentifierDeclare(
                self.semantic_metadata_from(*ln),
                name.clone(),
                p.clone(),
            ))),
            Identifier(ln, id) => Ok(Box::new(Identifier(
                self.semantic_metadata_from(*ln),
                id.clone(),
            ))),
            Path(ln, path) => Ok(Box::new(Path(
                self.semantic_metadata_from(*ln),
                path.clone(),
            ))),
            MemberAccess(ln, src, member) => Ok(Box::new(MemberAccess(
                self.semantic_metadata_from(*ln),
                self.from_parser_ast(src)?,
                member.clone(),
            ))),
            BinaryOp(ln, op, ref l, ref r) => Ok(Box::new(BinaryOp(
                self.semantic_metadata_from(*ln),
                *op,
                self.from_parser_ast(l)?,
                self.from_parser_ast(r)?,
            ))),
            UnaryOp(ln, op, ref operand) => Ok(Box::new(UnaryOp(
                self.semantic_metadata_from(*ln),
                *op,
                self.from_parser_ast(operand)?,
            ))),
            If(ln, cond, true_arm, false_arm) => Ok(Box::new(If(
                self.semantic_metadata_from(*ln),
                self.from_parser_ast(cond)?,
                self.from_parser_ast(true_arm)?,
                self.from_parser_ast(false_arm)?,
            ))),
            Mutate(ln, name, ref exp) => Ok(Box::new(Mutate(
                self.semantic_metadata_from(*ln),
                name.clone(),
                self.from_parser_ast(exp)?,
            ))),
            Bind(ln, name, mutable, p, ref exp) => Ok(Box::new(Bind(
                self.semantic_metadata_from(*ln),
                name.clone(),
                *mutable,
                p.clone(),
                self.from_parser_ast(exp)?,
            ))),
            Return(l, None) => Ok(Box::new(Return(self.semantic_metadata_from(*l), None))),
            Return(l, Some(exp)) => Ok(Box::new(Return(
                self.semantic_metadata_from(*l),
                Some(self.from_parser_ast(exp)?),
            ))),
            Yield(l, box exp) => Ok(Box::new(Yield(
                self.semantic_metadata_from(*l),
                self.from_parser_ast(exp)?,
            ))),
            YieldReturn(l, None) => {
                Ok(Box::new(YieldReturn(self.semantic_metadata_from(*l), None)))
            }
            YieldReturn(l, Some(exp)) => Ok(Box::new(YieldReturn(
                self.semantic_metadata_from(*l),
                Some(self.from_parser_ast(exp)?),
            ))),
            ExpressionBlock(ln, body) => {
                let mut nbody = vec![];
                for stmt in body.iter() {
                    let r = self.from_parser_ast(stmt)?;
                    nbody.push(*r);
                }
                Ok(Box::new(ExpressionBlock(
                    self.semantic_metadata_from(*ln),
                    nbody,
                )))
            }
            Statement(_, stmt) => Ok(self.from_parser_ast(stmt)?),
            RoutineDef {
                meta: ln,
                def,
                name: fname,
                params,
                ty,
                body,
            } => {
                let mut nbody = vec![];
                for stmt in body.iter() {
                    let r = self.from_parser_ast(stmt)?;
                    nbody.push(*r);
                }
                Ok(Box::new(RoutineDef {
                    meta: self.semantic_metadata_from(*ln),
                    def: *def,
                    name: fname.clone(),
                    params: params.clone(),
                    ty: ty.clone(),
                    body: nbody,
                }))
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
                    self.semantic_metadata_from(*l),
                    *call,
                    name.clone(),
                    nparams,
                )))
            }
            Printi(l, exp) => Ok(Box::new(Printi(
                self.semantic_metadata_from(*l),
                self.from_parser_ast(exp)?,
            ))),
            Printiln(l, exp) => Ok(Box::new(Printiln(
                self.semantic_metadata_from(*l),
                self.from_parser_ast(exp)?,
            ))),
            Prints(l, exp) => Ok(Box::new(Prints(
                self.semantic_metadata_from(*l),
                self.from_parser_ast(exp)?,
            ))),
            Printbln(l, exp) => Ok(Box::new(Printbln(
                self.semantic_metadata_from(*l),
                self.from_parser_ast(exp)?,
            ))),
            Module(m) => {
                let nmodule = self.from_module(m)?;
                Ok(Box::new(Module(nmodule)))
            }
            StructDef(l, name, fields) => Ok(Box::new(StructDef(
                self.semantic_metadata_from(*l),
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
                    self.semantic_metadata_from(*l),
                    name.clone(),
                    nfields,
                )))
            }
        };

        node
    }

    fn from_module(&mut self, m: &module::Module<u32>) -> Result<module::Module<SemanticMetadata>, String> {
        let meta = self.module_semantic_metadata_from(*m.get_metadata(), m.get_name());

        let mut nmodule = module::Module::new(m.get_name(), meta);
        for module in m.get_modules().iter() {
            nmodule.add_module(self.from_module(module)?);
        }
        for func in m.get_functions().iter() {
           nmodule.add_function(*self.from_parser_ast(func)?);
        }
        for cor in m.get_coroutines().iter() {
            nmodule.add_coroutine(*self.from_parser_ast(cor)?);
        }
        for st in m.get_structs().iter() {
            nmodule.add_struct(*self.from_parser_ast(st)?);
        }
        Ok(nmodule)
    }

    fn semantic_metadata_from(&mut self, l: u32) -> SemanticMetadata {
        let sm_data = SemanticMetadata::new(self.next_id, l, Type::Unknown);
        self.next_id += 1;
        sm_data
    }

    fn module_semantic_metadata_from(&mut self, ln: u32, name: &str) -> SemanticMetadata {
        let sm_data = SemanticMetadata {
            id: self.next_id,
            ln,
            ty: Type::Unknown,
            sym: SymbolTable::new_module(name),
            canonical_path: Path::new(),
        };
        self.next_id += 1;
        sm_data
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
                Ast::Integer(SemanticMetadata::new(0, 1, Type::Unknown), 3),
            ),
            (
                Ast::Boolean(1, true),
                Ast::Boolean(SemanticMetadata::new(0, 1, Type::Unknown), true),
            ),
            (
                Ast::Identifier(1, "x".into()),
                Ast::Identifier(SemanticMetadata::new(0, 1, Type::Unknown), "x".into()),
            ),
        ]
        .iter()
        {
            let mut sa = SemanticAst::new();
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
                    Ast::Integer(SemanticMetadata::new(1, 1, Type::Unknown), 3),
                    Ast::Integer(SemanticMetadata::new(2, 1, Type::Unknown), 3),
                ),
            ),
            (
                (
                    Ast::Identifier(1, "x".into()),
                    Ast::Identifier(1, "y".into()),
                ),
                (
                    Ast::Identifier(SemanticMetadata::new(1, 1, Type::Unknown), "x".into()),
                    Ast::Identifier(SemanticMetadata::new(2, 1, Type::Unknown), "y".into()),
                ),
            ),
            (
                (Ast::Boolean(1, true), Ast::Boolean(1, false)),
                (
                    Ast::Boolean(SemanticMetadata::new(1, 1, Type::Unknown), true),
                    Ast::Boolean(SemanticMetadata::new(2, 1, Type::Unknown), false),
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
                    SemanticMetadata::new(0, 1, Type::Unknown),
                    BinaryOperator::Mul,
                    Box::new(el.clone()),
                    Box::new(er.clone()),
                ),
            )]
            .iter()
            {
                let mut sa = SemanticAst::new();
                let snode = sa.from_parser_ast(tree).unwrap();
                assert_eq!(*snode, *expected);
            }
        }
    }
}
