use crate::{
    diagnostics::config::TracingConfig, parser::pnode::ParserInfo, syntax::structdef::StructDef,
};
use crate::{
    expression::*,
    syntax::path::Path,
    syntax::{
        module::{self, Item},
        routinedef::RoutineDef,
        ty::Type,
    },
};
use crate::syntax::statement::Statement;
use crate::{
    semantics::symbol_table::*,
    syntax::statement::{
        Bind, Mutate, Printbln, Printi, Printiln, Prints, Return, Yield, YieldReturn,
    },
};
use braid_lang::result::Result;

#[derive(Clone, Debug, PartialEq)]
pub struct SemanticMetadata {
    pub id: u32,
    pub ln: u32,
    pub ty: Type,
    pub sym: SymbolTable,
    pub canonical_path: Path,
}

pub type SemanticNode = Expression<SemanticMetadata>;

impl SemanticNode {
    pub fn get_type(&self) -> &Type {
        let meta = self.get_metadata();
        &meta.ty
    }
}

impl Statement<SemanticMetadata> {
    pub fn get_type(&self) -> &Type {
        let m = self.get_metadata();
        &m.ty
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

    pub fn from_parser_ast(&mut self, ast: &Expression<ParserInfo>) -> Result<Box<SemanticNode>> {
        use Expression::*;
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
            Yield(l, box exp) => Ok(Box::new(Yield(
                self.semantic_metadata_from(*l),
                self.from_parser_ast(exp)?,
            ))),
            ExpressionBlock(ln, body, final_exp) => {
                let mut nbody = vec![];
                for stmt in body.iter() {
                    let r = self.from_statement(stmt)?;
                    nbody.push(r);
                }
                let final_exp = match final_exp {
                    None => None,
                    Some(fe) => Some(self.from_parser_ast(fe)?),
                };
                Ok(Box::new(ExpressionBlock(
                    self.semantic_metadata_from(*ln),
                    nbody,
                    final_exp,
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
                    self.semantic_metadata_from(*l),
                    *call,
                    name.clone(),
                    nparams,
                )))
            }
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

    pub fn from_module(
        &mut self,
        m: &module::Module<u32>,
    ) -> Result<module::Module<SemanticMetadata>> {
        let meta = self.module_semantic_metadata_from(*m.get_metadata(), m.get_name());

        let mut nmodule = module::Module::new(m.get_name(), meta);
        for module in m.get_modules().iter() {
            nmodule.add_module(self.from_module(module)?);
        }
        for func in m.get_functions().iter() {
            nmodule.add_item(self.from_item(func)?)?;
        }
        for cor in m.get_coroutines().iter() {
            nmodule.add_item(self.from_item(cor)?)?;
        }
        for st in m.get_structs().iter() {
            nmodule.add_item(self.from_item(st)?)?;
        }
        Ok(nmodule)
    }

    fn from_item(&mut self, m: &Item<u32>) -> Result<module::Item<SemanticMetadata>> {
        match m {
            Item::Struct(s) => self.from_structdef(s).map(|s| Item::Struct(s)),
            Item::Routine(RoutineDef {
                meta: ln,
                def,
                name: fname,
                params,
                ty,
                body,
            }) => {
                let mut nbody = vec![];
                for stmt in body.iter() {
                    let r = self.from_statement(stmt)?;
                    nbody.push(r);
                }
                Ok(Item::Routine(RoutineDef {
                    meta: self.semantic_metadata_from(*ln),
                    def: *def,
                    name: fname.clone(),
                    params: params.clone(),
                    ty: ty.clone(),
                    body: nbody,
                }))
            }
        }
    }

    fn from_structdef(
        &mut self,
        sd: &StructDef<ParserInfo>,
    ) -> Result<StructDef<SemanticMetadata>> {
        if sd.get_name().len() == 0 {
            return Err(format!(
                "Invalid name for StructDef: must not be an empty string"
            ));
        }

        if *sd.get_metadata() == 0 {
            return Err("Source code line must be greater than 0".into());
        }

        let semantic = StructDef::new(
            sd.get_name().clone(),
            self.semantic_metadata_from(*sd.get_metadata()),
            sd.get_fields().clone(),
        );

        Ok(semantic)
    }

    fn from_statement(
        &mut self,
        statement: &Statement<ParserInfo>,
    ) -> Result<Statement<SemanticMetadata>> {
        use Statement::*;

        let inner = match statement {
            Bind(b) => Bind(Box::new(self.from_bind(b)?)),
            Mutate(x) => Mutate(Box::new(self.from_mutate(x)?)),
            Return(x) => Return(Box::new(self.from_return(x)?)),
            YieldReturn(x) => YieldReturn(Box::new(self.from_yieldreturn(x)?)),
            Printi(x) => Printi(Box::new(self.from_printi(x)?)),
            Printiln(x) => Printiln(Box::new(self.from_printiln(x)?)),
            Printbln(x) => Printbln(Box::new(self.from_printbln(x)?)),
            Prints(x) => Prints(Box::new(self.from_prints(x)?)),
            Expression(e) => Expression(self.from_parser_ast(e)?),
        };

        Ok(inner)
    }

    fn from_bind(&mut self, bind: &Bind<ParserInfo>) -> Result<Bind<SemanticMetadata>> {
        Ok(Bind::new(
            self.semantic_metadata_from(*bind.get_metadata()),
            bind.get_id(),
            bind.get_type().clone(),
            bind.is_mutable(),
            *self.from_parser_ast(bind.get_rhs())?,
        ))
    }

    fn from_mutate(&mut self, mutate: &Mutate<ParserInfo>) -> Result<Mutate<SemanticMetadata>> {
        Ok(Mutate::new(
            self.semantic_metadata_from(*mutate.get_metadata()),
            mutate.get_id(),
            *self.from_parser_ast(mutate.get_rhs())?,
        ))
    }

    fn from_printi(&mut self, p: &Printi<ParserInfo>) -> Result<Printi<SemanticMetadata>> {
        Ok(Printi::new(
            self.semantic_metadata_from(*p.get_metadata()),
            *self.from_parser_ast(p.get_value())?,
        ))
    }

    fn from_printiln(&mut self, p: &Printiln<ParserInfo>) -> Result<Printiln<SemanticMetadata>> {
        Ok(Printiln::new(
            self.semantic_metadata_from(*p.get_metadata()),
            *self.from_parser_ast(p.get_value())?,
        ))
    }

    fn from_printbln(&mut self, p: &Printbln<ParserInfo>) -> Result<Printbln<SemanticMetadata>> {
        Ok(Printbln::new(
            self.semantic_metadata_from(*p.get_metadata()),
            *self.from_parser_ast(p.get_value())?,
        ))
    }

    fn from_prints(&mut self, p: &Prints<ParserInfo>) -> Result<Prints<SemanticMetadata>> {
        Ok(Prints::new(
            self.semantic_metadata_from(*p.get_metadata()),
            *self.from_parser_ast(p.get_value())?,
        ))
    }

    fn from_yield(&mut self, y: &Yield<ParserInfo>) -> Result<Yield<SemanticMetadata>> {
        Ok(Yield::new(
            self.semantic_metadata_from(*y.get_metadata()),
            *self.from_parser_ast(y.get_value())?,
        ))
    }

    fn from_yieldreturn(
        &mut self,
        yr: &YieldReturn<ParserInfo>,
    ) -> Result<YieldReturn<SemanticMetadata>> {
        match yr.get_value() {
            None => Ok(YieldReturn::new(
                self.semantic_metadata_from(*yr.get_metadata()),
                None,
            )),
            Some(val) => Ok(YieldReturn::new(
                self.semantic_metadata_from(*yr.get_metadata()),
                Some(*self.from_parser_ast(val)?),
            )),
        }
    }

    fn from_return(&mut self, r: &Return<ParserInfo>) -> Result<Return<SemanticMetadata>> {
        match r.get_value() {
            None => Ok(Return::new(
                self.semantic_metadata_from(*r.get_metadata()),
                None,
            )),
            Some(val) => Ok(Return::new(
                self.semantic_metadata_from(*r.get_metadata()),
                Some(*self.from_parser_ast(val)?),
            )),
        }
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
                Expression::Integer(1, 3),
                Expression::Integer(SemanticMetadata::new(0, 1, Type::Unknown), 3),
            ),
            (
                Expression::Boolean(1, true),
                Expression::Boolean(SemanticMetadata::new(0, 1, Type::Unknown), true),
            ),
            (
                Expression::Identifier(1, "x".into()),
                Expression::Identifier(SemanticMetadata::new(0, 1, Type::Unknown), "x".into()),
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
                (Expression::Integer(1, 3), Expression::Integer(1, 3)),
                (
                    Expression::Integer(SemanticMetadata::new(1, 1, Type::Unknown), 3),
                    Expression::Integer(SemanticMetadata::new(2, 1, Type::Unknown), 3),
                ),
            ),
            (
                (
                    Expression::Identifier(1, "x".into()),
                    Expression::Identifier(1, "y".into()),
                ),
                (
                    Expression::Identifier(SemanticMetadata::new(1, 1, Type::Unknown), "x".into()),
                    Expression::Identifier(SemanticMetadata::new(2, 1, Type::Unknown), "y".into()),
                ),
            ),
            (
                (Expression::Boolean(1, true), Expression::Boolean(1, false)),
                (
                    Expression::Boolean(SemanticMetadata::new(1, 1, Type::Unknown), true),
                    Expression::Boolean(SemanticMetadata::new(2, 1, Type::Unknown), false),
                ),
            ),
        ]
        .iter()
        {
            for (tree, expected) in [(
                Expression::BinaryOp(
                    1,
                    BinaryOperator::Mul,
                    Box::new(l.clone()),
                    Box::new(r.clone()),
                ),
                Expression::BinaryOp(
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
