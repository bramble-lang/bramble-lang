use crate::syntax::{
    annotate::Annotation, node::Node, parameter::Parameter, statement::Statement,
};
use crate::{
    diagnostics::config::TracingConfig, parser::parser::ParserInfo, syntax::structdef::StructDef,
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
use crate::{
    semantics::symbol_table::*,
    syntax::statement::{Bind, Mutate, Return, Yield, YieldReturn},
};
use braid_lang::result::Result;

#[derive(Clone, Debug, PartialEq)]
pub struct SemanticAnnotations {
    pub id: u32,
    pub ln: u32,
    pub ty: Type,
    pub sym: SymbolTable,
    pub canonical_path: Path,
}

impl Annotation for SemanticAnnotations {
    fn id(&self) -> u32 {
        self.id
    }

    fn line(&self) -> u32 {
        self.ln
    }
}

pub type SemanticNode = Expression<SemanticAnnotations>;

impl SemanticNode {
    pub fn get_type(&self) -> &Type {
        let meta = self.annotation();
        &meta.ty
    }
}

impl Statement<SemanticAnnotations> {
    pub fn get_type(&self) -> &Type {
        let m = self.annotation();
        &m.ty
    }
}

impl SemanticAnnotations {
    pub fn new(id: u32, ln: u32, ty: Type) -> SemanticAnnotations {
        SemanticAnnotations {
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

    pub fn id(&self) -> u32 {
        self.id
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

    pub fn from_module(
        &mut self,
        m: &module::Module<u32>,
    ) -> Result<module::Module<SemanticAnnotations>> {
        let meta = self.module_semantic_annotations_from(*m.annotation(), m.get_name());

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

    fn from_item(&mut self, m: &Item<u32>) -> Result<module::Item<SemanticAnnotations>> {
        match m {
            Item::Struct(s) => self.from_structdef(s).map(|s| Item::Struct(s)),
            Item::Routine(rd) => self.from_routinedef(rd).map(|r| Item::Routine(r)),
        }
    }

    fn from_routinedef(
        &mut self,
        rd: &RoutineDef<ParserInfo>,
    ) -> Result<RoutineDef<SemanticAnnotations>> {
        let RoutineDef {
            annotations: ln,
            def,
            name: fname,
            params,
            ty,
            body,
            ..
        } = rd;

        let mut nbody = vec![];
        for stmt in body.iter() {
            let r = self.from_statement(stmt)?;
            nbody.push(r);
        }
        Ok(RoutineDef {
            annotations: self.semantic_annotations_from(*ln),
            def: *def,
            name: fname.clone(),
            params: self.from_parameters(params),
            ty: ty.clone(),
            body: nbody,
        })
    }

    fn from_structdef(
        &mut self,
        sd: &StructDef<ParserInfo>,
    ) -> Result<StructDef<SemanticAnnotations>> {
        if sd.get_name().len() == 0 {
            return Err(format!(
                "Invalid name for StructDef: must not be an empty string"
            ));
        }

        if *sd.annotation() == 0 {
            return Err("Source code line must be greater than 0".into());
        }

        let semantic = StructDef::new(
            sd.get_name().clone(),
            self.semantic_annotations_from(*sd.annotation()),
            self.from_parameters(sd.get_fields()),
        );

        Ok(semantic)
    }

    fn from_parameters(
        &mut self,
        params: &Vec<Parameter<ParserInfo>>,
    ) -> Vec<Parameter<SemanticAnnotations>> {
        params
            .iter()
            .map(|p| p.map_annotation(|a| self.semantic_annotations_from(*a)))
            .collect()
    }

    fn from_statement(
        &mut self,
        statement: &Statement<ParserInfo>,
    ) -> Result<Statement<SemanticAnnotations>> {
        use Statement::*;

        let inner = match statement {
            Bind(b) => Bind(Box::new(self.from_bind(b)?)),
            Mutate(x) => Mutate(Box::new(self.from_mutate(x)?)),
            Return(x) => Return(Box::new(self.from_return(x)?)),
            YieldReturn(x) => YieldReturn(Box::new(self.from_yieldreturn(x)?)),
            Expression(e) => Expression(self.from_expression(e)?),
        };

        Ok(inner)
    }

    fn from_bind(&mut self, bind: &Bind<ParserInfo>) -> Result<Bind<SemanticAnnotations>> {
        Ok(Bind::new(
            self.semantic_annotations_from(*bind.annotation()),
            bind.get_id(),
            bind.get_type().clone(),
            bind.is_mutable(),
            *self.from_expression(bind.get_rhs())?,
        ))
    }

    fn from_mutate(&mut self, mutate: &Mutate<ParserInfo>) -> Result<Mutate<SemanticAnnotations>> {
        Ok(Mutate::new(
            self.semantic_annotations_from(*mutate.annotation()),
            mutate.get_id(),
            *self.from_expression(mutate.get_rhs())?,
        ))
    }

    fn from_yieldreturn(
        &mut self,
        yr: &YieldReturn<ParserInfo>,
    ) -> Result<YieldReturn<SemanticAnnotations>> {
        match yr.get_value() {
            None => Ok(YieldReturn::new(
                self.semantic_annotations_from(*yr.annotation()),
                None,
            )),
            Some(val) => Ok(YieldReturn::new(
                self.semantic_annotations_from(*yr.annotation()),
                Some(*self.from_expression(val)?),
            )),
        }
    }

    fn from_return(&mut self, r: &Return<ParserInfo>) -> Result<Return<SemanticAnnotations>> {
        match r.get_value() {
            None => Ok(Return::new(
                self.semantic_annotations_from(*r.annotation()),
                None,
            )),
            Some(val) => Ok(Return::new(
                self.semantic_annotations_from(*r.annotation()),
                Some(*self.from_expression(val)?),
            )),
        }
    }

    fn from_expression(&mut self, ast: &Expression<ParserInfo>) -> Result<Box<SemanticNode>> {
        use Expression::*;
        let node = match ast {
            Integer32(ln, val) => Ok(Box::new(Integer32(
                self.semantic_annotations_from(*ln),
                *val,
            ))),
            Integer64(ln, val) => Ok(Box::new(Integer64(
                self.semantic_annotations_from(*ln),
                *val,
            ))),
            Boolean(ln, val) => Ok(Box::new(Boolean(self.semantic_annotations_from(*ln), *val))),
            StringLiteral(ln, val) => Ok(Box::new(StringLiteral(
                self.semantic_annotations_from(*ln),
                val.clone(),
            ))),
            CustomType(ln, val) => Ok(Box::new(CustomType(
                self.semantic_annotations_from(*ln),
                val.clone(),
            ))),
            IdentifierDeclare(ln, name, p) => Ok(Box::new(IdentifierDeclare(
                self.semantic_annotations_from(*ln),
                name.clone(),
                p.clone(),
            ))),
            Identifier(ln, id) => Ok(Box::new(Identifier(
                self.semantic_annotations_from(*ln),
                id.clone(),
            ))),
            Path(ln, path) => Ok(Box::new(Path(
                self.semantic_annotations_from(*ln),
                path.clone(),
            ))),
            MemberAccess(ln, src, member) => Ok(Box::new(MemberAccess(
                self.semantic_annotations_from(*ln),
                self.from_expression(src)?,
                member.clone(),
            ))),
            BinaryOp(ln, op, ref l, ref r) => Ok(Box::new(BinaryOp(
                self.semantic_annotations_from(*ln),
                *op,
                self.from_expression(l)?,
                self.from_expression(r)?,
            ))),
            UnaryOp(ln, op, ref operand) => Ok(Box::new(UnaryOp(
                self.semantic_annotations_from(*ln),
                *op,
                self.from_expression(operand)?,
            ))),
            If {
                annotation: ln,
                cond,
                if_arm,
                else_arm,
            } => {
                let cond = self.from_expression(cond)?;
                let if_arm = self.from_expression(if_arm)?;
                let else_arm = match else_arm {
                    Some(e) => Some(self.from_expression(e)?),
                    None => None,
                };
                Ok(Box::new(If {
                    annotation: self.semantic_annotations_from(*ln),
                    cond,
                    if_arm,
                    else_arm: else_arm,
                }))
            }
            Yield(l, box exp) => Ok(Box::new(Yield(
                self.semantic_annotations_from(*l),
                self.from_expression(exp)?,
            ))),
            ExpressionBlock(ln, body, final_exp) => {
                let mut nbody = vec![];
                for stmt in body.iter() {
                    let r = self.from_statement(stmt)?;
                    nbody.push(r);
                }
                let final_exp = match final_exp {
                    None => None,
                    Some(fe) => Some(self.from_expression(fe)?),
                };
                Ok(Box::new(ExpressionBlock(
                    self.semantic_annotations_from(*ln),
                    nbody,
                    final_exp,
                )))
            }
            RoutineCall(l, call, name, params) => {
                // test that the expressions passed to the function match the functions
                // parameter types
                let mut nparams = vec![];
                for param in params.iter() {
                    let np = self.from_expression(param)?;
                    nparams.push(*np);
                }
                Ok(Box::new(RoutineCall(
                    self.semantic_annotations_from(*l),
                    *call,
                    name.clone(),
                    nparams,
                )))
            }
            StructExpression(l, name, fields) => {
                let mut nfields = vec![];
                for (fname, fvalue) in fields.iter() {
                    let fvalue2 = self.from_expression(fvalue)?;
                    nfields.push((fname.clone(), *fvalue2));
                }
                Ok(Box::new(StructExpression(
                    self.semantic_annotations_from(*l),
                    name.clone(),
                    nfields,
                )))
            }
        };

        node
    }

    fn from_yield(&mut self, y: &Yield<ParserInfo>) -> Result<Yield<SemanticAnnotations>> {
        Ok(Yield::new(
            self.semantic_annotations_from(*y.annotation()),
            *self.from_expression(y.get_value())?,
        ))
    }

    fn semantic_annotations_from(&mut self, l: u32) -> SemanticAnnotations {
        let sm_data = SemanticAnnotations::new(self.next_id, l, Type::Unknown);
        self.next_id += 1;
        sm_data
    }

    fn module_semantic_annotations_from(&mut self, ln: u32, name: &str) -> SemanticAnnotations {
        let sm_data = SemanticAnnotations {
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
                Expression::Integer64(1, 3),
                Expression::Integer64(SemanticAnnotations::new(0, 1, Type::Unknown), 3),
            ),
            (
                Expression::Boolean(1, true),
                Expression::Boolean(SemanticAnnotations::new(0, 1, Type::Unknown), true),
            ),
            (
                Expression::Identifier(1, "x".into()),
                Expression::Identifier(SemanticAnnotations::new(0, 1, Type::Unknown), "x".into()),
            ),
        ]
        .iter()
        {
            let mut sa = SemanticAst::new();
            let snode = sa.from_expression(&node).unwrap();
            assert_eq!(*snode, *expected);
        }
    }

    #[test]
    fn test_multi_nodes() {
        for ((l, r), (el, er)) in [
            (
                (Expression::Integer64(1, 3), Expression::Integer64(1, 3)),
                (
                    Expression::Integer64(SemanticAnnotations::new(1, 1, Type::Unknown), 3),
                    Expression::Integer64(SemanticAnnotations::new(2, 1, Type::Unknown), 3),
                ),
            ),
            (
                (
                    Expression::Identifier(1, "x".into()),
                    Expression::Identifier(1, "y".into()),
                ),
                (
                    Expression::Identifier(
                        SemanticAnnotations::new(1, 1, Type::Unknown),
                        "x".into(),
                    ),
                    Expression::Identifier(
                        SemanticAnnotations::new(2, 1, Type::Unknown),
                        "y".into(),
                    ),
                ),
            ),
            (
                (Expression::Boolean(1, true), Expression::Boolean(1, false)),
                (
                    Expression::Boolean(SemanticAnnotations::new(1, 1, Type::Unknown), true),
                    Expression::Boolean(SemanticAnnotations::new(2, 1, Type::Unknown), false),
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
                    SemanticAnnotations::new(0, 1, Type::Unknown),
                    BinaryOperator::Mul,
                    Box::new(el.clone()),
                    Box::new(er.clone()),
                ),
            )]
            .iter()
            {
                let mut sa = SemanticAst::new();
                let snode = sa.from_expression(tree).unwrap();
                assert_eq!(*snode, *expected);
            }
        }
    }
}
