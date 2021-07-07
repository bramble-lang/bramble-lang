use crate::compiler::ast::*;
use crate::diagnostics::{config::TracingConfig, DiagRecorder};

use super::semanticnode::SemanticAnnotations;
use super::stack::SymbolTableScopeStack;

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
pub struct ForEachPreOrderMut<T>
where
    T: Fn(&SemanticAnnotations) -> String,
{
    pub name: String,
    pub tracing: TracingConfig,
    pub format: T,
    diag: DiagRecorder<SemanticAnnotations, SemanticAnnotations>,
    symbols: SymbolTableScopeStack, // I think I can move this into a Cell<> and then make `resolve_types` into &self instead of &mut self
}

impl<'a, T> ForEachPreOrderMut<T>
where
    T: Fn(&SemanticAnnotations) -> String,
{
    pub fn new(
        name: &str,
        root: &'a mut Module<SemanticAnnotations>,
        tracing: TracingConfig,
        format: T,
    ) -> ForEachPreOrderMut<T> {
        ForEachPreOrderMut {
            name: name.into(),
            tracing,
            format,
            diag: DiagRecorder::new(name, tracing),
            symbols: SymbolTableScopeStack::new(root),
        }
    }

    fn transform<F>(&self, a: &mut SemanticAnnotations, mut f: F)
    where
        F: FnMut(&SymbolTableScopeStack, &mut SemanticAnnotations) + Copy,
    {
        self.diag.begin(a);
        f(&self.symbols, a);
        self.diag.end(a);
    }

    pub fn for_each<F>(&mut self, m: &mut Module<SemanticAnnotations>, f: F)
    where
        F: FnMut(&SymbolTableScopeStack, &mut SemanticAnnotations) + Copy,
    {
        self.diag.start_trace();
        self.for_module(m, f);
        self.diag.end_trace();
    }

    fn for_module<F>(&mut self, m: &mut Module<SemanticAnnotations>, f: F)
    where
        F: FnMut(&SymbolTableScopeStack, &mut SemanticAnnotations) + Copy,
    {
        if self.tracing != TracingConfig::Off {
            println!("{}", self.name);
        }

        self.transform(m.annotation_mut(), f);

        self.symbols.enter_scope(&m.annotation().sym);
        for child_module in m.get_modules_mut().iter_mut() {
            self.for_module(child_module, f);
        }

        self.for_items(m.get_functions_mut(), f);

        self.for_items(m.get_coroutines_mut(), f);

        self.for_items(m.get_structs_mut(), f);
        self.symbols.leave_scope();
    }

    fn for_items<F>(&mut self, items: &mut Vec<Item<SemanticAnnotations>>, f: F)
    where
        F: FnMut(&SymbolTableScopeStack, &mut SemanticAnnotations) + Copy,
    {
        for i in items.iter_mut() {
            match i {
                Item::Struct(sd) => {
                    self.for_structdef(sd, f);
                }
                Item::Routine(rd) => {
                    self.for_routinedef(rd, f);
                }
                Item::Extern(ex) => self.for_extern(ex, f),
            };
        }
    }

    fn for_structdef<F>(&mut self, sd: &mut StructDef<SemanticAnnotations>, f: F)
    where
        F: FnMut(&SymbolTableScopeStack, &mut SemanticAnnotations) + Copy,
    {
        self.transform(sd.annotation_mut(), f);
        self.for_parameters(&mut sd.get_fields_mut(), f);
    }

    fn for_routinedef<F>(&mut self, rd: &mut RoutineDef<SemanticAnnotations>, f: F)
    where
        F: FnMut(&SymbolTableScopeStack, &mut SemanticAnnotations) + Copy,
    {
        self.transform(rd.annotation_mut(), f);
        // loop through all the params
        self.for_parameters(&mut rd.params, f);

        self.symbols.enter_scope(&rd.annotation().sym);
        // loop through every statement and analyze the child nodes of the routine definition
        for e in rd.get_body_mut().iter_mut() {
            self.for_statement(e, f);
        }
        self.symbols.leave_scope();
    }

    fn for_extern<F>(&mut self, ex: &mut Extern<SemanticAnnotations>, f: F)
    where
        F: FnMut(&SymbolTableScopeStack, &mut SemanticAnnotations) + Copy,
    {
        self.transform(ex.annotation_mut(), f);
        self.for_parameters(&mut ex.params, f);
    }

    fn for_parameters<F>(&mut self, params: &mut Vec<Parameter<SemanticAnnotations>>, f: F)
    where
        F: FnMut(&SymbolTableScopeStack, &mut SemanticAnnotations) + Copy,
    {
        for p in params {
            self.transform(p.annotation_mut(), f);
        }
    }

    fn for_statement<F>(&mut self, statement: &mut Statement<SemanticAnnotations>, f: F)
    where
        F: FnMut(&SymbolTableScopeStack, &mut SemanticAnnotations) + Copy,
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

    fn for_bind<F>(&mut self, bind: &mut Bind<SemanticAnnotations>, f: F)
    where
        F: FnMut(&SymbolTableScopeStack, &mut SemanticAnnotations) + Copy,
    {
        self.transform(bind.annotation_mut(), f);
        self.for_expression(bind.get_rhs_mut(), f)
    }

    fn for_mutate<F>(&mut self, mutate: &mut Mutate<SemanticAnnotations>, f: F)
    where
        F: FnMut(&SymbolTableScopeStack, &mut SemanticAnnotations) + Copy,
    {
        self.transform(mutate.annotation_mut(), f);
        self.for_expression(mutate.get_rhs_mut(), f);
    }

    fn for_yieldreturn<F>(&mut self, yr: &mut YieldReturn<SemanticAnnotations>, f: F)
    where
        F: FnMut(&SymbolTableScopeStack, &mut SemanticAnnotations) + Copy,
    {
        self.transform(yr.annotation_mut(), f);
        yr.get_value_mut()
            .as_mut()
            .map(|rv| self.for_expression(rv, f));
    }

    fn for_return<F>(&mut self, r: &mut Return<SemanticAnnotations>, f: F)
    where
        F: FnMut(&SymbolTableScopeStack, &mut SemanticAnnotations) + Copy,
    {
        self.transform(r.annotation_mut(), f);
        r.get_value_mut()
            .as_mut()
            .map(|rv| self.for_expression(rv, f));
    }

    fn for_expression<F>(&mut self, exp: &mut Expression<SemanticAnnotations>, f: F)
    where
        F: FnMut(&SymbolTableScopeStack, &mut SemanticAnnotations) + Copy,
    {
        use Expression::*;

        match exp {
            ExpressionBlock(..) => self.for_expression_block(exp, f),
            Expression::U8(ref mut annotation, _i) => self.transform(annotation, f),
            Expression::U16(ref mut annotation, _i) => self.transform(annotation, f),
            Expression::U32(ref mut annotation, _i) => self.transform(annotation, f),
            Expression::U64(ref mut annotation, _i) => self.transform(annotation, f),
            Expression::I8(ref mut annotation, _i) => self.transform(annotation, f),
            Expression::I16(ref mut annotation, _i) => self.transform(annotation, f),
            Expression::I32(ref mut annotation, _i) => self.transform(annotation, f),
            Expression::I64(ref mut annotation, _i) => self.transform(annotation, f),
            Expression::Boolean(ref mut annotation, _b) => self.transform(annotation, f),
            Expression::StringLiteral(ref mut annotation, _s) => self.transform(annotation, f),
            Expression::ArrayValue(ref mut annotation, el, _) => {
                self.transform(annotation, f);
                for e in el {
                    self.transform(e.annotation_mut(), f);
                }
            }
            Expression::ArrayAt {
                annotation,
                array,
                index,
            } => {
                self.transform(annotation, f);
                self.transform(array.annotation_mut(), f);
                self.transform(index.annotation_mut(), f);
            }
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
            While { .. } => todo!(),
            Yield(..) => self.for_yield(exp, f),
            RoutineCall(..) => self.for_routine_call(exp, f),
            StructExpression(..) => self.for_struct_expression(exp, f),
        }
    }

    fn for_expression_block<F>(&mut self, block: &mut Expression<SemanticAnnotations>, f: F)
    where
        F: FnMut(&SymbolTableScopeStack, &mut SemanticAnnotations) + Copy,
    {
        if let Expression::ExpressionBlock(ref mut annotation, ref mut body, ref mut final_exp) =
            block
        {
            self.transform(annotation, f);

            self.symbols.enter_scope(&annotation.sym);
            for e in body.iter_mut() {
                self.for_statement(e, f);
            }

            final_exp.as_mut().map(|fe| self.for_expression(fe, f));
            self.symbols.leave_scope();
        } else {
            panic!("Expected ExpressionBlock, but got {:?}", block)
        }
    }

    fn for_member_access<F>(&mut self, access: &mut Expression<SemanticAnnotations>, f: F)
    where
        F: FnMut(&SymbolTableScopeStack, &mut SemanticAnnotations) + Copy,
    {
        if let Expression::MemberAccess(ref mut annotation, src, _member) = access {
            self.transform(annotation, f);
            self.for_expression(src, f);
        } else {
            panic!("Expected MemberAccess, but got {:?}", access)
        }
    }

    fn for_unary_op<F>(&mut self, un_op: &mut Expression<SemanticAnnotations>, f: F)
    where
        F: FnMut(&SymbolTableScopeStack, &mut SemanticAnnotations) + Copy,
    {
        if let Expression::UnaryOp(ref mut annotation, _op, operand) = un_op {
            self.transform(annotation, f);
            self.for_expression(operand, f);
        } else {
            panic!("Expected UnaryOp, but got {:?}", un_op)
        }
    }

    fn for_binary_op<F>(&mut self, bin_op: &mut Expression<SemanticAnnotations>, f: F)
    where
        F: FnMut(&SymbolTableScopeStack, &mut SemanticAnnotations) + Copy,
    {
        if let Expression::BinaryOp(ref mut annotation, _op, l, r) = bin_op {
            self.transform(annotation, f);
            self.for_expression(l, f);
            self.for_expression(r, f);
        } else {
            panic!("Expected BinaryOp, but got {:?}", bin_op)
        }
    }

    fn for_if<F>(&mut self, if_exp: &mut Expression<SemanticAnnotations>, f: F)
    where
        F: FnMut(&SymbolTableScopeStack, &mut SemanticAnnotations) + Copy,
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

    fn for_while<F>(&mut self, while_exp: &mut Expression<SemanticAnnotations>, f: F)
    where
        F: FnMut(&SymbolTableScopeStack, &mut SemanticAnnotations) + Copy,
    {
        if let Expression::While {
            ref mut annotation,
            cond,
            body,
        } = while_exp
        {
            self.transform(annotation, f);
            self.for_expression(cond, f);
            self.for_expression(body, f);
        } else {
            panic!("Expected WhileExpression, but got {:?}", while_exp)
        }
    }

    fn for_routine_call<F>(&mut self, rc: &mut Expression<SemanticAnnotations>, f: F)
    where
        F: FnMut(&SymbolTableScopeStack, &mut SemanticAnnotations) + Copy,
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

    fn for_yield<F>(&mut self, yield_exp: &mut Expression<SemanticAnnotations>, f: F)
    where
        F: FnMut(&SymbolTableScopeStack, &mut SemanticAnnotations) + Copy,
    {
        if let Expression::Yield(ref mut annotation, e) = yield_exp {
            self.transform(annotation, f);
            self.for_expression(e, f);
        } else {
            panic!("Expected Yield, but got {:?}", yield_exp)
        }
    }

    fn for_struct_expression<F>(&mut self, se: &mut Expression<SemanticAnnotations>, f: F)
    where
        F: FnMut(&SymbolTableScopeStack, &mut SemanticAnnotations) + Copy,
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
    use crate::compiler::{
        lexer::tokens::Token, parser::parser, semantics::semanticnode::SemanticAst, Lexer,
    };
    use braid_lang::result::Result;

    use super::*;

    #[test]
    fn empty_module_ignore_stack() {
        let text = "mod m{}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let ast = parser::parse("test", &tokens)
            .expect(&format!("{}", text))
            .unwrap();
        let mut sa = SemanticAst::new();
        let mut sm_ast = sa.from_module(&ast, TracingConfig::Off);

        let mut t =
            ForEachPreOrderMut::new("test", &mut sm_ast, TracingConfig::Off, |_| "test".into());
        t.for_module(&mut sm_ast, |_stack, n| n.ln *= 4);

        assert_eq!(sm_ast.annotation().ln, 4);
    }

    #[test]
    fn empty_module_use_stack() {
        let text = "mod m{}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let ast = parser::parse("test", &tokens)
            .expect(&format!("{}", text))
            .unwrap();
        let mut sa = SemanticAst::new();
        let mut sm_ast = sa.from_module(&ast, TracingConfig::Off);

        let mut t =
            ForEachPreOrderMut::new("test", &mut sm_ast, TracingConfig::Off, |_| "test".into());
        t.for_module(&mut sm_ast, |stack, n| {
            let cpath = stack.to_canonical(&vec!["annotation"].into()).unwrap();
            n.set_canonical_path(cpath);
        });

        assert_eq!(
            *sm_ast.annotation().get_canonical_path(),
            vec!["project", "test", "annotation"].into()
        );
        assert_eq!(
            *sm_ast
                .get_module("m")
                .unwrap()
                .annotation()
                .get_canonical_path(),
            vec!["project", "test", "m", "annotation"].into()
        );
    }

    /*#[test]
    fn module_with_items() {
        let mut m = Module::new("m", 1);
        m.add_function(RoutineDef::new_coroutine(
            "cor",
            1,
            vec![],
            Type::Unit,
            vec![Statement::Expression(box Expression::I64(1, 2))],
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
            vec![Statement::Expression(box Expression::I64(1, 2))],
        ))
        .unwrap();
        m.add_module(Module::new("m2", 1));
        m.add_struct(StructDef::new("sd", 1, vec![])).unwrap();

        let t = ForEachPreOrderMut::new("test", TracingConfig::Off, |_| "test".into());
        t.for_module(&mut m, |n| *n = 2);

        for n in m.iter_preorder() {
            assert_eq!(*n.annotation(), 2);
        }
    }*/
}
