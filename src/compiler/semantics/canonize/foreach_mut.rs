use crate::compiler::import::Import;
use crate::result::Result;

use crate::compiler::ast::*;
use crate::diagnostics::{config::TracingConfig, DiagRecorder};

use super::super::{semanticnode::SemanticContext, stack::SymbolTableScopeStack};
use super::Canonizable;

/**
Traverse through each node, in pre-order DFS, and apply a function to mutate the
context on that node.

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
    T: Fn(&SemanticContext) -> String,
{
    pub name: String,
    pub tracing: TracingConfig,
    pub format: T,
    diag: DiagRecorder<SemanticContext, SemanticContext>,
    symbols: SymbolTableScopeStack, // I think I can move this into a Cell<> and then make `resolve_types` into &self instead of &mut self
}

impl<'a, T> ForEachPreOrderMut<T>
where
    T: Fn(&SemanticContext) -> String,
{
    pub fn new(
        name: &str,
        root: &'a mut Module<SemanticContext>,
        imports: &[Import],
        tracing: TracingConfig,
        format: T,
    ) -> ForEachPreOrderMut<T> {
        ForEachPreOrderMut {
            name: name.into(),
            tracing,
            format,
            diag: DiagRecorder::new(name, tracing),
            symbols: SymbolTableScopeStack::new(root, imports),
        }
    }

    fn transform<F>(&mut self, a: &mut dyn Canonizable, mut f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn Canonizable) -> Result<()> + Copy,
    {
        self.diag.begin(a.get_context());
        let r = f(&self.symbols, a).map_err(|e| format!("L{}: {}", a.get_context().line(), e));
        self.diag.end(a.get_context());
        r
    }

    pub fn for_each<F>(&mut self, m: &mut Module<SemanticContext>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn Canonizable) -> Result<()> + Copy,
    {
        if self.tracing != TracingConfig::Off {
            println!("ForEach: {}", self.name);
        }

        self.diag.start_trace();
        let r = self.for_module(m, f);
        self.diag.end_trace();

        if self.tracing != TracingConfig::Off {
            println!("ForEach: {:?}", r);
        }

        r.map_err(|e| format!("Semantic: {}", e))
    }

    fn for_module<F>(&mut self, m: &mut Module<SemanticContext>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn Canonizable) -> Result<()> + Copy,
    {
        self.symbols.enter_scope(&m.get_context().sym);
        let r = self.transform(m, f);

        for child_module in m.get_modules_mut().iter_mut() {
            self.for_module(child_module, f)?;
        }

        self.for_items(m.get_functions_mut(), f)?;
        self.for_items(m.get_coroutines_mut(), f)?;
        self.for_items(m.get_structs_mut(), f)?;
        self.for_items(m.get_externs_mut(), f)?;

        self.symbols.leave_scope();
        r
    }

    fn for_items<F>(&mut self, items: &mut Vec<Item<SemanticContext>>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn Canonizable) -> Result<()> + Copy,
    {
        for i in items.iter_mut() {
            match i {
                Item::Struct(sd) => self.for_structdef(sd, f)?,
                Item::Routine(rd) => self.for_routinedef(rd, f)?,
                Item::Extern(ex) => self.for_extern(ex, f)?,
            };
        }
        Ok(())
    }

    fn for_structdef<F>(&mut self, sd: &mut StructDef<SemanticContext>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn Canonizable) -> Result<()> + Copy,
    {
        let r = self.transform(sd, f);
        self.for_parameters(&mut sd.get_fields_mut(), f)?;
        r
    }

    fn for_routinedef<F>(&mut self, rd: &mut RoutineDef<SemanticContext>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn Canonizable) -> Result<()> + Copy,
    {
        self.symbols.enter_scope(&rd.get_context().sym);
        let r = self.transform(rd, f);
        // loop through all the params
        self.for_parameters(&mut rd.params, f)?;

        // loop through every statement and analyze the child nodes of the routine definition
        for e in rd.get_body_mut().iter_mut() {
            self.for_statement(e, f)?;
        }
        self.symbols.leave_scope();
        r
    }

    fn for_extern<F>(&mut self, ex: &mut Extern<SemanticContext>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn Canonizable) -> Result<()> + Copy,
    {
        let r = self.transform(ex, f);
        self.for_parameters(&mut ex.params, f)?;
        r
    }

    fn for_parameters<F>(
        &mut self,
        params: &mut Vec<Parameter<SemanticContext>>,
        f: F,
    ) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn Canonizable) -> Result<()> + Copy,
    {
        for p in params {
            self.transform(p, f)?;
        }
        Ok(())
    }

    fn for_statement<F>(&mut self, statement: &mut Statement<SemanticContext>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn Canonizable) -> Result<()> + Copy,
    {
        match statement {
            Statement::Bind(b) => {
                self.for_bind(b, f)?;
            }
            Statement::Mutate(m) => {
                self.for_mutate(m, f)?;
            }
            Statement::Return(r) => {
                self.for_return(r, f)?;
            }
            Statement::YieldReturn(yr) => {
                self.for_yieldreturn(yr, f)?;
            }
            Statement::Expression(e) => {
                self.for_expression(e, f)?;
            }
        };
        Ok(())
    }

    fn for_bind<F>(&mut self, bind: &mut Bind<SemanticContext>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn Canonizable) -> Result<()> + Copy,
    {
        let r = self.transform(bind, f);
        self.for_expression(bind.get_rhs_mut(), f)?;
        r
    }

    fn for_mutate<F>(&mut self, mutate: &mut Mutate<SemanticContext>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn Canonizable) -> Result<()> + Copy,
    {
        let r = self.transform(mutate, f);
        self.for_expression(mutate.get_rhs_mut(), f)?;
        r
    }

    fn for_yieldreturn<F>(&mut self, yr: &mut YieldReturn<SemanticContext>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn Canonizable) -> Result<()> + Copy,
    {
        let r = self.transform(yr, f);
        match yr.get_value_mut() {
            Some(rv) => self.for_expression(rv, f)?,
            None => (),
        }
        r
    }

    fn for_return<F>(&mut self, ret: &mut Return<SemanticContext>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn Canonizable) -> Result<()> + Copy,
    {
        let r = self.transform(ret, f);

        match ret.get_value_mut() {
            Some(rv) => self.for_expression(rv, f)?,
            None => (),
        }

        r
    }

    fn for_expression<F>(&mut self, exp: &mut Expression<SemanticContext>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn Canonizable) -> Result<()> + Copy,
    {
        use Expression::*;

        match exp {
            ExpressionBlock(..) => self.for_expression_block(exp, f),
            Expression::U8(..) => self.transform(exp, f),
            Expression::U16(..) => self.transform(exp, f),
            Expression::U32(..) => self.transform(exp, f),
            Expression::U64(..) => self.transform(exp, f),
            Expression::I8(..) => self.transform(exp, f),
            Expression::I16(..) => self.transform(exp, f),
            Expression::I32(..) => self.transform(exp, f),
            Expression::I64(..) => self.transform(exp, f),
            Expression::Boolean(..) => self.transform(exp, f),
            Expression::StringLiteral(..) => self.transform(exp, f),
            Expression::ArrayExpression(_, el, _) => {
                for e in el {
                    self.for_expression(e, f)?;
                    //println!("{} Checking Elements => {}", self.name, e.annotation().ty);
                }
                //println!("{} Checking ArrayValue", self.name);
                self.transform(exp, f)
            }
            Expression::ArrayAt { array, index, .. } => {
                self.for_expression(array, f)?;
                self.for_expression(index, f)?;
                self.transform(exp, f)
            }
            Expression::CustomType(..) => self.transform(exp, f),
            Expression::Identifier(..) => self.transform(exp, f),
            Path(..) => self.transform(exp, f),
            Expression::IdentifierDeclare(..) => self.transform(exp, f),
            MemberAccess(..) => self.for_member_access(exp, f),
            UnaryOp(..) => self.for_unary_op(exp, f),
            BinaryOp(..) => self.for_binary_op(exp, f),
            If { .. } => self.for_if(exp, f),
            While { .. } => self.for_while(exp, f),
            Yield(..) => self.for_yield(exp, f),
            RoutineCall(..) => self.for_routine_call(exp, f),
            StructExpression(..) => self.for_struct_expression(exp, f),
        }
    }

    fn for_expression_block<F>(
        &mut self,
        block: &mut Expression<SemanticContext>,
        f: F,
    ) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn Canonizable) -> Result<()> + Copy,
    {
        self.symbols.enter_scope(&block.get_context().sym);
        let r = self.transform(block, f);
        if let Expression::ExpressionBlock(_, ref mut body, ref mut final_exp) = block {
            for e in body.iter_mut() {
                self.for_statement(e, f)?;
            }

            match final_exp {
                Some(fe) => self.for_expression(fe, f)?,
                None => (),
            }

            self.symbols.leave_scope();
        } else {
            panic!("Expected ExpressionBlock, but got {:?}", block)
        }
        r
    }

    fn for_member_access<F>(&mut self, access: &mut Expression<SemanticContext>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn Canonizable) -> Result<()> + Copy,
    {
        let r = self.transform(access, f);
        if let Expression::MemberAccess(_, src, _member) = access {
            self.for_expression(src, f)?;
        } else {
            panic!("Expected MemberAccess, but got {:?}", access)
        }
        r
    }

    fn for_unary_op<F>(&mut self, un_op: &mut Expression<SemanticContext>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn Canonizable) -> Result<()> + Copy,
    {
        let r = self.transform(un_op, f);
        if let Expression::UnaryOp(_, _op, operand) = un_op {
            self.for_expression(operand, f)?;
        } else {
            panic!("Expected UnaryOp, but got {:?}", un_op)
        }
        r
    }

    fn for_binary_op<F>(&mut self, bin_op: &mut Expression<SemanticContext>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn Canonizable) -> Result<()> + Copy,
    {
        let r = self.transform(bin_op, f);
        if let Expression::BinaryOp(_, _op, l, r) = bin_op {
            self.for_expression(l, f)?;
            self.for_expression(r, f)?;
        } else {
            panic!("Expected BinaryOp, but got {:?}", bin_op)
        }
        r
    }

    fn for_if<F>(&mut self, if_exp: &mut Expression<SemanticContext>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn Canonizable) -> Result<()> + Copy,
    {
        let r = self.transform(if_exp, f);
        if let Expression::If {
            cond,
            if_arm,
            else_arm,
            ..
        } = if_exp
        {
            self.for_expression(cond, f)?;
            self.for_expression(if_arm, f)?;
            match else_arm {
                Some(else_arm) => self.for_expression(else_arm, f)?,
                None => (),
            }
            //else_arm.as_mut().map(|ea| self.for_expression(ea, f));
        } else {
            panic!("Expected IfExpression, but got {:?}", if_exp)
        }
        r
    }

    fn for_while<F>(&mut self, while_exp: &mut Expression<SemanticContext>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn Canonizable) -> Result<()> + Copy,
    {
        let r = self.transform(while_exp, f);
        if let Expression::While { cond, body, .. } = while_exp {
            self.for_expression(cond, f)?;
            self.for_expression(body, f)?;
        } else {
            panic!("Expected WhileExpression, but got {:?}", while_exp)
        }
        r
    }

    fn for_routine_call<F>(&mut self, rc: &mut Expression<SemanticContext>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn Canonizable) -> Result<()> + Copy,
    {
        let r = self.transform(rc, f);
        if let Expression::RoutineCall(_, _call, _name, params) = rc {
            for p in params {
                self.for_expression(p, f)?;
            }
        } else {
            panic!("Expected RoutineCall, but got {:?}", rc)
        }
        r
    }

    fn for_yield<F>(&mut self, yield_exp: &mut Expression<SemanticContext>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn Canonizable) -> Result<()> + Copy,
    {
        let r = self.transform(yield_exp, f);
        if let Expression::Yield(_, e) = yield_exp {
            self.for_expression(e, f)?;
        } else {
            panic!("Expected Yield, but got {:?}", yield_exp)
        }
        r
    }

    fn for_struct_expression<F>(&mut self, se: &mut Expression<SemanticContext>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn Canonizable) -> Result<()> + Copy,
    {
        let r = self.transform(se, f);
        if let Expression::StructExpression(_, _struct_name, fields) = se {
            for (_, fe) in fields {
                self.for_expression(fe, f)?;
            }
        } else {
            panic!("Expected StructExpression, but got {:?}", se)
        }
        r
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::{
        lexer::stringtable::StringTable, lexer::tokens::Token, lexer::LexerError, parser::parser,
        semantics::semanticnode::SemanticAst, Lexer,
    };

    use super::*;

    type LResult = std::result::Result<Vec<Token>, LexerError>;

    #[test]
    fn empty_module_ignore_stack() {
        let text = "mod m{}";
        let mut table = StringTable::new();
        let test = table.insert("test".into());

        let tokens: Vec<Token> = Lexer::new(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let ast = parser::parse(test, &tokens)
            .expect(&format!("{}", text))
            .unwrap();
        let mut sa = SemanticAst::new();
        let mut sm_ast = sa.from_module(&ast, TracingConfig::Off);

        let mut t =
            ForEachPreOrderMut::new("test", &mut sm_ast, &vec![], TracingConfig::Off, |_| {
                "test".into()
            });
        t.for_module(&mut sm_ast, |_stack, n| {
            n.get_context_mut().ty = Type::I64;
            Ok(())
        })
        .unwrap();

        assert_eq!(sm_ast.get_context().ty, Type::I64);
    }

    #[test]
    fn empty_module_use_stack() {
        let text = "mod m{}";
        let mut table = StringTable::new();
        let test = table.insert("test".into());
        let annotation = table.insert("annotation".into());
        let m = table.insert("m".into());

        let tokens: Vec<Token> = Lexer::new(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let ast = parser::parse(test, &tokens)
            .expect(&format!("{}", text))
            .unwrap();
        let mut sa = SemanticAst::new();
        let mut sm_ast = sa.from_module(&ast, TracingConfig::Off);

        let mut t =
            ForEachPreOrderMut::new("test", &mut sm_ast, &vec![], TracingConfig::Off, |_| {
                "test".into()
            });
        t.for_module(&mut sm_ast, |stack, n| {
            let cpath = stack
                .to_canonical(&vec![Element::Id(annotation)].into())
                .unwrap();
            n.get_context_mut().set_canonical_path(cpath);
            Ok(())
        })
        .unwrap();

        assert_eq!(
            *sm_ast.get_context().get_canonical_path(),
            vec![
                Element::CanonicalRoot,
                Element::Id(test),
                Element::Id(annotation)
            ]
            .into()
        );
        assert_eq!(
            *sm_ast
                .get_module(m)
                .unwrap()
                .get_context()
                .get_canonical_path(),
            vec![
                Element::CanonicalRoot,
                Element::Id(test),
                Element::Id(m),
                Element::Id(annotation)
            ]
            .into()
        );
    }

    #[test]
    fn canonize_structdef() {
        let text = "mod m{ struct MyStruct {}}";
        let mut table = StringTable::new();
        let test = table.insert("test".into());
        let m = table.insert("m".into());
        let my_struct = table.insert("MyStruct".into());

        let tokens: Vec<Token> = Lexer::new(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let ast = parser::parse(test, &tokens)
            .expect(&format!("{}", text))
            .unwrap();
        let mut sa = SemanticAst::new();
        let mut sm_ast = sa.from_module(&ast, TracingConfig::Off);

        let mut t =
            ForEachPreOrderMut::new("test", &mut sm_ast, &vec![], TracingConfig::Off, |_| {
                "test".into()
            });
        t.for_module(&mut sm_ast, |stack, n| match n.name() {
            Some(name) => {
                let cpath = stack.to_canonical(&vec![Element::Id(name)].into()).unwrap();
                n.get_context_mut().set_canonical_path(cpath);
                Ok(())
            }
            None => Ok(()),
        })
        .unwrap();

        assert_eq!(
            *sm_ast
                .get_module(m)
                .unwrap()
                .get_item(my_struct)
                .unwrap()
                .get_context()
                .get_canonical_path(),
            vec![
                Element::CanonicalRoot,
                Element::Id(test),
                Element::Id(m),
                Element::Id(my_struct)
            ]
            .into()
        );
    }
}
