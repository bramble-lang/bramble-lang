use braid_lang::result::Result;

use crate::compiler::ast::*;
use crate::diagnostics::{config::TracingConfig, DiagRecorder};

use super::semanticnode::SemanticAnnotations;
use super::stack::SymbolTableScopeStack;

/// A collection of functions that are specific to Semantic Analysis
/// on the AST. Moving Semantic Analysis functions to a trait allows
/// the structural operations (traversal, scope stack) to be managed
/// by operators (e.g. ForEach) which can then apply transformations
/// that use the SemanticNode information
///
/// Because contextual information is need for semantic analysis
/// operations, the scope stack is passed into these functions.
pub trait SemanticNode: Node<SemanticAnnotations> {
    // TODO: make one canonize function that handles everything and then the special cases
    // do their own thing.  I think that will be easier than 3 separate functions
    fn canonize_annotation_path(&mut self, stack: &SymbolTableScopeStack) -> Result<()> {
        // If this node has a name, then use the current stack to construct
        // a canonical path from the root of the AST to the current node
        // (this is for routine definitions, modules, and structure definitions)
        match self.name() {
            // Set SemanticAnnotation::canonical_path to CanonicalPath
            // Addresses RoutineDefs and StructDefs (for LLVM IR)
            Some(name) => {
                let cpath = stack.to_canonical(&vec![name].into())?;
                self.annotation_mut().set_canonical_path(cpath);
            }
            None => (),
        }
        Ok(())
    }

    fn canonize_annotation_type(&mut self, stack: &SymbolTableScopeStack) -> Result<()> {
        // Update the Type information in the annotation data
        let ctype = stack.canonize_local_type_ref(self.annotation().ty())?;
        self.annotation_mut().ty = ctype;
        Ok(())
    }

    fn canonize_type_refs(&mut self, _stack: &SymbolTableScopeStack) -> Result<()> {
        Ok(())
    }
}

impl SemanticNode for Expression<SemanticAnnotations> {
    fn canonize_type_refs(&mut self, stack: &SymbolTableScopeStack) -> Result<()> {
        match self {
            Expression::Path(ref mut ann, ref mut path) => {
                if !path.is_canonical() {
                    stack
                        .lookup_symbol_by_path(path)
                        .and_then(|(_, canonical_path)| {
                            *path = canonical_path;
                            ann.ty = Type::Custom(path.clone());
                            Ok(())
                        })
                } else {
                    Ok(())
                }
            }
            Expression::StructExpression(ref mut ann, ref mut path, _) => {
                if !path.is_canonical() {
                    stack
                        .lookup_symbol_by_path(path)
                        .and_then(|(_, canonical_path)| {
                            //print!("L{}: Before: {}\t", ann.line(), path);
                            *path = canonical_path;
                            //println!("After: {}", path);
                            ann.ty = Type::Custom(path.clone());
                            Ok(())
                        })
                } else {
                    Ok(())
                }
            }
            Expression::RoutineCall(_, _, ref mut path, _) => {
                if !path.is_canonical() {
                    stack
                        .lookup_symbol_by_path(path)
                        .and_then(|(_, canonical_path)| {
                            //println!("Before: {}", path);
                            *path = canonical_path;
                            //println!("ForEach: {}", path);
                            //println!("Stack: \n{}", stack);
                            Ok(())
                        })
                } else {
                    Ok(())
                }
            }
            _ => Ok(()),
        }
        .map_err(|e| format!("L{}: {}", self.annotation().line(), e))
    }
}
impl SemanticNode for Statement<SemanticAnnotations> {}
impl SemanticNode for Bind<SemanticAnnotations> {
    fn canonize_annotation_type(&mut self, stack: &SymbolTableScopeStack) -> Result<()> {
        self.annotation_mut().ty = stack.canonize_local_type_ref(self.get_type())?;
        self.set_type(self.annotation().ty.clone());
        Ok(())
    }
}
impl SemanticNode for Mutate<SemanticAnnotations> {}
impl SemanticNode for Module<SemanticAnnotations> {
    fn canonize_annotation_path(&mut self, stack: &SymbolTableScopeStack) -> Result<()> {
        // If this node has a name, then use the current stack to construct
        // a canonical path from the root of the AST to the current node
        // (this is for routine definitions, modules, and structure definitions)
        match self.name() {
            // Set SemanticAnnotation::canonical_path to CanonicalPath
            // Addresses RoutineDefs and StructDefs (for LLVM IR)
            Some(name) => {
                let cpath = stack.to_canonical(&vec![name].into())?;
                self.annotation_mut().set_canonical_path(cpath);
            }
            None => (),
        }

        // Canonize Symbol Table
        let sym = &mut self.annotation_mut().sym;
        for s in sym.table_mut().iter_mut() {
            let cty = stack.canonize_local_type_ref(&s.ty)?;
            //print!("SYM fn {} -> {} =>", s.name, s.ty);
            s.ty = cty;
            //println!("SYM fn {} -> {}", s.name, s.ty);
        }

        Ok(())
    }
}
impl SemanticNode for StructDef<SemanticAnnotations> {}
impl SemanticNode for RoutineDef<SemanticAnnotations> {
    fn canonize_type_refs(&mut self, stack: &SymbolTableScopeStack) -> Result<()> {
        let ctype = stack.canonize_local_type_ref(&self.ty)?;
        self.ty = ctype;
        self.annotation_mut().ty = self.ty.clone();
        Ok(())
    }
}
impl SemanticNode for Extern<SemanticAnnotations> {
    fn canonize_annotation_path(&mut self, _: &SymbolTableScopeStack) -> Result<()> {
        let name = match self.name() {
            Some(name) => name,
            None => panic!("Externs must have a name"),
        };
        let cpath = vec![name].into();
        self.annotation_mut().set_canonical_path(cpath);
        Ok(())
    }

    fn canonize_annotation_type(&mut self, stack: &SymbolTableScopeStack) -> Result<()> {
        self.annotation_mut().ty = stack.canonize_local_type_ref(self.get_return_type())?;
        Ok(())
    }

    fn canonize_type_refs(&mut self, stack: &SymbolTableScopeStack) -> Result<()> {
        let ctype = stack.canonize_local_type_ref(&self.ty)?;
        self.ty = ctype;
        Ok(())
    }
}
impl SemanticNode for Parameter<SemanticAnnotations> {
    fn canonize_type_refs(&mut self, stack: &SymbolTableScopeStack) -> Result<()> {
        let ctype = stack.canonize_local_type_ref(&self.ty)?;
        self.ty = ctype;
        Ok(())
    }
}
impl SemanticNode for YieldReturn<SemanticAnnotations> {
    fn canonize_annotation_type(&mut self, stack: &SymbolTableScopeStack) -> Result<()> {
        let current_fn = stack
            .get_current_fn()
            .ok_or("YieldReturn must appear within a function")?;
        let (_, ret_ty) = stack.lookup_coroutine(current_fn)?;
        self.annotation_mut().ty = stack.canonize_local_type_ref(ret_ty)?;
        Ok(())
    }
}
impl SemanticNode for Return<SemanticAnnotations> {}

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

    fn transform<F>(&self, a: &mut dyn SemanticNode, mut f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn SemanticNode) -> Result<()> + Copy,
    {
        self.diag.begin(a.annotation());
        let r = f(&self.symbols, a);
        self.diag.end(a.annotation());
        r
    }

    pub fn for_each<F>(&mut self, m: &mut Module<SemanticAnnotations>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn SemanticNode) -> Result<()> + Copy,
    {
        self.diag.start_trace();
        let r = self.for_module(m, f);
        self.diag.end_trace();
        r.map_err(|e| format!("Semantic: {}", e))
    }

    fn for_module<F>(&mut self, m: &mut Module<SemanticAnnotations>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn SemanticNode) -> Result<()> + Copy,
    {
        if self.tracing != TracingConfig::Off {
            println!("{}", self.name);
        }

        self.symbols.enter_scope(&m.annotation().sym);
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

    fn for_items<F>(&mut self, items: &mut Vec<Item<SemanticAnnotations>>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn SemanticNode) -> Result<()> + Copy,
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

    fn for_structdef<F>(&mut self, sd: &mut StructDef<SemanticAnnotations>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn SemanticNode) -> Result<()> + Copy,
    {
        let r = self.transform(sd, f);
        self.for_parameters(&mut sd.get_fields_mut(), f)?;
        r
    }

    fn for_routinedef<F>(&mut self, rd: &mut RoutineDef<SemanticAnnotations>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn SemanticNode) -> Result<()> + Copy,
    {
        self.symbols.enter_scope(&rd.annotation().sym);
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

    fn for_extern<F>(&mut self, ex: &mut Extern<SemanticAnnotations>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn SemanticNode) -> Result<()> + Copy,
    {
        let r = self.transform(ex, f);
        self.for_parameters(&mut ex.params, f)?;
        r
    }

    fn for_parameters<F>(
        &mut self,
        params: &mut Vec<Parameter<SemanticAnnotations>>,
        f: F,
    ) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn SemanticNode) -> Result<()> + Copy,
    {
        for p in params {
            self.transform(p, f)?;
        }
        Ok(())
    }

    fn for_statement<F>(
        &mut self,
        statement: &mut Statement<SemanticAnnotations>,
        f: F,
    ) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn SemanticNode) -> Result<()> + Copy,
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

    fn for_bind<F>(&mut self, bind: &mut Bind<SemanticAnnotations>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn SemanticNode) -> Result<()> + Copy,
    {
        let r = self.transform(bind, f);
        self.for_expression(bind.get_rhs_mut(), f)?;
        r
    }

    fn for_mutate<F>(&mut self, mutate: &mut Mutate<SemanticAnnotations>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn SemanticNode) -> Result<()> + Copy,
    {
        let r = self.transform(mutate, f);
        self.for_expression(mutate.get_rhs_mut(), f)?;
        r
    }

    fn for_yieldreturn<F>(&mut self, yr: &mut YieldReturn<SemanticAnnotations>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn SemanticNode) -> Result<()> + Copy,
    {
        let r = self.transform(yr, f);
        match yr.get_value_mut() {
            Some(rv) => self.for_expression(rv, f)?,
            None => (),
        }
        r
    }

    fn for_return<F>(&mut self, ret: &mut Return<SemanticAnnotations>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn SemanticNode) -> Result<()> + Copy,
    {
        let r = self.transform(ret, f);

        match ret.get_value_mut() {
            Some(rv) => self.for_expression(rv, f)?,
            None => (),
        }

        r
    }

    fn for_expression<F>(&mut self, exp: &mut Expression<SemanticAnnotations>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn SemanticNode) -> Result<()> + Copy,
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
            Expression::ArrayValue(_, el, _) => {
                for e in el {
                    self.for_expression(e, f)?;
                    //println!("L{}: for_exp {}", e.annotation().line(), e);
                }
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
            StructExpression(..) => {
                let r = self.for_struct_expression(exp, f);
                //println!("L{}: for_exp {}", exp.annotation().line(), exp);
                r
            }
        }
    }

    fn for_expression_block<F>(
        &mut self,
        block: &mut Expression<SemanticAnnotations>,
        f: F,
    ) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn SemanticNode) -> Result<()> + Copy,
    {
        self.symbols.enter_scope(&block.annotation().sym);
        let r = self.transform(block, f);
        if let Expression::ExpressionBlock(ref mut _annotation, ref mut body, ref mut final_exp) =
            block
        {
            for e in body.iter_mut() {
                self.for_statement(e, f)?;
            }

            match final_exp {
                Some(fe) => self.for_expression(fe, f)?,
                None => (),
            }
            //final_exp.as_mut().map(|fe| self.for_expression(fe, f));
            self.symbols.leave_scope();
        } else {
            panic!("Expected ExpressionBlock, but got {:?}", block)
        }
        r
    }

    fn for_member_access<F>(
        &mut self,
        access: &mut Expression<SemanticAnnotations>,
        f: F,
    ) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn SemanticNode) -> Result<()> + Copy,
    {
        let r = self.transform(access, f);
        if let Expression::MemberAccess(_, src, _member) = access {
            self.for_expression(src, f)?;
        } else {
            panic!("Expected MemberAccess, but got {:?}", access)
        }
        r
    }

    fn for_unary_op<F>(&mut self, un_op: &mut Expression<SemanticAnnotations>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn SemanticNode) -> Result<()> + Copy,
    {
        let r = self.transform(un_op, f);
        if let Expression::UnaryOp(_, _op, operand) = un_op {
            self.for_expression(operand, f)?;
        } else {
            panic!("Expected UnaryOp, but got {:?}", un_op)
        }
        r
    }

    fn for_binary_op<F>(&mut self, bin_op: &mut Expression<SemanticAnnotations>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn SemanticNode) -> Result<()> + Copy,
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

    fn for_if<F>(&mut self, if_exp: &mut Expression<SemanticAnnotations>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn SemanticNode) -> Result<()> + Copy,
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

    fn for_while<F>(&mut self, while_exp: &mut Expression<SemanticAnnotations>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn SemanticNode) -> Result<()> + Copy,
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

    fn for_routine_call<F>(&mut self, rc: &mut Expression<SemanticAnnotations>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn SemanticNode) -> Result<()> + Copy,
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

    fn for_yield<F>(&mut self, yield_exp: &mut Expression<SemanticAnnotations>, f: F) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn SemanticNode) -> Result<()> + Copy,
    {
        let r = self.transform(yield_exp, f);
        if let Expression::Yield(_, e) = yield_exp {
            self.for_expression(e, f)?;
        } else {
            panic!("Expected Yield, but got {:?}", yield_exp)
        }
        r
    }

    fn for_struct_expression<F>(
        &mut self,
        se: &mut Expression<SemanticAnnotations>,
        f: F,
    ) -> Result<()>
    where
        F: FnMut(&SymbolTableScopeStack, &mut dyn SemanticNode) -> Result<()> + Copy,
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
        t.for_module(&mut sm_ast, |_stack, n| {
            n.annotation_mut().ln *= 4;
            Ok(())
        })
        .unwrap();

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
            n.annotation_mut().set_canonical_path(cpath);
            Ok(())
        })
        .unwrap();

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

    #[test]
    fn canonize_structdef() {
        let text = "mod m{ struct MyStruct {}}";
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
        t.for_module(&mut sm_ast, |stack, n| match n.name() {
            Some(name) => {
                let cpath = stack.to_canonical(&vec![name].into()).unwrap();
                n.annotation_mut().set_canonical_path(cpath);
                Ok(())
            }
            None => Ok(()),
        })
        .unwrap();

        assert_eq!(
            *sm_ast
                .get_module("m")
                .unwrap()
                .get_item("MyStruct")
                .unwrap()
                .annotation()
                .get_canonical_path(),
            vec!["project", "test", "m", "MyStruct"].into()
        );
    }
}
