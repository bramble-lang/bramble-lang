use crate::ast::Expression;
use std::collections::HashMap;

use crate::ast::*;

#[derive(Debug, PartialEq)]
pub struct StringPool {
    pub pool: HashMap<String, usize>,
}

impl StringPool {
    pub fn new() -> StringPool {
        StringPool {
            pool: HashMap::new(),
        }
    }

    pub fn iter(&self) -> std::collections::hash_map::Iter<'_, String, usize> {
        self.pool.iter()
    }

    /// If the given string is not in the string pool, this will insert the
    /// string and generate a unique ID for it.
    pub fn insert(&mut self, s: &str) {
        if self.pool.contains_key(s.into()) {
            return;
        }
        let id = self.pool.len();
        self.pool.insert(s.into(), id);
    }

    /// Returns the unique ID for a given string if the string is in the
    /// pool, otherwise it will return None.
    pub fn get(&self, s: &str) -> Option<&usize> {
        self.pool.get(s)
    }

    /// Traverse through all the nodes in an AST and find any occurances of
    /// String Literals and will add them to the string pool.
    pub fn extract_from<A>(&mut self, ast: &Expression<A>) {
        use crate::ast::Expression::*;

        match ast {
            ExpressionBlock(_, body, final_exp) => {
                for e in body.iter() {
                    self.extract_from_statement(e);
                }
                match final_exp {
                    None => (),
                    Some(fe) => self.extract_from(fe),
                }
            }
            Integer32(..) => {}
            Integer64(..) => {}
            Boolean(..) => {}
            StringLiteral(_, s) => {
                self.insert(s);
            }
            CustomType(..) => {}
            Identifier(..) => {}
            IdentifierDeclare(..) => {}
            MemberAccess(_, src, _) => {
                self.extract_from(src);
            }
            Path(..) => {}
            UnaryOp(_, _, ref operand) => {
                self.extract_from(operand);
            }
            BinaryOp(_, _, ref l, ref r) => {
                self.extract_from(l);
                self.extract_from(r);
            }
            If {
                cond,
                if_arm,
                else_arm,
                ..
            } => {
                self.extract_from(cond);
                self.extract_from(if_arm);
                else_arm.as_ref().map(|ea| self.extract_from(&ea));
            }
            Yield(_, e) => {
                self.extract_from(e);
            }
            RoutineCall(.., params) => {
                for e in params.iter() {
                    self.extract_from(e);
                }
            }
            StructExpression(_, _, fields) => {
                for (_, f) in fields.iter() {
                    self.extract_from(f);
                }
            }
        }
    }

    pub fn extract_from_module<A>(&mut self, module: &Module<A>) {
        for m in module.get_modules().iter() {
            self.extract_from_module(m);
        }

        for f in module.get_functions().iter() {
            self.extract_from_item(f);
        }
        for c in module.get_coroutines().iter() {
            self.extract_from_item(c);
        }
    }

    pub fn extract_from_item<A>(&mut self, item: &Item<A>) {
        match item {
            Item::Routine(r) => self.extract_from_routine(r),
            Item::Struct(_) => (),
        }
    }

    pub fn extract_from_routine<A>(&mut self, routine: &RoutineDef<A>) {
        for s in routine.get_body().iter() {
            self.extract_from_statement(s);
        }
    }

    pub fn extract_from_statement<A>(&mut self, statement: &Statement<A>) {
        match statement {
            Statement::Bind(b) => self.extract_from_bind(b),
            Statement::Mutate(m) => self.extract_from_mutate(m),
            Statement::Return(r) => self.extract_from_return(r),
            Statement::YieldReturn(ast) => self.extract_from_yieldreturn(ast),
            Statement::Expression(ast) => self.extract_from(ast),
        }
    }

    pub fn extract_from_bind<A>(&mut self, bind: &Bind<A>) {
        self.extract_from(bind.get_rhs())
    }

    pub fn extract_from_mutate<A>(&mut self, mutate: &Mutate<A>) {
        self.extract_from(mutate.get_rhs())
    }

    pub fn extract_from_yieldreturn<A>(&mut self, yr: &YieldReturn<A>) {
        match yr.get_value() {
            None => (),
            Some(val) => self.extract_from(val),
        }
    }

    pub fn extract_from_return<A>(&mut self, r: &Return<A>) {
        match r.get_value() {
            None => (),
            Some(val) => self.extract_from(val),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::lexer::Lexer;
    use crate::lexer::tokens::Token;
    use crate::parser::parser;
    use crate::{diagnostics::config::TracingConfig, resolve_types};

    use super::super::layout::compute_layout_for_program;
    use super::*;

    #[test]
    fn insert_string() {
        let mut sp = StringPool::new();
        sp.insert("hello, world");

        assert!(sp.get("hello, world").is_some());
        assert!(sp.get("not there").is_none());
    }

    #[test]
    fn insert_duplicate() {
        let mut sp = StringPool::new();
        sp.insert("test");
        let first_id = *sp.get("test").unwrap();
        sp.insert("test");
        let second_id = *sp.get("test").unwrap();

        assert_eq!(first_id, second_id);
    }

    #[test]
    fn extract_from_ast() {
        for (text, expected) in vec![
            ("fn test() -> string {return \"test\";}", vec!["test"]),
            ("fn test() -> string {return \"test2\";}", vec!["test2"]),
            (
                "fn test() -> string {let s: string := \"hello\"; return \"test2\";}",
                vec!["hello", "test2"],
            ),
            (
                "mod my_mod{fn test() -> string {let s: string := \"hello\"; return \"test2\";}}",
                vec!["hello", "test2"],
            ),
            (
                "mod my_mod{ mod inner{fn test() -> string {let s: string := \"hello\"; return \"test2\";}}}",
                vec!["hello", "test2"],
            ),
            (
                "fn test() -> string {
                    let mut s: string := \"hello\"; 
                    mut s := \"world\";
                    return \"test2\";
                }",
                vec!["hello", "world", "test2"],
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_, _>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off, TracingConfig::Off).unwrap();
            let (compiler_ast, ..) = compute_layout_for_program(&module).unwrap();
            let mut sp = StringPool::new();
            sp.extract_from_module(&compiler_ast);

            assert!(cmp(&sp, &expected));
        }
    }

    fn cmp(sp: &StringPool, expected: &Vec<&str>) -> bool {
        if sp.pool.len() != expected.len() {
            return false;
        }

        for e in expected.iter() {
            if sp.get(e).is_none() {
                return false;
            }
        }

        true
    }
}
