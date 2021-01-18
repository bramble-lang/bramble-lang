use std::collections::HashMap;

use module::Module;

use crate::{compiler::ast::ast::CompilerNode, syntax::{module::{self, Item}, routinedef::RoutineDef, statement::{Bind, Mutate, Printbln, Printi, Printiln, Prints, Statement}}};

use super::scope::Scope;

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
    pub fn extract_from(&mut self, ast: &CompilerNode) {
        use crate::ast::Ast::*;

        match ast {
            ExpressionBlock(_, body) => {
                for e in body.iter() {
                    self.extract_from(e);
                }
            }
            Integer(..) => {}
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
            Printi(_, ref e) => {
                self.extract_from(e);
            }
            Printiln(_, ref e) => {
                self.extract_from(e);
            }
            Prints(_, ref e) => {
                self.extract_from(e);
            }
            Printbln(_, ref e) => {
                self.extract_from(e);
            }
            If(_, ref cond, ref tb, ref fb) => {
                self.extract_from(cond);
                self.extract_from(tb);
                self.extract_from(fb);
            }
            Yield(_, e) => {
                self.extract_from(e);
            }
            Return(_, None) => {}
            Return(_, Some(e)) => {
                self.extract_from(e);
            }
            YieldReturn(_, None) => {}
            YieldReturn(_, Some(e)) => {
                self.extract_from(e);
            }
            Statement(s) => self.extract_from_statement(s),
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

    pub fn extract_from_module(&mut self, module: &Module<Scope>) {
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

    pub fn extract_from_item(&mut self, item: &Item<Scope>) {
        match item {
            Item::Routine(r) => self.extract_from_routine(r),
            Item::Struct(_) => (),
        }
    }

    pub fn extract_from_routine(&mut self, routine: &RoutineDef<Scope>) {
        for s in routine.get_body().iter() {
            self.extract_from(s);
        }
    }

    pub fn extract_from_statement(&mut self, statement: &Statement<Scope>) {
        match statement {
            Statement::Bind(b) => self.extract_from_bind(b),
            Statement::Mutate(m) => self.extract_from_mutate(m),
            Statement::Return(r) => self.extract_from(r),
            Statement::Yield(y) => self.extract_from(y),
            Statement::YieldReturn(ast) => self.extract_from(ast),
            Statement::Printi(pi) => self.extract_from_printi(pi),
            Statement::Printiln(ast) => self.extract_from_printiln(ast),
            Statement::Printbln(ast) => self.extract_from_printbln(ast),
            Statement::Prints(ast) => self.extract_from_prints(ast),
            Statement::Expression(ast) => self.extract_from(ast),
        }
        /*if let CompilerNode::Statement(.., e) = statement {
            self.extract_from(e)
        } else {
            panic!("Expected a statement, but got {}", statement.root_str())
        }*/
    }

    pub fn extract_from_bind(&mut self, bind: &Bind<Scope>) {
        self.extract_from(bind.get_rhs())
    }

    pub fn extract_from_mutate(&mut self, mutate: &Mutate<Scope>) {
        self.extract_from(mutate.get_rhs())
    }

    pub fn extract_from_printi(&mut self, p: &Printi<Scope>) {
        self.extract_from(p.get_value())
    }

    pub fn extract_from_printiln(&mut self, p: &Printiln<Scope>) {
        self.extract_from(p.get_value())
    }

    pub fn extract_from_printbln(&mut self, p: &Printbln<Scope>) {
        self.extract_from(p.get_value())
    }

    pub fn extract_from_prints(&mut self, p: &Prints<Scope>) {
        self.extract_from(p.get_value())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::lexer::Lexer;
    use crate::lexer::tokens::Token;
    use crate::parser::parser;
    use crate::{diagnostics::config::TracingConfig, type_check};

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
            let module = type_check(&ast, TracingConfig::Off, TracingConfig::Off).unwrap();
            let (compiler_ast, ..) = CompilerNode::from(&module).unwrap();
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
