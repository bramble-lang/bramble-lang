use std::collections::HashMap;

use crate::StringTable;

use super::ast::*;

/// Stores the statically defined strings that occur within a Bramble compilation unit
/// These will then be encoded into the data section of the generated binary for
/// quick access at run time.
#[derive(Debug)]
pub struct StringPool<'st> {
    pub pool: HashMap<String, usize>,
    string_table: &'st StringTable,
}

impl<'a> StringPool<'a> {
    pub fn new(string_table: &'a StringTable) -> StringPool<'a> {
        StringPool {
            pool: HashMap::new(),
            string_table,
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
        use Expression::*;

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
            Null(_) => {}
            U8(..) => {}
            U16(..) => {}
            U32(..) => {}
            U64(..) => {}
            I8(..) => {}
            I16(..) => {}
            I32(..) => {}
            I64(..) => {}
            F64(..) => {}
            Boolean(..) => {}
            StringLiteral(_, s) => {
                let val = self.string_table.get(*s).unwrap();
                self.insert(&val);
            }
            ArrayExpression(_, elements, _) => {
                for e in elements {
                    self.extract_from(e);
                }
            }
            ArrayAt { array, index, .. } => {
                self.extract_from(array);
                self.extract_from(index);
            }
            SizeOf(..) => {}
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
            While { cond, body, .. } => {
                self.extract_from(cond);
                self.extract_from(body);
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

    pub fn extract_from_module<A: Context>(&mut self, module: &Module<A>) {
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
            Item::Extern(_) => (),
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
    use super::super::super::lexer::tokens::Token;
    use super::super::super::semantics::type_resolver::resolve_types;
    use crate::compiler::diagnostics::Logger;
    use crate::compiler::parser::Parser;
    use crate::compiler::{Lexer, SourceMap};

    use super::*;

    #[test]
    fn insert_string() {
        let table = StringTable::new();
        let mut sp = StringPool::new(&table);
        sp.insert("hello, world");

        assert!(sp.get("hello, world").is_some());
        assert!(sp.get("not there").is_none());
    }

    #[test]
    fn insert_duplicate() {
        let table = StringTable::new();
        let mut sp = StringPool::new(&table);
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
            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();

            let mut table = StringTable::new();
            let main_mod = table.insert(MAIN_MODULE.into());
            let main_fn = table.insert("my_main".into());
            let test_mod = table.insert("test_mod".into());

            let src = sm.get(0).unwrap().read().unwrap();
            let logger = Logger::new();
            let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger).unwrap()
                .tokenize()
                .into_iter()
                .collect::<Result<_, _>>()
                .unwrap();

            let parser = Parser::new(&logger);
            let ast = parser.parse(test_mod, &tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                main_mod,
                main_fn,
                &logger,
            ).unwrap();
            let mut sp = StringPool::new(&table);
            sp.extract_from_module(&module);

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
