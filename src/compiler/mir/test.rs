#[cfg(test)]
pub mod tests {
    use crate::{
        compiler::{
            ast::{MAIN_MODULE, Module, Expression, Type},
            diagnostics::Logger,
            lexer::{tokens::Token, LexerError},
            parser::Parser,
            CompilerError, Lexer, SourceMap, mir::{transform, ir::{BasicBlockId, StatementKind, LValue, RValue, Operand, Constant}}, semantics::semanticnode::SemanticContext,
        },
        resolve_types, StringTable,
    };

    type LResult = std::result::Result<Vec<Token>, CompilerError<LexerError>>;

    fn compile(input: &str) -> Module<SemanticContext> {
        let mut sm = SourceMap::new();
        sm.add_string(input, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let mut table = StringTable::new();
        let main = table.insert("main".into());
        let main_mod = table.insert(MAIN_MODULE.into());
        let main_fn = table.insert("my_main".into());

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();

        let parser = Parser::new(&logger);
        let ast = parser.parse(main, &tokens).unwrap().unwrap();
        resolve_types(&ast, main_mod, main_fn, &logger).unwrap()
    }

    #[test]
    fn basic() {
        let text = "
        fn test() -> i64 {
            let x: i64 := 5;
            let mut b: bool := true;
            let y: i64 := if (b) {13} else {29};
            return 1 + 2 + 3 + x + y;
        }
        ";
        let module = compile(text);
        let mirs = transform::module_transform(&module);
        for mir in mirs {
            println!("{}", mir);
        }
    }

    #[test]
    fn if_no_else() {
        let text = "
        fn test() -> i64 {
            let x: i64 := 5;
            let b: bool := true;
            if (b) {};
            return 1 + 2 + 3 + x;
        }
        ";
        let module = compile(text);
        let mirs = transform::module_transform(&module);
        for mir in mirs {
            println!("{}", mir);
        }
    }

    #[test]
    fn constants() {
        let test = (Type::I64, 1, Constant::I64(1));
        let text = format!("
        fn test() -> {} {{ 
            return {};
        }}
        ", test.0, test.1);
        let exp = test.2;
        let module = compile(&text);
        let mirs = transform::module_transform(&module);
        assert_eq!(1, mirs.len());
        let bb = mirs[0].get_bb(BasicBlockId::new(0));
        let stm = bb.get_stm(0);
        match stm.kind() {
            StatementKind::Assign(l, r) => {
                assert_eq!(*l, LValue::ReturnPointer);
                assert_eq!(*r, RValue::Use(Operand::Constant(exp)));
            },
        }
    }
}
