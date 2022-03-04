#[cfg(test)]
pub mod tests {
    use crate::{
        compiler::{
            ast::{MAIN_MODULE, Module},
            diagnostics::Logger,
            lexer::{tokens::Token, LexerError},
            parser::Parser,
            CompilerError, Lexer, SourceMap, mir::transform, semantics::semanticnode::SemanticContext,
        },
        resolve_types, StringTable, diagnostics::{ConsoleWriter, JsonWriter},
    };

    type LResult = std::result::Result<Vec<Token>, CompilerError<LexerError>>;

    fn compile(input: &str, logger: &Logger) -> (SourceMap, StringTable, Module<SemanticContext>) {
        let mut sm = SourceMap::new();
        sm.add_string(input, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let mut table = StringTable::new();

        let main = table.insert("main".into());
        let main_mod = table.insert(MAIN_MODULE.into());
        let main_fn = table.insert("my_main".into());

        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();

        let parser = Parser::new(&logger);
        let ast = parser.parse(main, &tokens).unwrap().unwrap();
        (sm, table, resolve_types(&ast, main_mod, main_fn, &logger).unwrap())
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
        let mut logger = Logger::new();
        let (sm, st, module) = compile(text, &logger);
        let cw = ConsoleWriter::new(&sm, &st);
        logger.add_writer(&cw);

        let file = std::fs::File::create("./target/test.json").unwrap();
        let jw = JsonWriter::new(file, &sm, &st);
        logger.add_writer(&jw);

        let mirs = transform::module_transform(&module, &logger);
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
        let mut logger = Logger::new();
        let (sm, st, module) = compile(text, &logger);

        let cw = ConsoleWriter::new(&sm, &st);
        logger.add_writer(&cw);

        let file = std::fs::File::create("./target/test.json").unwrap();
        let jw = JsonWriter::new(file, &sm, &st);
        logger.add_writer(&jw);

        logger.enable();
        let mirs = transform::module_transform(&module, &mut logger);
        for mir in mirs {
            println!("{}", mir);
        }
    }
}
