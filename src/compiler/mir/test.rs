#[cfg(test)]
pub mod tests {
    use crate::{
        compiler::{
            ast::MAIN_MODULE,
            diagnostics::Logger,
            lexer::{tokens::Token, LexerError},
            parser::Parser,
            CompilerError, Lexer, SourceMap, mir::transform,
        },
        resolve_types, StringTable,
    };

    type LResult = std::result::Result<Vec<Token>, CompilerError<LexerError>>;

    #[test]
    fn basic() {
        let text = "
        fn test() -> i64 {
            let x: i64 := 5;
            let b: bool := true;
            if (b) {13} else {29};
            return 1 + 2 + 3 + x;
        }
        ";

        let mut sm = SourceMap::new();
        sm.add_string(&text, "/test".into()).unwrap();
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
        let module = resolve_types(&ast, main_mod, main_fn, &logger).unwrap();
        transform::module_transform(&module);
    }
}
