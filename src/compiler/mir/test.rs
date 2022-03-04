#[cfg(test)]
pub mod tests {
    use crate::{
        compiler::{
            ast::{Expression, Module, PointerMut, Type, MAIN_MODULE},
            diagnostics::Logger,
            lexer::{tokens::Token, LexerError},
            mir::{
                ir::{BasicBlockId, Constant, LValue, Operand, RValue, StatementKind},
                transform,
            },
            parser::Parser,
            semantics::semanticnode::SemanticContext,
            CompilerError, Lexer, SourceMap,
        },
        resolve_types, StringTable,
    };

    type LResult = std::result::Result<Vec<Token>, CompilerError<LexerError>>;

    fn compile(input: &str, table: &mut StringTable) -> Module<SemanticContext> {
        let mut sm = SourceMap::new();
        sm.add_string(input, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let main = table.insert("main".into());
        let main_mod = table.insert(MAIN_MODULE.into());
        let main_fn = table.insert("my_main".into());

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, table, &logger)
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
        let mut table = StringTable::new();
        let module = compile(text, &mut table);
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
        let mut table = StringTable::new();
        let module = compile(text, &mut table);
        let mirs = transform::module_transform(&module);
        for mir in mirs {
            println!("{}", mir);
        }
    }

    #[test]
    fn literals() {
        let mut table = StringTable::new();
        let hello = table.insert("hello".into());
        for (ty, v, exp) in &[
            (Type::I8, Expression::I8((), 1), Constant::I8(1)),
            (Type::I16, Expression::I16((), 1), Constant::I16(1)),
            (Type::I32, Expression::I32((), 1), Constant::I32(1)),
            (Type::I64, Expression::I64((), 1), Constant::I64(1)),
            (Type::U8, Expression::U8((), 1), Constant::U8(1)),
            (Type::U16, Expression::U16((), 1), Constant::U16(1)),
            (Type::U32, Expression::U32((), 1), Constant::U32(1)),
            (Type::U64, Expression::U64((), 1), Constant::U64(1)),
            (Type::F64, Expression::F64((), 5.0), Constant::F64(5.0)),
            (Type::StringLiteral, Expression::StringLiteral((), hello), Constant::StringLiteral(hello)),
            (
                Type::RawPointer(PointerMut::Const, Box::new(Type::I16)),
                Expression::Null(()),
                Constant::Null,
            ),
            (
                Type::Bool,
                Expression::Boolean((), true),
                Constant::Bool(true),
            ),
        ] {
            let text = format!(
                "
                    fn test() {{ 
                        let x: {} := {};
                        return;
                    }}
                    ",
                ty,
                to_code(v, &table),
            );
            let module = compile(&text, &mut table);
            let mirs = transform::module_transform(&module);
            assert_eq!(1, mirs.len());
            let bb = mirs[0].get_bb(BasicBlockId::new(0));
            let stm = bb.get_stm(0);
            match stm.kind() {
                StatementKind::Assign(_, r) => {
                    assert_eq!(*r, RValue::Use(Operand::Constant(exp.clone())));
                }
            }
        }
    }

    fn to_code(e: &Expression<()>, table: &StringTable) -> String {
        match e {
            Expression::StringLiteral(_, sid) => format!("\"{}\"", table.get(*sid).unwrap()),
            _ => e.root_str(),
        }
    }
}
