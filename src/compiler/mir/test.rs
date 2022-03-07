#[cfg(test)]
pub mod tests {
    use crate::{
        compiler::{
            ast::*,
            diagnostics::Logger,
            lexer::{tokens::Token, LexerError},
            mir::{ir::*, transform},
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
    fn while_expr() {
        let text = "
        fn test() -> i64 {
            let x: i64 := 5;
            let b: bool := true;
            while (b && false) {
                let y: i64 := 6 + x;
            };
            return 0;
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
    fn address_of() {
        let mut table = StringTable::new();
        let hello = table.insert("hello".into());
        for (op, ptr_mut) in [
            (UnaryOperator::AddressConst, PointerMut::Const),
            (UnaryOperator::AddressMut, PointerMut::Mut),
        ] {
            for (literal_ty, v) in &[
                (Type::I8, Expression::I8((), 1)),
                (Type::I16, Expression::I16((), 1)),
                (Type::I32, Expression::I32((), 1)),
                (Type::I64, Expression::I64((), 1)),
                (Type::U8, Expression::U8((), 1)),
                (Type::U16, Expression::U16((), 1)),
                (Type::U32, Expression::U32((), 1)),
                (Type::U64, Expression::U64((), 1)),
                (Type::F64, Expression::F64((), 5.0)),
                (Type::StringLiteral, Expression::StringLiteral((), hello)),
                (Type::Bool, Expression::Boolean((), true)),
            ] {
                let literal = to_code(v, &table);
                let ptr_ty = Type::RawPointer(ptr_mut, Box::new(literal_ty.clone()));
                let text = format!(
                    "
                    fn test() {{ 
                        let mut x: {literal_ty} := {literal};
                        let p: {ptr_ty} := {op} x;
                        return;
                    }}
                    ",
                );
                let module = compile(&text, &mut table);
                let mirs = transform::module_transform(&module);
                assert_eq!(1, mirs.len());

                let bb = mirs[0].get_bb(BasicBlockId::new(0));
                let stm = bb.get_stm(1);
                match stm.kind() {
                    StatementKind::Assign(_, r) => {
                        assert_eq!(*r, RValue::AddressOf(LValue::Var(VarId::new(0))));
                    }
                }
            }
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
            (
                Type::StringLiteral,
                Expression::StringLiteral((), hello),
                Constant::StringLiteral(hello),
            ),
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

    #[test]
    fn numerical_binary_ops() {
        let mut table = StringTable::new();
        for (op, exp_op, ty_override) in [
            (BinaryOperator::Add, BinOp::Add, None),
            (BinaryOperator::Sub, BinOp::Sub, None),
            (BinaryOperator::Mul, BinOp::Mul, None),
            (BinaryOperator::Div, BinOp::Div, None),
            (BinaryOperator::Eq, BinOp::Eq, Some(Type::Bool)),
            (BinaryOperator::NEq, BinOp::Ne, Some(Type::Bool)),
            (BinaryOperator::Ls, BinOp::Lt, Some(Type::Bool)),
            (BinaryOperator::LsEq, BinOp::Le, Some(Type::Bool)),
            (BinaryOperator::Gr, BinOp::Gt, Some(Type::Bool)),
            (BinaryOperator::GrEq, BinOp::Ge, Some(Type::Bool)),
        ] {
            for (literal_ty, v, exp) in &[
                (Type::I8, Expression::I8((), 1), Constant::I8(1)),
                (Type::I16, Expression::I16((), 1), Constant::I16(1)),
                (Type::I32, Expression::I32((), 1), Constant::I32(1)),
                (Type::I64, Expression::I64((), 1), Constant::I64(1)),
                (Type::U8, Expression::U8((), 1), Constant::U8(1)),
                (Type::U16, Expression::U16((), 1), Constant::U16(1)),
                (Type::U32, Expression::U32((), 1), Constant::U32(1)),
                (Type::U64, Expression::U64((), 1), Constant::U64(1)),
                (Type::F64, Expression::F64((), 5.0), Constant::F64(5.0)),
            ] {
                let literal = to_code(v, &table);
                let ty = ty_override.as_ref().unwrap_or(literal_ty);
                let text = format!(
                    "
                    fn test() {{ 
                        let x: {ty} := {literal} {op} {literal};
                        return;
                    }}
                    ",
                );
                let module = compile(&text, &mut table);
                let mirs = transform::module_transform(&module);
                assert_eq!(1, mirs.len());

                let bb = mirs[0].get_bb(BasicBlockId::new(0));
                let stm = bb.get_stm(0);
                match stm.kind() {
                    StatementKind::Assign(_, r) => {
                        assert_eq!(
                            *r,
                            RValue::BinOp(
                                exp_op,
                                Operand::Constant(exp.clone()),
                                Operand::Constant(exp.clone())
                            )
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn negate() {
        let mut table = StringTable::new();
        // this can only be used on signed types
        for (literal_ty, v, exp) in &[
            (Type::I8, Expression::I8((), 1), Constant::I8(1)),
            (Type::I16, Expression::I16((), 1), Constant::I16(1)),
            (Type::I32, Expression::I32((), 1), Constant::I32(1)),
            (Type::I64, Expression::I64((), 1), Constant::I64(1)),
            (Type::F64, Expression::F64((), 5.0), Constant::F64(5.0)),
        ] {
            let literal = to_code(v, &table);
            let text = format!(
                "
                    fn test() {{ 
                        let x: {literal_ty} := -{literal};
                        return;
                    }}
                    ",
            );
            let module = compile(&text, &mut table);
            let mirs = transform::module_transform(&module);
            assert_eq!(1, mirs.len());

            let bb = mirs[0].get_bb(BasicBlockId::new(0));
            let stm = bb.get_stm(0);
            match stm.kind() {
                StatementKind::Assign(_, r) => {
                    assert_eq!(
                        *r,
                        RValue::UnOp(UnOp::Negate, Operand::Constant(exp.clone()))
                    );
                }
            }
        }
    }

    #[test]
    fn boolean_binary_ops() {
        let mut table = StringTable::new();
        for (op, exp_op) in [
            (BinaryOperator::BAnd, BinOp::And),
            (BinaryOperator::BOr, BinOp::Or),
        ] {
            for (literal_ty, v, exp) in &[
                (
                    Type::Bool,
                    Expression::Boolean((), true),
                    Constant::Bool(true),
                ),
                (
                    Type::Bool,
                    Expression::Boolean((), false),
                    Constant::Bool(false),
                ),
            ] {
                let literal = to_code(v, &table);
                let text = format!(
                    "
                    fn test() {{ 
                        let x: {literal_ty} := {literal} {op} {literal};
                        return;
                    }}
                    ",
                );
                let module = compile(&text, &mut table);
                let mirs = transform::module_transform(&module);
                assert_eq!(1, mirs.len());

                let bb = mirs[0].get_bb(BasicBlockId::new(0));
                let stm = bb.get_stm(0);
                match stm.kind() {
                    StatementKind::Assign(_, r) => {
                        assert_eq!(
                            *r,
                            RValue::BinOp(
                                exp_op,
                                Operand::Constant(exp.clone()),
                                Operand::Constant(exp.clone())
                            )
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn not() {
        let mut table = StringTable::new();
        for (literal_ty, v, exp) in &[(
            Type::Bool,
            Expression::Boolean((), true),
            Constant::Bool(true),
        )] {
            let literal = to_code(v, &table);
            let text = format!(
                "
                    fn test() {{ 
                        let x: {literal_ty} := !{literal};
                        return;
                    }}
                    ",
            );
            let module = compile(&text, &mut table);
            let mirs = transform::module_transform(&module);
            assert_eq!(1, mirs.len());

            let bb = mirs[0].get_bb(BasicBlockId::new(0));
            let stm = bb.get_stm(0);
            match stm.kind() {
                StatementKind::Assign(_, r) => {
                    assert_eq!(*r, RValue::UnOp(UnOp::Not, Operand::Constant(exp.clone())));
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
