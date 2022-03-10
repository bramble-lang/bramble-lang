#[cfg(test)]
pub mod tests {
    use crate::{
        compiler::{
            ast::*,
            diagnostics::Logger,
            lexer::{tokens::Token, LexerError},
            mir::{
                ir::*,
                transform, builder::MirProject,
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

    /// These tests will take small Bramble programs and print their
    /// MIR representation to the screen. These "tests" do not do any
    /// validation. They are to be used for visual validation of a MIR
    /// and to aid in writing the actual unit tests.
    mod print {

        use super::*;

        #[test]
        fn print_mir() {
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
            let mut project = MirProject::new();
            let mirs = transform::module_transform(&module, &mut project);
            for mir in mirs {
                println!("{}", mir);
            }
        }

        #[test]
        fn print_mir_for_if_no_else() {
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
            let mut project = MirProject::new();
            let mirs = transform::module_transform(&module, &mut project);
            for mir in mirs {
                println!("{}", mir);
            }
        }

        #[test]
        fn print_mir_for_while_expr() {
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
            let mut project = MirProject::new();
            let mirs = transform::module_transform(&module, &mut project);
            for mir in mirs {
                println!("{}", mir);
            }
        }

        #[test]
        fn print_mir_for_array_expression() {
            let text = "
        fn test() -> i64 {
            let x: [[i64; 2]; 2] := [[1, 2], [3, 4]];
            return x[1][0];
        }
        ";
            let mut table = StringTable::new();
            let module = compile(text, &mut table);
            let mut project = MirProject::new();
            let mirs = transform::module_transform(&module, &mut project);
            for mir in mirs {
                println!("{}", mir);
            }
        }

        #[test]
        fn print_mir_for_struct_field() {
            let text = "
        fn test(s: S2) -> i64 {
            return s.s.a + s.s.b;
        }

        struct S {
            a: i64,
            b: i64,
        }

        struct S2 {
            s: S,
        }
        ";
            let mut table = StringTable::new();
            let module = compile(text, &mut table);
            let mut project = MirProject::new();
            let mirs = transform::module_transform(&module, &mut project);
            for mir in mirs {
                println!("{}", mir);
            }
        }
    }

    #[test]
    fn array_expression() {
        let text = "
        fn test() -> i64 {
            let x: [i64; 2] := [1, 2];
            return x[0];
        }
        ";
        let mut table = StringTable::new();
        let module = compile(text, &mut table);
        let mut project = MirProject::new();
        let mir = &transform::module_transform(&module, &mut project)[0];

        let bb = mir.get_bb(BasicBlockId::new(0));
        let first = bb.get_stm(0);
        let second = bb.get_stm(1);

        match first.kind() {
            StatementKind::Assign(lv, rv) => {
                assert_eq!(
                    lv,
                    &LValue::Access(
                        Box::new(LValue::Temp(TempId::new(0))),
                        Accessor::Index(Box::new(Operand::Constant(Constant::I64(0))))
                    )
                );
                assert_eq!(rv, &RValue::Use(Operand::Constant(Constant::I64(1))));
            }
        }

        match second.kind() {
            StatementKind::Assign(lv, rv) => {
                assert_eq!(
                    lv,
                    &LValue::Access(
                        Box::new(LValue::Temp(TempId::new(0))),
                        Accessor::Index(Box::new(Operand::Constant(Constant::I64(1))))
                    )
                );
                assert_eq!(rv, &RValue::Use(Operand::Constant(Constant::I64(2))));
            }
        }
    }

    #[test]
    fn array_access() {
        let text = "
        fn test() -> i64 {
            let x: [i64; 2] := [1, 2];
            return x[0];
        }
        ";
        let mut table = StringTable::new();
        let module = compile(text, &mut table);
        let mut project = MirProject::new();
        let mir = &transform::module_transform(&module, &mut project)[0];

        let bb = mir.get_bb(BasicBlockId::new(0));
        let last = bb.get_stm(bb.len() - 1);

        match last.kind() {
            StatementKind::Assign(lv, rv) => {
                assert_eq!(lv, &LValue::ReturnPointer);
                assert_eq!(
                    rv,
                    &RValue::Use(Operand::LValue(LValue::Access(
                        Box::new(LValue::Var(VarId::new(0))),
                        Accessor::Index(Box::new(Operand::Constant(Constant::I64(0))))
                    )))
                );
            }
        }
    }

    #[test]
    fn double_array_access() {
        let text = "
        fn test() -> i64 {
            let x: [[i64; 2]; 2] := [[1, 2], [3, 4]];
            return x[0][1];
        }
        ";
        let mut table = StringTable::new();
        let module = compile(text, &mut table);
        let mut project = MirProject::new();
        let mir = &transform::module_transform(&module, &mut project)[0];

        let bb = mir.get_bb(BasicBlockId::new(0));
        let last = bb.get_stm(bb.len() - 1);

        match last.kind() {
            StatementKind::Assign(lv, rv) => {
                assert_eq!(lv, &LValue::ReturnPointer);
                assert_eq!(
                    rv,
                    &RValue::Use(Operand::LValue(LValue::Access(
                        Box::new(LValue::Access(
                            Box::new(LValue::Var(VarId::new(0))),
                            Accessor::Index(Box::new(Operand::Constant(Constant::I64(0))))
                        )),
                        Accessor::Index(Box::new(Operand::Constant(Constant::I64(1))))
                    )))
                );
            }
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
                let mut project = MirProject::new();
                let mirs = transform::module_transform(&module, &mut project);
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
            let mut project = MirProject::new();
            let mirs = transform::module_transform(&module, &mut project);
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
                let mut project = MirProject::new();
                let mirs = transform::module_transform(&module, &mut project);
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
            let mut project = MirProject::new();
            let mirs = transform::module_transform(&module, &mut project);
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
                let mut project = MirProject::new();
                let mirs = transform::module_transform(&module, &mut project);
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
            let mut project = MirProject::new();
            let mirs = transform::module_transform(&module, &mut project);
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
        let mut project = MirProject::new();
        let mirs = transform::module_transform(&module, &mut project);
        let mir = &mirs[0];

        // Check that the right number of BBs are created
        assert_eq!(mir.len(), 4);

        // Check first transition into cond BB
        assert_eq!(
            mir.get_bb(BasicBlockId::new(0)).get_term().unwrap().kind(),
            &TerminatorKind::GoTo {
                target: BasicBlockId::new(1)
            }
        );

        // Check that cond BB has a cond goto into the body bb or the exit bb
        if let TerminatorKind::CondGoTo { cond: _, tru, fls } =
            mir.get_bb(BasicBlockId::new(1)).get_term().unwrap().kind()
        {
            assert_eq!(*tru, BasicBlockId::new(2));
            assert_eq!(*fls, BasicBlockId::new(3));
        } else {
            panic!("Expected a conditional go to")
        }

        // Check that the body BB always goes to the cond bb
        if let TerminatorKind::GoTo { target } =
            mir.get_bb(BasicBlockId::new(2)).get_term().unwrap().kind()
        {
            assert_eq!(*target, BasicBlockId::new(1));
        } else {
            panic!("While body should always return to conditional BB")
        }
    }

    #[test]
    fn member_access() {
        let text = "
        fn test(s: S) -> i64 {
            return s.a + s.b;
        }

        struct S {
            a: i64,
            b: i64,
        }
        ";
        let mut table = StringTable::new();
        let module = compile(text, &mut table);

        let mut project = MirProject::new();
        let mirs = transform::module_transform(&module, &mut project);
        assert_eq!(mirs.len(), 1);

        let mir = &mirs[0];
        assert_eq!(mir.len(), 1);
        let bb = mir.get_bb(BasicBlockId::new(0));

        let stm = bb.get_stm(0);
        match stm.kind() {
            StatementKind::Assign(
                _,
                RValue::BinOp(
                    BinOp::Add,
                    Operand::LValue(LValue::Access(lv, Accessor::Field(lfid, lfty))),
                    Operand::LValue(LValue::Access(rv, Accessor::Field(rfid, rfty))),
                ),
            ) => {
                assert_eq!(lv, rv);

                let expected_ty = project.find_type(&Type::I64).unwrap();
                assert_eq!(*lfty, expected_ty);
                assert_eq!(*rfty, expected_ty);

                assert_eq!(u32::from(*lfid), 0u32);
                assert_eq!(u32::from(*rfid), 1u32);
            }
            _ => panic!(),
        }
    }

    #[test]
    fn nested_member_access() {
        let text = "
        fn test(s: S2) -> i64 {
            return s.s.a + s.s.b;
        }

        struct S {
            a: i64,
            b: i64,
        }

        struct S2 {
            s: S,
        }
        ";
        let mut table = StringTable::new();
        let module = compile(text, &mut table);

        let mut project = MirProject::new();
        let mirs = transform::module_transform(&module, &mut project);
        assert_eq!(mirs.len(), 1);

        let mir = &mirs[0];
        assert_eq!(mir.len(), 1);
        let bb = mir.get_bb(BasicBlockId::new(0));

        let stm = bb.get_stm(0);
        match stm.kind() {
            StatementKind::Assign(
                _,
                RValue::BinOp(
                    BinOp::Add,
                    Operand::LValue(LValue::Access(lv, Accessor::Field(lfid, lfty))),
                    Operand::LValue(LValue::Access(rv, Accessor::Field(rfid, rfty))),
                ),
            ) => {
                let expected_ty = project.find_type(&Type::I64).unwrap();

                let lv = if let LValue::Access(lv, Accessor::Field(fid, _)) = lv.as_ref() {
                    assert_eq!(u32::from(*fid), 0);
                    lv
                } else {
                    panic!()
                };
                let rv = if let LValue::Access(rv, Accessor::Field(fid, _)) = rv.as_ref() {
                    assert_eq!(u32::from(*fid), 0);
                    rv
                } else {
                    panic!()
                };
                assert_eq!(lv, rv);

                assert_eq!(*lfty, expected_ty);
                assert_eq!(*rfty, expected_ty);

                assert_eq!(u32::from(*lfid), 0);
                assert_eq!(u32::from(*rfid), 1);
            }
            _ => panic!(),
        }
    }

    fn to_code(e: &Expression<()>, table: &StringTable) -> String {
        match e {
            Expression::StringLiteral(_, sid) => format!("\"{}\"", table.get(*sid).unwrap()),
            _ => e.root_str(),
        }
    }
}
