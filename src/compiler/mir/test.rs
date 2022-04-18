#[cfg(test)]
pub mod tests {
    use crate::{
        compiler::{
            ast::*,
            diagnostics::Logger,
            lexer::{tokens::Token, LexerError},
            mir::{ir::*, project::*, transform},
            parser::Parser,
            semantics::semanticnode::SemanticContext,
            CompilerDisplay, CompilerError, Lexer, SourceMap,
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
        let ast = match parser.parse(main, &tokens) {
            Ok(ast) => ast.unwrap(),
            Err(err) => {
                panic!("{}", err.fmt(&sm, table).unwrap());
            }
        };
        match resolve_types(&ast, main_mod, main_fn, &logger) {
            Ok(module) => module,
            Err(err) => {
                panic!("{}", err.fmt(&sm, table).unwrap());
            }
        }
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
            transform::transform(&module, &[], &mut project).unwrap();
            println!("{}", project);
        }

        #[test]
        fn print_mir_scopes() {
            let text = "
        fn test() -> i64 {
            let x: i64 := 5;
            {
                let z: i64 := x;
                let x: f64 := 5.0;
                let y: f64 := x;
            };
            let y: i64 := x;
            return 1;
        }
        ";
            let mut table = StringTable::new();
            let module = compile(text, &mut table);
            let mut project = MirProject::new();
            transform::transform(&module, &[], &mut project).unwrap();
            println!("{}", project);
        }

        #[test]
        fn print_mir_fn_call() {
            let text = "
        fn test() -> i64 {
            let x: i64 := test2();
            return 1 + 2 + 3 + x;
        }
        
        fn test2() -> i64 {
            return 2;
        }
        ";
            let mut table = StringTable::new();
            let module = compile(text, &mut table);
            let mut project = MirProject::new();
            transform::transform(&module, &[], &mut project).unwrap();
            println!("{}", project);
        }

        #[test]
        fn print_mir_unit_fn_call() {
            let text = "
        fn test() -> i64 {
            test2();
            return 1 + 2 + 3;
        }
        
        fn test2() {
            return;
        }
        ";
            let mut table = StringTable::new();
            let module = compile(text, &mut table);
            let mut project = MirProject::new();
            transform::transform(&module, &[], &mut project).unwrap();
            println!("{}", project);
        }

        #[test]
        fn print_mir_extern() {
            let text = "
        extern fn printf(x: i64, ...);

        fn test() {
            printf(1, 2, 3);
            return;
        }
        ";
            let mut table = StringTable::new();
            let module = compile(text, &mut table);
            let mut project = MirProject::new();
            transform::transform(&module, &[], &mut project).unwrap();
            println!("{}", project);
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
            transform::transform(&module, &[], &mut project).unwrap();
            println!("{}", project);
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
            transform::transform(&module, &[], &mut project).unwrap();
            println!("{}", project);
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
            transform::transform(&module, &[], &mut project).unwrap();
            println!("{}", project);
        }

        #[test]
        fn print_mir_for_struct_expression() {
            let text = "
        fn test() -> Test {
            let x: Test := Test{a: 1, b: 2};
            return x;
        }
        struct Test {
            a: i64,
            b: i64,
        }
        ";
            let mut table = StringTable::new();
            let module = compile(text, &mut table);
            let mut project = MirProject::new();
            transform::transform(&module, &[], &mut project).unwrap();
            println!("{}", project);
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
            transform::transform(&module, &[], &mut project).unwrap();
            println!("{}", project);
        }

        #[test]
        fn print_mir_for_deref() {
            let text = "
        fn test() -> i64 {
            let mut x: i64 := 5;
            let p: *mut i64 := @mut x;
            mut ^p := 10;
            return ^p;
        }
        ";
            let mut table = StringTable::new();
            let module = compile(text, &mut table);
            let mut project = MirProject::new();
            transform::transform(&module, &[], &mut project).unwrap();
            println!("{}", project);
        }

        #[test]
        fn print_mir_submodule() {
            let text = "
                fn test() -> i64 {
                    return my_mod::test();
                }

                mod my_mod {
                    fn test() -> i64 {
                        return 2;
                    }
                }
                ";
            let mut table = StringTable::new();
            let module = compile(text, &mut table);
            let mut project = MirProject::new();
            transform::transform(&module, &[], &mut project).unwrap();
            println!("{}", project);
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
        transform::transform(&module, &[], &mut project).unwrap();

        let path: Path = to_path(&["main", "test"], &table);
        let def_id = project.find_def(&path).unwrap();
        let mir = project.get_def_fn(def_id).unwrap();

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
        transform::transform(&module, &[], &mut project).unwrap();

        let path: Path = to_path(&["main", "test"], &table);
        let def_id = project.find_def(&path).unwrap();
        let mir = project.get_def_fn(def_id).unwrap();

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
        transform::transform(&module, &[], &mut project).unwrap();

        let path: Path = to_path(&["main", "test"], &table);
        let def_id = project.find_def(&path).unwrap();
        let mir = project.get_def_fn(def_id).unwrap();

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
    fn pointer_offset() {
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
                        let p2: {ptr_ty} := p @ 2;
                        return;
                    }}
                    ",
                );
                let module = compile(&text, &mut table);
                let mut project = MirProject::new();
                transform::transform(&module, &[], &mut project).unwrap();

                let path: Path = to_path(&["main", "test"], &table);
                let def_id = project.find_def(&path).unwrap();
                let mir = project.get_def_fn(def_id).unwrap();

                let bb = mir.get_bb(BasicBlockId::new(0));
                let stm = bb.get_stm(3);
                match stm.kind() {
                    StatementKind::Assign(_, r) => {
                        let expected = RValue::BinOp(
                            BinOp::RawPointerOffset,
                            Operand::LValue(LValue::Var(VarId::new(1))),
                            Operand::Constant(Constant::I64(2)),
                        );
                        assert_eq!(*r, expected);
                    }
                }
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
                transform::transform(&module, &[], &mut project).unwrap();

                let path: Path = to_path(&["main", "test"], &table);
                let def_id = project.find_def(&path).unwrap();
                let mir = project.get_def_fn(def_id).unwrap();

                let bb = mir.get_bb(BasicBlockId::new(0));
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
                Constant::StringLiteral(DefId::new(1)),
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
            transform::transform(&module, &[], &mut project).unwrap();

            let path: Path = to_path(&["main", "test"], &table);
            let def_id = project.find_def(&path).unwrap();
            let mir = project.get_def_fn(def_id).unwrap();

            let bb = mir.get_bb(BasicBlockId::new(0));
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
            (BinaryOperator::Div, BinOp::SIDiv, None),
            (BinaryOperator::Eq, BinOp::Eq, Some(Type::Bool)),
            (BinaryOperator::NEq, BinOp::Ne, Some(Type::Bool)),
            (BinaryOperator::Ls, BinOp::SILt, Some(Type::Bool)),
            (BinaryOperator::LsEq, BinOp::SILe, Some(Type::Bool)),
            (BinaryOperator::Gr, BinOp::SIGt, Some(Type::Bool)),
            (BinaryOperator::GrEq, BinOp::SIGe, Some(Type::Bool)),
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
                // If the test input is an unsigned integer then swap the expected operator to the
                // unsigned form
                let exp_op = if literal_ty.is_signed_int() {
                    exp_op
                } else if literal_ty.is_unsigned_int() {
                    match exp_op {
                        BinOp::Add => BinOp::Add,
                        BinOp::Sub => BinOp::Sub,
                        BinOp::Mul => BinOp::Mul,
                        BinOp::SIDiv => BinOp::UIDiv,
                        BinOp::UIDiv => BinOp::UIDiv,
                        BinOp::Eq => BinOp::Eq,
                        BinOp::Ne => BinOp::Ne,
                        BinOp::SILe => BinOp::UILe,
                        BinOp::SILt => BinOp::UILt,
                        BinOp::UILe => todo!(),
                        BinOp::UILt => todo!(),
                        BinOp::SIGe => BinOp::UIGe,
                        BinOp::SIGt => BinOp::UIGt,
                        BinOp::And => BinOp::And,
                        BinOp::Or => BinOp::Or,
                        BinOp::RawPointerOffset => todo!(),
                        BinOp::UIGe => todo!(),
                        BinOp::UIGt => todo!(),
                        _ => todo!(),
                    }
                } else {
                    match op {
                        BinaryOperator::Add => BinOp::FAdd,
                        BinaryOperator::Sub => BinOp::FSub,
                        BinaryOperator::Mul => BinOp::FMul,
                        BinaryOperator::Div => BinOp::FDiv,
                        BinaryOperator::Eq => BinOp::FEq,
                        BinaryOperator::NEq => BinOp::FNe,
                        BinaryOperator::Ls => BinOp::FLt,
                        BinaryOperator::LsEq => BinOp::FLe,
                        BinaryOperator::Gr => BinOp::FGt,
                        BinaryOperator::GrEq => BinOp::FGe,
                        _ => exp_op,
                    }
                };
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
                transform::transform(&module, &[], &mut project).unwrap();

                let path: Path = to_path(&["main", "test"], &table);
                let def_id = project.find_def(&path).unwrap();
                let mir = project.get_def_fn(def_id).unwrap();

                let bb = mir.get_bb(BasicBlockId::new(0));
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
        for (literal_ty, v, exp_op, exp) in &[
            (
                Type::I8,
                Expression::I8((), 1),
                UnOp::Negate,
                Constant::I8(1),
            ),
            (
                Type::I16,
                Expression::I16((), 1),
                UnOp::Negate,
                Constant::I16(1),
            ),
            (
                Type::I32,
                Expression::I32((), 1),
                UnOp::Negate,
                Constant::I32(1),
            ),
            (
                Type::I64,
                Expression::I64((), 1),
                UnOp::Negate,
                Constant::I64(1),
            ),
            (
                Type::F64,
                Expression::F64((), 5.0),
                UnOp::FNegate,
                Constant::F64(5.0),
            ),
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
            transform::transform(&module, &[], &mut project).unwrap();

            let path: Path = to_path(&["main", "test"], &table);
            let def_id = project.find_def(&path).unwrap();
            let mir = project.get_def_fn(def_id).unwrap();

            let bb = mir.get_bb(BasicBlockId::new(0));
            let stm = bb.get_stm(0);
            match stm.kind() {
                StatementKind::Assign(_, r) => {
                    assert_eq!(*r, RValue::UnOp(*exp_op, Operand::Constant(exp.clone())));
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
                transform::transform(&module, &[], &mut project).unwrap();

                let path: Path = to_path(&["main", "test"], &table);
                let def_id = project.find_def(&path).unwrap();
                let mir = project.get_def_fn(def_id).unwrap();

                let bb = mir.get_bb(BasicBlockId::new(0));
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
    fn deref_rawpointer() {
        let mut table = StringTable::new();
        for literal_ty in &[
            Type::Bool,
            Type::I8,
            Type::I16,
            Type::I32,
            Type::I64,
            Type::U8,
            Type::U16,
            Type::U32,
            Type::U64,
            Type::StringLiteral,
        ] {
            let text = format!(
                "
                    fn test(p: *mut {literal_ty}, v: {literal_ty}) {{ 
                        mut ^p := v;
                        let x: {literal_ty} := ^p;
                        return;
                    }}
                    ",
            );
            let module = compile(&text, &mut table);
            let mut project = MirProject::new();
            transform::transform(&module, &[], &mut project).unwrap();

            let path: Path = to_path(&["main", "test"], &table);
            let def_id = project.find_def(&path).unwrap();
            let mir = project.get_def_fn(def_id).unwrap();

            let bb = mir.get_bb(BasicBlockId::new(0));

            // mut ^p := ..;
            let stm = bb.get_stm(0);
            match stm.kind() {
                StatementKind::Assign(l, _) => {
                    assert_eq!(
                        *l,
                        LValue::Access(Box::new(LValue::Var(VarId::new(0))), Accessor::Deref)
                    );
                }
            }

            // let x := ^p;
            let stm = bb.get_stm(1);
            match stm.kind() {
                StatementKind::Assign(_, r) => {
                    assert_eq!(
                        *r,
                        RValue::Use(Operand::LValue(LValue::Access(
                            Box::new(LValue::Var(VarId::new(0))),
                            Accessor::Deref
                        )))
                    );
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
            transform::transform(&module, &[], &mut project).unwrap();

            let path: Path = to_path(&["main", "test"], &table);
            let def_id = project.find_def(&path).unwrap();
            let mir = project.get_def_fn(def_id).unwrap();

            let bb = mir.get_bb(BasicBlockId::new(0));
            let stm = bb.get_stm(0);
            match stm.kind() {
                StatementKind::Assign(_, r) => {
                    assert_eq!(*r, RValue::UnOp(UnOp::Not, Operand::Constant(exp.clone())));
                }
            }
        }
    }

    #[test]
    fn mutate() {
        let text = "
        fn test() -> i64 {
            let mut x: i64 := 5;
            mut x := 6;
            return x;
        }
        ";
        let mut table = StringTable::new();
        let module = compile(text, &mut table);
        let mut project = MirProject::new();
        transform::transform(&module, &[], &mut project).unwrap();

        let path: Path = to_path(&["main", "test"], &table);
        let def_id = project.find_def(&path).unwrap();
        let mir = project.get_def_fn(def_id).unwrap();

        // Check that the right number of BBs are created
        assert_eq!(mir.len(), 1);

        // Check first transition into cond BB
        let bb = mir.get_bb(BasicBlockId::new(0));

        let letx = bb.get_stm(0); // let x: i64 := 5;
        let expected_letx = StatementKind::Assign(
            LValue::Var(VarId::new(0)),
            RValue::Use(Operand::Constant(Constant::I64(5))),
        );
        assert_eq!(letx.kind(), &expected_letx);

        let mutx = bb.get_stm(1); // mut x := 6;
        let expected_mutx = StatementKind::Assign(
            LValue::Var(VarId::new(0)),
            RValue::Use(Operand::Constant(Constant::I64(6))),
        );
        assert_eq!(mutx.kind(), &expected_mutx);
    }

    #[test]
    fn variable_scopes() {
        let text = "
        fn test() -> i64 {
            let x: i64 := 5;
            {
                let y: i64 := x;
                let x: f64 := 5.0;
                let z: f64 := x;
            };
            let y: i64 := x;
            return y;
        }
        ";
        let mut table = StringTable::new();
        let module = compile(text, &mut table);
        let mut project = MirProject::new();
        transform::transform(&module, &[], &mut project).unwrap();

        let path: Path = to_path(&["main", "test"], &table);
        let def_id = project.find_def(&path).unwrap();
        let mir = project.get_def_fn(def_id).unwrap();

        // Check that the right number of BBs are created
        assert_eq!(mir.len(), 1);

        // Check first transition into cond BB
        let bb = mir.get_bb(BasicBlockId::new(0));

        let lety = bb.get_stm(1); // let y: i64 := x;
        let expected_lety = StatementKind::Assign(
            LValue::Var(VarId::new(1)),
            RValue::Use(Operand::LValue(LValue::Var(VarId::new(0)))),
        );
        assert_eq!(lety.kind(), &expected_lety);

        let letx = bb.get_stm(2); // let x: f64 := 5.0;
        let expected_letx = StatementKind::Assign(
            LValue::Var(VarId::new(2)),
            RValue::Use(Operand::Constant(Constant::F64(5.0))),
        );
        assert_eq!(letx.kind(), &expected_letx);

        let letz = bb.get_stm(3);
        let expected_letz = StatementKind::Assign(
            LValue::Var(VarId::new(3)),
            RValue::Use(Operand::LValue(LValue::Var(VarId::new(2)))),
        );
        assert_eq!(letz.kind(), &expected_letz);

        let letx2 = bb.get_stm(4);
        let expected_letx2 = StatementKind::Assign(
            LValue::Var(VarId::new(4)),
            RValue::Use(Operand::LValue(LValue::Var(VarId::new(0)))),
        );
        assert_eq!(letx2.kind(), &expected_letx2);
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
        transform::transform(&module, &[], &mut project).unwrap();

        let path: Path = to_path(&["main", "test"], &table);
        let def_id = project.find_def(&path).unwrap();
        let mir = project.get_def_fn(def_id).unwrap();

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
        transform::transform(&module, &[], &mut project).unwrap();

        let path: Path = to_path(&["main", "test"], &table);
        let def_id = project.find_def(&path).unwrap();
        let mir = project.get_def_fn(def_id).unwrap();

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
        transform::transform(&module, &[], &mut project).unwrap();

        let path: Path = to_path(&["main", "test"], &table);
        let def_id = project.find_def(&path).unwrap();
        let mir = project.get_def_fn(def_id).unwrap();

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

    #[test]
    fn add_multiple_functions() {
        let text = "
        fn test(y: i64) -> i64 {
            return y;
        }

        fn test2(x: i64) -> i64 {
            return x;
        }
        ";
        let mut table = StringTable::new();
        let module = compile(text, &mut table);

        let mut project = MirProject::new();
        transform::transform(&module, &[], &mut project).unwrap();

        let path: Path = to_path(&["main", "test"], &table);
        let def_id = project.find_def(&path).unwrap();
        let mir = project.get_def_fn(def_id).unwrap();
        assert_eq!(mir.len(), 1);
        assert_eq!(mir.path(), &path);

        let path2: Path = to_path(&["main", "test2"], &table);
        let def2_id = project.find_def(&path2).unwrap();
        let mir2 = project.get_def_fn(def2_id).unwrap();
        assert_eq!(mir2.len(), 1);
        assert_eq!(mir2.path(), &path2);

        assert!(def_id != def2_id);
    }

    #[test]
    fn call_function() {
        let text = "
        fn test() -> i64 {
            return test2(1);
        }

        fn test2(x: i64) -> i64 {
            return x;
        }
        ";
        let mut table = StringTable::new();
        let module = compile(text, &mut table);

        let mut project = MirProject::new();
        transform::transform(&module, &[], &mut project).unwrap();

        let path: Path = to_path(&["main", "test"], &table);
        let def_id = project.find_def(&path).unwrap();
        let mir = project.get_def_fn(def_id).unwrap();
        assert_eq!(mir.len(), 2); // There should be 2: the first BB calls the func and the second is the reentry BB
        assert_eq!(mir.path(), &path);

        // Get the Defid of the expected target
        let path: Path = to_path(&["main", "test2"], &table);
        let expected_target = project.find_def(&path).unwrap();

        // Check the BB terminator
        let term = mir.get_bb(BasicBlockId::new(0)).get_term().unwrap();
        let (func, args, reentry) = match term.kind() {
            TerminatorKind::CallFn {
                func,
                args,
                reentry,
            } => (func, args, reentry),
            _ => panic!(),
        };

        assert_eq!(*func, Operand::LValue(LValue::Static(expected_target)));
        assert_eq!(args[0], Operand::Constant(Constant::I64(1)));

        let expected_temp = TempId::new(0);
        assert_eq!(reentry.0, LValue::Temp(expected_temp));
        assert_eq!(reentry.1, BasicBlockId::new(1));

        // Check temp type
        let ret_val = mir.get_temp(expected_temp);
        let expected_ty = project.find_type(&Type::I64).unwrap();
        assert_eq!(ret_val.ty(), expected_ty);
    }

    #[test]
    fn call_unit_function() {
        let text = "
        fn test() -> i64 {
            test2(3, 2i32);
            return 1;
        }

        fn test2(x: i64, y: i32) {
            return;
        }
        ";
        let mut table = StringTable::new();
        let module = compile(text, &mut table);

        let mut project = MirProject::new();
        transform::transform(&module, &[], &mut project).unwrap();

        let path: Path = to_path(&["main", "test"], &table);
        let def_id = project.find_def(&path).unwrap();
        let mir = project.get_def_fn(def_id).unwrap();
        assert_eq!(mir.len(), 2); // There should be 2: the first BB calls the func and the second is the reentry BB
        assert_eq!(mir.path(), &path);

        // Get the Defid of the expected target
        let path: Path = to_path(&["main", "test2"], &table);
        let expected_target = project.find_def(&path).unwrap();

        // Check the BB terminator
        let term = mir.get_bb(BasicBlockId::new(0)).get_term().unwrap();
        let (func, args, reentry) = match term.kind() {
            TerminatorKind::CallFn {
                func,
                args,
                reentry,
            } => (func, args, reentry),
            _ => panic!(),
        };

        assert_eq!(*func, Operand::LValue(LValue::Static(expected_target)));
        assert_eq!(args[0], Operand::Constant(Constant::I64(3)));
        assert_eq!(args[1], Operand::Constant(Constant::I32(2)));

        let expected_temp = TempId::new(0);
        assert_eq!(reentry.0, LValue::Temp(expected_temp));
        assert_eq!(reentry.1, BasicBlockId::new(1));

        // Check temp type
        let ret_val = mir.get_temp(expected_temp);
        let expected_ty = project.find_type(&Type::Unit).unwrap();
        assert_eq!(ret_val.ty(), expected_ty);
    }

    #[test]
    fn call_recursive_function() {
        let text = "
        fn test() -> i64 {
            return test();
        }
        ";
        let mut table = StringTable::new();
        let module = compile(text, &mut table);

        let mut project = MirProject::new();
        transform::transform(&module, &[], &mut project).unwrap();

        let path: Path = to_path(&["main", "test"], &table);
        let expected_target = project.find_def(&path).unwrap();
        let mir = project.get_def_fn(expected_target).unwrap();
        assert_eq!(mir.len(), 2); // There should be 2: the first BB calls the func and the second is the reentry BB
        assert_eq!(mir.path(), &path);

        // Check the BB terminator
        let term = mir.get_bb(BasicBlockId::new(0)).get_term().unwrap();
        let (func, args, reentry) = match term.kind() {
            TerminatorKind::CallFn {
                func,
                args,
                reentry,
            } => (func, args, reentry),
            _ => panic!(),
        };

        assert_eq!(*func, Operand::LValue(LValue::Static(expected_target)));
        assert_eq!(args.len(), 0);

        let expected_temp = TempId::new(0);
        assert_eq!(reentry.0, LValue::Temp(expected_temp));
        assert_eq!(reentry.1, BasicBlockId::new(1));

        // Check temp type
        let ret_val = mir.get_temp(expected_temp);
        let expected_ty = project.find_type(&Type::I64).unwrap();
        assert_eq!(ret_val.ty(), expected_ty);
    }

    #[test]
    fn call_extern() {
        let text = "
        extern fn test2(x: i64) -> i64;

        fn test() -> i64 {
            return test2(1);
        }
        ";
        let mut table = StringTable::new();
        let module = compile(text, &mut table);

        let mut project = MirProject::new();
        transform::transform(&module, &[], &mut project).unwrap();

        let path: Path = to_path(&["main", "test"], &table);
        let def_id = project.find_def(&path).unwrap();
        let mir = project.get_def_fn(def_id).unwrap();
        assert_eq!(mir.len(), 2); // There should be 2: the first BB calls the func and the second is the reentry BB
        assert_eq!(mir.path(), &path);

        // Get the Defid of the expected target
        let path: Path = vec![Element::Id(table.find("test2").unwrap())].into();
        let expected_target = project.find_def(&path).unwrap();

        // Check the BB terminator
        let term = mir.get_bb(BasicBlockId::new(0)).get_term().unwrap();
        let (func, args, reentry) = match term.kind() {
            TerminatorKind::CallFn {
                func,
                args,
                reentry,
            } => (func, args, reentry),
            _ => panic!(),
        };

        assert_eq!(*func, Operand::LValue(LValue::Static(expected_target)));
        assert_eq!(args[0], Operand::Constant(Constant::I64(1)));

        let expected_temp = TempId::new(0);
        assert_eq!(reentry.0, LValue::Temp(expected_temp));
        assert_eq!(reentry.1, BasicBlockId::new(1));

        // Check temp type
        let ret_val = mir.get_temp(expected_temp);
        let expected_ty = project.find_type(&Type::I64).unwrap();
        assert_eq!(ret_val.ty(), expected_ty);
    }

    #[test]
    fn call_variadic_extern() {
        let text = "
        extern fn test2(x: i64, ...) -> i64;

        fn test() -> i64 {
            return test2(1, 2, 3);
        }
        ";
        let mut table = StringTable::new();
        let module = compile(text, &mut table);

        let mut project = MirProject::new();
        transform::transform(&module, &[], &mut project).unwrap();

        let path: Path = to_path(&["main", "test"], &table);
        let def_id = project.find_def(&path).unwrap();
        let mir = project.get_def_fn(def_id).unwrap();
        assert_eq!(mir.len(), 2); // There should be 2: the first BB calls the func and the second is the reentry BB
        assert_eq!(mir.path(), &path);

        // Get the Defid of the expected target
        let path: Path = vec![Element::Id(table.find("test2").unwrap())].into();
        let expected_target = project.find_def(&path).unwrap();

        // check that the extern is variadic
        let target_def = project.get_def_fn(expected_target).unwrap();
        assert!(target_def.has_varargs());

        // Check the BB terminator
        let term = mir.get_bb(BasicBlockId::new(0)).get_term().unwrap();
        let (func, args, reentry) = match term.kind() {
            TerminatorKind::CallFn {
                func,
                args,
                reentry,
            } => (func, args, reentry),
            _ => panic!(),
        };

        assert_eq!(*func, Operand::LValue(LValue::Static(expected_target)));
        assert_eq!(args[0], Operand::Constant(Constant::I64(1)));

        let expected_temp = TempId::new(0);
        assert_eq!(reentry.0, LValue::Temp(expected_temp));
        assert_eq!(reentry.1, BasicBlockId::new(1));

        // Check temp type
        let ret_val = mir.get_temp(expected_temp);
        let expected_ty = project.find_type(&Type::I64).unwrap();
        assert_eq!(ret_val.ty(), expected_ty);
    }

    #[test]
    fn casting() {
        let text = "
        fn test() -> i32 {
            let x: i64 := 5;
            let y: i32 := x as i32;
            return y;
        }
        ";
        let mut table = StringTable::new();
        let module = compile(text, &mut table);
        let mut project = MirProject::new();
        transform::transform(&module, &[], &mut project).unwrap();

        let path: Path = to_path(&["main", "test"], &table);
        let def_id = project.find_def(&path).unwrap();
        let mir = project.get_def_fn(def_id).unwrap();

        // Check that the right number of BBs are created
        assert_eq!(mir.len(), 1);

        // Check first transition into cond BB
        let bb = mir.get_bb(BasicBlockId::new(0));

        let cast = bb.get_stm(1); // x as i32;
        let expected_cast = RValue::Cast(
            Operand::LValue(LValue::Var(VarId::new(0))),
            project.find_type(&Type::I64).unwrap(),
            project.find_type(&Type::I32).unwrap(),
        );
        let StatementKind::Assign(_, rhs) = cast.kind();
        assert_eq!(rhs, &expected_cast);
    }

    #[test]
    fn struct_expression() {
        let text = "
        fn test() -> S {
            let s: S := S{a: 1, b: 2};
            return s;
        }

        struct S {
            a: i64,
            b: i64,
        }
        ";
        let mut table = StringTable::new();
        let module = compile(text, &mut table);

        let mut project = MirProject::new();
        transform::transform(&module, &[], &mut project).unwrap();

        let path: Path = to_path(&["main", "test"], &table);
        let def_id = project.find_def(&path).unwrap();
        let mir = project.get_def_fn(def_id).unwrap();

        assert_eq!(mir.len(), 1);
        let bb = mir.get_bb(BasicBlockId::new(0));

        let stm = bb.get_stm(0);
        match stm.kind() {
            StatementKind::Assign(
                LValue::Access(_, Accessor::Field(fid, _)),
                RValue::Use(Operand::Constant(Constant::I64(1))),
            ) => {
                assert_eq!(u32::from(*fid), 0u32);
            }
            _ => panic!(),
        }

        let stm = bb.get_stm(1);
        match stm.kind() {
            StatementKind::Assign(
                LValue::Access(_, Accessor::Field(fid, _)),
                RValue::Use(Operand::Constant(Constant::I64(2))),
            ) => {
                assert_eq!(u32::from(*fid), 1u32);
            }
            _ => panic!(),
        }
    }

    fn to_path(v: &[&str], table: &StringTable) -> Path {
        let mut path = vec![Element::CanonicalRoot];

        for s in v {
            let id = table.find(s).unwrap();
            path.push(Element::Id(id))
        }

        path.into()
    }

    fn to_code(e: &Expression<()>, table: &StringTable) -> String {
        match e {
            Expression::StringLiteral(_, sid) => format!("\"{}\"", table.get(*sid).unwrap()),
            _ => e.root_str(),
        }
    }
}
