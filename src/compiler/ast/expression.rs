use crate::{
    compiler::{source::SourceIr, Span},
    StringId,
};

use super::{
    node::{
        Context, Node, NodeType, {PostOrderIter, PreOrderIter},
    },
    path::Path,
    statement::Statement,
    ty::Type,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Expression<I> {
    Null(I),
    U8(I, u8),
    U16(I, u16),
    U32(I, u32),
    U64(I, u64),
    I8(I, i8),
    I16(I, i16),
    I32(I, i32),
    I64(I, i64),
    F64(I, f64),
    Boolean(I, bool),
    StringLiteral(I, StringId),
    ArrayExpression(I, Vec<Expression<I>>, usize),
    ArrayAt {
        context: I,
        array: Box<Expression<I>>,
        index: Box<Expression<I>>,
    },
    SizeOf(I, Box<Type>),
    CustomType(I, Path),
    Identifier(I, StringId),
    Path(I, Path),
    MemberAccess(I, Box<Expression<I>>, StringId),
    IdentifierDeclare(I, StringId, Type),
    RoutineCall(I, RoutineCall, Path, Vec<Expression<I>>),
    StructExpression(I, Path, Vec<(StringId, Expression<I>)>),
    If {
        context: I,
        cond: Box<Expression<I>>,
        if_arm: Box<Expression<I>>,
        else_arm: Option<Box<Expression<I>>>,
    },
    While {
        context: I,
        cond: Box<Expression<I>>,
        body: Box<Expression<I>>,
    },
    ExpressionBlock(I, Vec<Statement<I>>, Option<Box<Expression<I>>>),

    BinaryOp(I, BinaryOperator, Box<Expression<I>>, Box<Expression<I>>),
    UnaryOp(I, UnaryOperator, Box<Expression<I>>),

    Yield(I, Box<Expression<I>>),
}

impl<M: Context> SourceIr for Expression<M> {
    fn span(&self) -> Span {
        self.context().span()
    }
}

impl<M: Context> Node<M> for Expression<M> {
    fn context(&self) -> &M {
        use Expression::*;
        match self {
            Null(m)
            | U8(m, ..)
            | U16(m, ..)
            | U32(m, ..)
            | U64(m, ..)
            | I8(m, ..)
            | I16(m, ..)
            | I32(m, ..)
            | I64(m, ..)
            | F64(m, ..)
            | Boolean(m, ..)
            | StringLiteral(m, ..)
            | SizeOf(m, ..)
            | CustomType(m, ..)
            | Identifier(m, ..)
            | IdentifierDeclare(m, ..)
            | Path(m, ..)
            | MemberAccess(m, ..)
            | BinaryOp(m, ..)
            | UnaryOp(m, ..)
            | If { context: m, .. }
            | While { context: m, .. }
            | ExpressionBlock(m, ..)
            | Yield(m, ..)
            | RoutineCall(m, ..) => m,
            StructExpression(m, ..) => m,
            ArrayExpression(m, _, _) => m,
            ArrayAt { context: m, .. } => m,
        }
    }

    fn get_context_mut(&mut self) -> &mut M {
        use Expression::*;
        match self {
            Null(m)
            | U8(m, ..)
            | U16(m, ..)
            | U32(m, ..)
            | U64(m, ..)
            | I8(m, ..)
            | I16(m, ..)
            | I32(m, ..)
            | I64(m, ..)
            | F64(m, ..)
            | Boolean(m, ..)
            | StringLiteral(m, ..)
            | SizeOf(m, ..)
            | CustomType(m, ..)
            | Identifier(m, ..)
            | IdentifierDeclare(m, ..)
            | Path(m, ..)
            | MemberAccess(m, ..)
            | BinaryOp(m, ..)
            | UnaryOp(m, ..)
            | If { context: m, .. }
            | While { context: m, .. }
            | ExpressionBlock(m, ..)
            | Yield(m, ..)
            | RoutineCall(m, ..) => m,
            StructExpression(m, ..) => m,
            ArrayExpression(m, _, _) => m,
            ArrayAt { context: m, .. } => m,
        }
    }

    fn node_type(&self) -> NodeType {
        match self {
            Expression::RoutineCall(..) => NodeType::RoutineCall,
            Expression::BinaryOp(..) => NodeType::BinOp,
            _ => NodeType::Expression,
        }
    }

    fn children(&self) -> Vec<&dyn Node<M>> {
        use Expression::*;
        match self {
            StructExpression(.., se) => {
                let mut o: Vec<&dyn Node<M>> = vec![];
                for (_, e) in se.into_iter() {
                    o.push(e);
                }
                o
            }
            ArrayAt { array, index, .. } => vec![array.as_ref(), index.as_ref()],
            MemberAccess(_, src, _) => vec![src.as_ref()],
            BinaryOp(.., l, r) => vec![l.as_ref(), r.as_ref()],
            UnaryOp(.., r) => vec![r.as_ref()],
            If {
                cond,
                if_arm,
                else_arm,
                ..
            } => {
                let mut o: Vec<&dyn Node<M>> = vec![cond.as_ref(), if_arm.as_ref()];
                if let Some(e) = else_arm {
                    o.push(e.as_ref());
                }
                o
            }
            While { cond, body, .. } => {
                let o: Vec<&dyn Node<M>> = vec![cond.as_ref(), body.as_ref()];
                o
            }
            ExpressionBlock(_, stms, exp) => {
                let mut o: Vec<&dyn Node<M>> = vec![];
                for s in stms.into_iter() {
                    o.push(s);
                }
                if let Some(e) = exp {
                    o.push(e.as_ref());
                }
                o
            }
            Yield(_, e) => vec![e.as_ref()],
            RoutineCall(.., exps) => {
                let mut o: Vec<&dyn Node<M>> = vec![];
                for e in exps.into_iter() {
                    o.push(e);
                }
                o
            }
            Null(..)
            | U8(..)
            | U16(..)
            | U32(..)
            | U64(..)
            | I8(..)
            | I16(..)
            | I32(..)
            | I64(..)
            | F64(..)
            | Boolean(..)
            | StringLiteral(..)
            | ArrayExpression(_, _, _)
            | SizeOf(..)
            | CustomType(..)
            | Identifier(..)
            | IdentifierDeclare(..)
            | Path(..) => vec![],
        }
    }

    fn name(&self) -> Option<StringId> {
        None
    }

    fn iter_postorder(&self) -> PostOrderIter<M> {
        PostOrderIter::new(self)
    }

    fn iter_preorder(&self) -> PreOrderIter<M> {
        PreOrderIter::new(self)
    }
}

impl<M> std::fmt::Display for Expression<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.write_str(&self.root_str())
    }
}

impl<I> Expression<I> {
    pub fn root_str(&self) -> String {
        use Expression::*;
        match self {
            Null(_) => format!("null"),
            U8(_, v) => format!("u8({})", v),
            U16(_, v) => format!("u16({})", v),
            U32(_, v) => format!("u32({})", v),
            U64(_, v) => format!("u64({})", v),
            I8(_, v) => format!("i8({})", v),
            I16(_, v) => format!("i16({})", v),
            I32(_, v) => format!("i32({})", v),
            I64(_, v) => format!("i64({})", v),
            F64(_, v) => format!("f64({})", v),
            Boolean(_, v) => format!("bool({})", v),
            StringLiteral(_, v) => format!("\"{}\"", v),
            ArrayExpression(_, v, _) => format!(
                "[{}]",
                v.iter()
                    .map(|e| format!("{}", e))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            ArrayAt { array, index, .. } => format!("{}[{}]", array, index),
            SizeOf(_, ty) => format!("size_of({})", ty),
            CustomType(_, v) => format!("{}", v),
            Identifier(_, v) => format!("{}", v),
            IdentifierDeclare(_, v, p) => format!("{}:{}", v, p),
            MemberAccess(_, s, m) => format!("{}.{}", s.root_str(), m),
            Path(_, path) => format!("{}", path),
            BinaryOp(_, op, _, _) => format!("{}", op),
            UnaryOp(_, op, _) => format!("{}", op),
            StructExpression(_, name, ..) => format!("intialization for struct {}", name),
            RoutineCall(_, call, name, ..) => format!("{} {}", call, name),
            If { .. } => "if".into(),
            While { .. } => "while".into(),
            ExpressionBlock(..) => "expression block".into(),
            Yield(_, _) => "yield".into(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    BAnd,
    BOr,
    Eq,
    NEq,
    Ls,
    LsEq,
    Gr,
    GrEq,
    RawPointerOffset,
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        use BinaryOperator::*;
        match self {
            Add => f.write_str("+"),
            Sub => f.write_str("-"),
            Mul => f.write_str("*"),
            Div => f.write_str("/"),
            BAnd => f.write_str("&&"),
            BOr => f.write_str("||"),
            Eq => f.write_str("=="),
            NEq => f.write_str("!="),
            Ls => f.write_str("<"),
            LsEq => f.write_str("<="),
            Gr => f.write_str(">"),
            GrEq => f.write_str(">="),
            RawPointerOffset => f.write_str("@"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOperator {
    Negate,
    Not,
    AddressConst,
    AddressMut,
    DerefRawPointer,
}

impl std::fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        use UnaryOperator::*;
        match self {
            Negate => f.write_str("-"),
            Not => f.write_str("!"),
            AddressConst => f.write_str("@const "),
            AddressMut => f.write_str("@mut "),
            DerefRawPointer => f.write_str("^"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RoutineCall {
    Function,
    CoroutineInit,
    Extern,
}

impl std::fmt::Display for RoutineCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        use RoutineCall::*;
        match self {
            CoroutineInit => f.write_str("init"),
            Function => f.write_str("call"),
            Extern => f.write_str("call extern"),
        }
    }
}
