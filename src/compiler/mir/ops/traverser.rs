//! Traverses the MIR representation of a function and calls the appropriate
//! methods on a value which implements the [`super::transformer::Transformer`] trait.

use std::{collections::VecDeque, marker::PhantomData};

use log::debug;

use crate::compiler::mir::{ir::*, MirProject, MirStructDef, MirTypeDef, TransformerError, TypeId};

use super::{transformer::FunctionBuilder, ProgramBuilder};

/// Traverses all the items in an input [`MirProject`] and orchestrates
/// a [`ProgramTransformer`] to transform the input MIR into a target
/// IR form.
pub struct ProgramTraverser<'a> {
    /// Reference to the [`MirProject`] being transformed.
    mir: &'a MirProject,
}

impl<'a> ProgramTraverser<'a> {
    pub fn new(mir: &'a MirProject) -> Self {
        Self { mir }
    }

    /// This function takes an implementation of [`ProgramTransformer`] and uses it to
    /// conver source MIR value into the target IR form.
    pub fn map<'p, L, V, F: FunctionBuilder<L, V>, P: ProgramBuilder<'p, L, V, F>>(
        &self,
        xfmr: &'p mut P,
    ) {
        debug!("Applying given Transformer to MIR");

        // Declare every structure, but do not define the structures yet.
        // This solves the problem of structures with reference cycles
        debug!("Declare any structures");
        for (id, ty) in self.mir.type_iter() {
            match ty {
                MirTypeDef::Structure { path, .. } => xfmr.declare_struct(id, path).unwrap(),
                _ => (),
            }
        }

        // Add every type in the type table to the project
        for (id, ty) in self.mir.type_iter() {
            self.map_type(id, ty, xfmr).unwrap();
        }

        // Declare every function in the ProgramTransformer
        for (id, f) in self.mir.function_iter() {
            xfmr.add_function(id, f.path(), f.get_args(), f.ret_ty())
                .unwrap();
        }

        // Iterate over every function in MIR
        for (id, f) in self.mir.function_iter().filter(|(_, f)| !f.is_extern()) {
            debug!("Transforming: {:?}", f.path());

            // For each function, iterate over every BB
            //self.function = Some(f);
            let mut fn_xfm = xfmr.get_function_transformer(id).unwrap();

            // Create function traverser and pass it the transformer
            let mut traverser = FunctionTraverser::new(self.mir, f, &mut fn_xfm);
            traverser.map();
        }
    }

    fn map_type<'p, L, V, F: FunctionBuilder<L, V>, P: ProgramBuilder<'p, L, V, F>>(
        &self,
        id: TypeId,
        ty: &MirTypeDef,
        xfmr: &mut P,
    ) -> Result<(), TransformerError> {
        debug!("Traversing type {:?}", id);

        // If this is a type that references other types, make sure those referenced types
        // are defined before transforming this type.
        match ty {
            MirTypeDef::Base(_) => (),
            MirTypeDef::Array { ty, .. } => self.map_type(*ty, self.mir.get_type(*ty), xfmr)?,
            MirTypeDef::RawPointer { target, .. } => {
                let target_ty = self.mir.get_type(*target);
                match target_ty {
                    MirTypeDef::Structure { .. } => (),
                    MirTypeDef::Array { .. }
                    | MirTypeDef::Base(..)
                    | MirTypeDef::RawPointer { .. } => self.map_type(*target, target_ty, xfmr)?,
                }
            }
            MirTypeDef::Structure { def, .. } => match def {
                MirStructDef::Declared => panic!("Attempting to convert Undefined structure"),
                MirStructDef::Defined(fields) => {
                    for field in fields {
                        self.map_type(field.ty, self.mir.get_type(field.ty), xfmr)?
                    }
                }
            },
        }

        match xfmr.add_type(id, ty) {
            Ok(_) => Ok(()),
            Err(TransformerError::TypeAlreadyDefined) => Ok(()),
            Err(e) => Err(e),
        }
    }
}

/// This will traverse the MIR representation of a function and use the given
/// [`Transformer`] implementation to generate a new IR for the function.
///
/// `L` - the type used by the target IR to represent addressable expressions
/// or [`LValue`].
///
/// `V` - the type used by the target IR to represent values such as
/// constants or the value read from a [memory location](LValue).
///
/// `T` - the type that implements the [`Transformer`] trait and will actually handle the
/// conversion to the target IR.
pub struct FunctionTraverser<'a, L, V, T: FunctionBuilder<L, V>> {
    xfmr: &'a mut T,
    mir: &'a MirProject,
    function: &'a Procedure,
    _l: PhantomData<L>,
    _v: PhantomData<V>,
}

impl<'a, L, V, T: FunctionBuilder<L, V>> FunctionTraverser<'a, L, V, T> {
    pub fn new(mir: &'a MirProject, function: &'a Procedure, xfmr: &'a mut T) -> Self {
        debug!("New Function Traverser");
        Self {
            xfmr,
            mir,
            function,
            _l: PhantomData,
            _v: PhantomData,
        }
    }

    pub fn map(&mut self) {
        debug!("Applying given Transformer to MIR");

        for (id, bb) in self.function.bb_iter() {
            self.xfmr
                .create_bb(id, bb)
                .expect("BasicBlock already created");
        }

        // Allocate variables
        self.allocate_local_vars();

        // Convert every basic block
        for (id, bb) in self.function.bb_iter() {
            self.basic_block(id, bb)
        }
    }

    fn allocate_local_vars(&mut self) {
        debug!("Allocating local variables");

        // Tell the transformer to make the entry BasicBlock active
        self.xfmr
            .set_bb(ENTRY_BB)
            .expect("There must be at least one BasicBlock for a function");

        // Loop through all arguments and add them to the function builder

        // Move function parameters into local variables
        for (id, arg) in self.function.arg_iter() {
            // get span from arg decl
            // get value of the llvm function parameter associated with arg id
            self.xfmr.alloc_arg(id, arg).unwrap();
        }

        // Loop through all user defined variables and allocate them
        // in the Entry BasicBlock
        for id in self.get_varid_iter() {
            let decl = *self.get_var(id);
            self.xfmr.alloc_var(id, &decl).unwrap();
        }

        // Loop through all temp vars and allocate them
        // in the Entry BasicBlock
        for id in self.get_tempid_iter() {
            let decl = *self.get_temp(id);
            self.xfmr.alloc_temp(id, &decl).unwrap();
        }
    }

    /// Traverses every variable, [statement](Statement), and the final [terminator](Terminator)
    /// in the given [`BasicBlock`] and calls the appropriate conversion functions on the
    /// given [`Transformer`].
    pub fn basic_block(&mut self, id: BasicBlockId, bb: &BasicBlock) {
        self.xfmr.set_bb(id).expect("Could not find BasicBlock");

        // Iterate over the statements in the basic block
        bb.stm_iter().for_each(|s| self.statement(s));

        // Convert the terminator
        let term = bb
            .get_term()
            .expect("Terminator must be defined for a basic block");
        match term.kind() {
            TerminatorKind::Return => self.xfmr.term_return(),
            TerminatorKind::GoTo { target } => self.xfmr.term_goto(*target).unwrap(),
            TerminatorKind::CondGoTo { cond, tru, fls } => {
                let cond = self.operand(cond);
                self.xfmr.term_cond_goto(cond, *tru, *fls).unwrap()
            }
            TerminatorKind::CallFn {
                func,
                args,
                reentry,
            } => {
                // Get the target function
                let target = match func {
                    Operand::Constant(_) => {
                        panic!("Attempting to use a constant in a function call")
                    }
                    Operand::LValue(l) => self.lvalue(l),
                };

                // evaluate arguments?
                let args: VecDeque<_> = args.iter().map(|a| self.operand(a)).collect();

                // convert LValue?
                let reentry = (self.lvalue(&reentry.0), reentry.1);

                // create function call
                self.xfmr
                    .term_call_fn(term.span(), target, args, reentry)
                    .unwrap()
            }
        }
    }

    /// Call the [`Transformer`] on a statement
    fn statement(&mut self, stm: &Statement) {
        let span = stm.span();

        match stm.kind() {
            StatementKind::Assign(lv, rv) => {
                let lv = self.lvalue(lv);
                let rv = self.rvalue(rv);
                self.xfmr.store(span, lv, rv);
            }
        }
    }

    /// Use the [`Transformer`] to convert a MIR [`RValue`] to the target IR value type `V`
    fn rvalue(&mut self, rv: &RValue) -> V {
        match rv {
            RValue::Use(o) => self.operand(o),
            RValue::BinOp(op, l, r) => {
                let lv = self.operand(l);
                let rv = self.operand(r);
                match op {
                    BinOp::Add => self.xfmr.i_add(lv, rv),
                    BinOp::Sub => self.xfmr.i_sub(lv, rv),
                    BinOp::Mul => self.xfmr.i_mul(lv, rv),
                    BinOp::SIDiv => self.xfmr.si_div(lv, rv),
                    BinOp::UIDiv => self.xfmr.ui_div(lv, rv),
                    BinOp::Eq => self.xfmr.i_eq(lv, rv),
                    BinOp::Ne => self.xfmr.i_neq(lv, rv),
                    BinOp::SILe => self.xfmr.si_lte(lv, rv),
                    BinOp::UILe => self.xfmr.ui_lte(lv, rv),
                    BinOp::SILt => self.xfmr.si_lt(lv, rv),
                    BinOp::UILt => self.xfmr.ui_lt(lv, rv),
                    BinOp::SIGe => self.xfmr.si_gte(lv, rv),
                    BinOp::UIGe => self.xfmr.ui_gte(lv, rv),
                    BinOp::SIGt => self.xfmr.si_gt(lv, rv),
                    BinOp::UIGt => self.xfmr.ui_gt(lv, rv),
                    BinOp::And => self.xfmr.i_and(lv, rv),
                    BinOp::Or => self.xfmr.i_or(lv, rv),
                    BinOp::RawPointerOffset => self.xfmr.pointer_offset(lv, rv),
                    BinOp::FAdd => self.xfmr.f_add(lv, rv),
                    BinOp::FSub => self.xfmr.f_sub(lv, rv),
                    BinOp::FMul => self.xfmr.f_mul(lv, rv),
                    BinOp::FDiv => self.xfmr.f_div(lv, rv),
                    BinOp::FEq => self.xfmr.f_eq(lv, rv),
                    BinOp::FNe => self.xfmr.f_neq(lv, rv),
                    BinOp::FLe => self.xfmr.f_lte(lv, rv),
                    BinOp::FLt => self.xfmr.f_lt(lv, rv),
                    BinOp::FGe => self.xfmr.f_gte(lv, rv),
                    BinOp::FGt => self.xfmr.f_gt(lv, rv),
                }
                .unwrap()
            }
            RValue::UnOp(op, v) => {
                let v = self.operand(v);
                match op {
                    UnOp::Negate => self.xfmr.i_neg(v),
                    UnOp::FNegate => self.xfmr.f_neg(v),
                    UnOp::Not => self.xfmr.i_not(v),
                }
                .unwrap()
            }
            RValue::Cast(o, oty, target) => {
                let l = self.operand(o);
                // is operand signed
                let l_signed = self.mir.is_signed(*oty);
                let l_sz = self.mir.width(*oty).unwrap();
                // is target type signed
                let ty_signed = self.mir.is_signed(*target);
                let ty_sz = self.mir.width(*target).unwrap();
                self.xfmr
                    .cast(l, l_signed, l_sz, *target, ty_signed, ty_sz)
                    .unwrap()
            }
            RValue::AddressOf(t) => {
                let l = self.lvalue(t);
                self.xfmr.address_of(l).unwrap()
            }
        }
    }

    /// Use the [`Transformer`] to convert a Mir [`Operand`] to the target IR value type `V`
    fn operand(&mut self, o: &Operand) -> V {
        match o {
            Operand::Constant(c) => self.constant(*c),
            Operand::LValue(lv) => {
                let l = self.lvalue(lv);
                self.xfmr.load(l).unwrap()
            }
        }
    }

    fn constant(&mut self, c: Constant) -> V {
        match c {
            Constant::Unit => todo!(),
            Constant::I8(i) => self.xfmr.const_i8(i),
            Constant::I16(i) => self.xfmr.const_i16(i),
            Constant::I32(i) => self.xfmr.const_i32(i),
            Constant::I64(i) => self.xfmr.const_i64(i),
            Constant::U8(u) => self.xfmr.const_u8(u),
            Constant::U16(u) => self.xfmr.const_u16(u),
            Constant::U32(u) => self.xfmr.const_u32(u),
            Constant::U64(u) => self.xfmr.const_u64(u),
            Constant::F64(f) => self.xfmr.const_f64(f),
            Constant::Bool(b) => self.xfmr.const_bool(b),
            Constant::StringLiteral(s) => self.xfmr.string_literal(
                self.mir
                    .get_def_string(s)
                    .expect("DefId must refer to a static string literal"),
            ),
            Constant::Null => todo!(),
            Constant::SizeOf(_) => todo!(),
        }
    }

    // TODO: return a result and move the error handling up
    fn lvalue(&mut self, lv: &LValue) -> L {
        match lv {
            LValue::Static(sid) => self.xfmr.static_loc(*sid).unwrap(),
            LValue::Var(vid) => self.xfmr.var(*vid).unwrap(),
            LValue::Temp(tid) => self.xfmr.temp(*tid).unwrap(),
            LValue::Access(loc, method) => self.accessor(loc, method),
            LValue::ReturnPointer => self.xfmr.return_ptr().unwrap(),
        }
    }

    fn accessor(&mut self, loc: &LValue, method: &Accessor) -> L {
        match method {
            Accessor::Index(idx) => {
                let loc = self.lvalue(loc);
                let idx = self.operand(idx);
                self.xfmr.array_access(loc, idx).unwrap()
            }
            Accessor::Field(field, _) => {
                let loc = self.lvalue(loc);
                self.xfmr.field_access(loc, *field).unwrap()
            }
            Accessor::Deref => {
                let loc = self.lvalue(loc);
                self.xfmr.deref(loc).unwrap()
            }
        }
    }

    fn get_varid_iter(&self) -> impl Iterator<Item = VarId> {
        self.function.varid_iter()
    }

    /// Given a [`VarId`] this will find the associated [`VarDecl`] from the currently
    /// transforming [`Procedure`].  If no [`Procedure`] is currently being transformed this
    /// will panic.
    fn get_var(&self, id: VarId) -> &VarDecl {
        self.function.get_var(id)
    }

    fn get_tempid_iter(&self) -> impl Iterator<Item = TempId> {
        self.function.tempid_iter()
    }

    fn get_temp(&self, id: TempId) -> &TempDecl {
        self.function.get_temp(id)
    }
}
