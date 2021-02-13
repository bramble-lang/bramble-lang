use crate::{ast::node::Node, compiler::memory::register_assigner};
use std::collections::HashMap;

// ASM - types capturing the different assembly instructions along with functions to
// convert to text so that a compiled program can be saves as a file of assembly
// instructions
use crate::ast::routinedef::RoutineDefType;
use crate::compiler::memory::layout::compute_layout_for_program;
use crate::compiler::memory::scope::CompilerAnnotation;
use crate::compiler::memory::scope::Level::Routine;
use crate::compiler::memory::stack::CompilerAnnotationStack;
use crate::compiler::memory::stringpool::StringPool;
use crate::compiler::x86::assembly::*;
use crate::operand;
use crate::register;
use crate::unary_op;
use crate::unit_op;
use crate::TracingConfig;
use crate::{assembly, ast::path::Path};
use crate::{
    assembly2,
    ast::{
        module::{Item, Module},
        routinedef::RoutineDef,
    },
};
use crate::{ast::statement::Statement, expression::RoutineCall};
use crate::{
    ast::statement::{Bind, Mutate, Return, Yield, YieldReturn},
    binary_op,
};
use crate::{
    ast::ty::Type,
    expression::{BinaryOperator, UnaryOperator},
};
use crate::{expression::Expression, semantics::semanticnode::SemanticAnnotations};

use super::memory::{struct_definition::FieldInfo, struct_table::ResolvedStructTable};

// Coroutine entry/return annotations: offsets relative to the coroutine's RBP
// These live within the stack frame of the coroutine
const COROUTINE_RIP_STORE: i32 = -8;
const COROUTINE_CALLER_RSP_STORE: i32 = -16;
const COROUTINE_CALLER_RBP_STORE: i32 = -24;
const COROUTINE_CALLER_RIP_STORE: i32 = -32;
const COROUTINE_RSP_STORE: i32 = -40;

// How much space to allocate for each coroutine's stack
const COROUTINE_STACK_SIZE: i64 = 8 * 1024;

// Function entry/return annotations: offsets relative to RBP
// These live above the function's stack frame (hence they are positive)
const FUNCTION_CALLER_RSP: i32 = 16;

pub enum TargetOS {
    Linux,
    MacOS,
}

impl From<&str> for TargetOS {
    fn from(s: &str) -> Self {
        let s = s.to_lowercase();
        match s.as_str() {
            "linux" => TargetOS::Linux,
            "machos" => TargetOS::MacOS,
            _ => panic!("Invalid target platform: {}", s),
        }
    }
}

pub struct Compiler<'a> {
    scope: CompilerAnnotationStack<'a>,
    string_pool: StringPool,
    type_table: &'a ResolvedStructTable,
    root: &'a Module<CompilerAnnotation>,
    c_extern_functions: HashMap<String, String>,
    imported_functions: Vec<Path>,
}

impl<'a> Compiler<'a> {
    pub fn compile(
        module: Module<SemanticAnnotations>,
        imported_functions: Vec<Path>,
        target_os: TargetOS,
        trace_reg_assigner: TracingConfig,
    ) -> Vec<Inst> {
        // Put user code here
        let (mut compiler_ast, struct_table) = compute_layout_for_program(&module).unwrap();

        let mut string_pool = StringPool::new();
        string_pool.extract_from_module(&compiler_ast);

        // assign register sizes
        register_assigner::assign(trace_reg_assigner, &mut compiler_ast, &struct_table);

        let c_extern_functions = Compiler::configure_c_extern_functions(target_os);

        let mut compiler = Compiler {
            scope: CompilerAnnotationStack::new(),
            string_pool,
            root: &compiler_ast,
            type_table: &struct_table,
            c_extern_functions,
            imported_functions,
        };

        let mut code = vec![];
        compiler.create_base(&mut code);
        compiler.coroutine_init("next_stack_addr", "stack_size", &mut code);
        compiler.runtime_yield_into_coroutine(&mut code);
        compiler.runtime_yield_return(&mut code);

        // Configure the names for functions which will be called by the system
        // and external functions that will be called by this code

        let global_func = "".into();
        compiler
            .traverse_module(&compiler_ast, &global_func, &mut code)
            .unwrap();
        code
    }

    /// Creates the runtime code that will manage the entire execution of this program.
    fn create_base(&self, code: &mut Vec<Inst>) {
        let main_label = self.c_extern_functions["main"].clone();
        assembly! {
            (code) {
                {{self.write_includes()}}
                {{self.write_data_section()}}

                section ".text";
                global {main_label};
                @{main_label}:
                    push %rbp;
                    mov %rbp, %rsp;
                    mov %rax, %rsp;
                    sub %rax, [@stack_size];
                    mov [@next_stack_addr], %rax;

                    call @root_my_main;

                    mov %rsp, %rbp;
                    pop %rbp;
                    ret;
            }
        };
    }

    fn write_includes(&self) -> Vec<Inst> {
        let mut code = vec![];
        for (_, platform_name) in self.c_extern_functions.iter() {
            code.push(Inst::Extern(platform_name.clone()));
        }
        for imported_function in self.imported_functions.iter() {
            code.push(Inst::Extern(imported_function.to_label()));
        }
        code
    }

    fn write_data_section(&self) -> Vec<Inst> {
        let mut code = vec![];
        assembly! {
            (code) {
                section ".data";
                data stdout: dq 1;
                data next_stack_addr: dq 0;
                data stack_size: dq {COROUTINE_STACK_SIZE};
                {{self.write_string_pool()}}
            }
        };

        code
    }

    fn write_string_pool(&self) -> Vec<Inst> {
        let mut code = vec![];

        code.push(Inst::DataString("_i32_fmt".into(), "%ld\\n".into()));
        code.push(Inst::DataString("_str_fmt".into(), "%s".into()));
        code.push(Inst::DataString("_true".into(), "true\\n".into()));
        code.push(Inst::DataString("_false".into(), "false\\n".into()));

        for (s, id) in self.string_pool.pool.iter() {
            let lbl = format!("str_{}", id);
            code.push(Inst::DataString(lbl, s.clone()));
        }
        code
    }

    /// Writes the function which will handle initializing a new coroutine
    fn coroutine_init(
        &self,
        next_stack_variable: &str,
        stack_increment_variable: &str,
        code: &mut Vec<Inst>,
    ) {
        /*
         * Input:
         * EAX - address of the coroutine's instructions
         * EDI - number of bytes to allocate into the stack frame (including coroutine meta data)
         *
         * Output:
         * EAX - address of the new coroutine instance
         *
         * Parameter: the IP to the coroutine's actual code.  Along with any init parameters
         * to be passed to the coroutine.
         *
         * Returns a pointer to the new coroutine stack (which contains the coroutine's
         * annotations)
         *
         * use the next stack address variable as the location for the new coroutine, then
         * increment the next stack address.
         *
         * Store the entry address
         * Compute the initial stack frame: allocating space for meta data, to store the
         * esp/ebp, and to store initial parameters.
         *
         * Store esp/ebp
         * Store the initial parameters.
         *
         * Move the address of the stack into EAX and return
         */

        assembly! {
            (code) {
                @runtime_init_coroutine:
                    push %rbp;
                    mov %rbp, %rsp;
                    mov %rsp, [@{next_stack_variable}];
                    ; {format!("[{}]: The RIP for the coroutine", COROUTINE_RIP_STORE)}
                    ; {format!("[{}]: The RSP for the caller", COROUTINE_CALLER_RSP_STORE)}
                    ; {format!("[{}]: The RBP for the caller", COROUTINE_CALLER_RBP_STORE)}
                    ; {format!("[{}]: The caller return address (for yield return)", COROUTINE_CALLER_RIP_STORE)}
                    ; {format!("[{}]: The coroutine RSP", COROUTINE_RSP_STORE)}
                    mov [%rsp+{COROUTINE_RIP_STORE}], %rax;
                    mov [%rsp+{COROUTINE_CALLER_RSP_STORE}], 0u64;
                    mov [%rsp+{COROUTINE_CALLER_RBP_STORE}], 0u64;
                    mov [%rsp+{COROUTINE_CALLER_RIP_STORE}], 0u64;
                    mov %rax, %rsp;
                    sub %rax, %rdi;
                    mov [%rsp+{COROUTINE_RSP_STORE}], %rax;
                    mov %rax, %rsp;
                    sub %rsp, [@{stack_increment_variable}];
                    mov [@{next_stack_variable}], %rsp;
                    mov %rsp, %rbp;
                    pop %rbp;
                    ret;
            }
        }
    }

    fn runtime_yield_into_coroutine(&self, code: &mut Vec<Inst>) {
        /*
         * Input:
         * EAX - address of the coroutine instance
         * EBX - address of the return point
         */
        assembly! {
            (code) {
                @runtime_yield_into_coroutine:
                    mov [%rax+{COROUTINE_CALLER_RSP_STORE}], %rsp;
                    mov [%rax+{COROUTINE_CALLER_RBP_STORE}], %rbp;
                    mov [%rax+{COROUTINE_CALLER_RIP_STORE}], %rbx;
                    mov %rbp, %rax;
                    mov %rsp, [%rbp+{COROUTINE_RSP_STORE}];
                    jmp [%rbp+{COROUTINE_RIP_STORE}];
            }
        }
    }

    fn runtime_yield_return(&self, code: &mut Vec<Inst>) {
        /*
         * Input:
         * EAX - value being returned (if any)
         * EBX - re-entry address
         */
        // When in a coroutine, return to the calling coroutine
        assembly! {
            (code) {
                @runtime_yield_return:
                    mov [%rbp+{COROUTINE_RSP_STORE}], %rsp;
                    mov [%rbp+{COROUTINE_RIP_STORE}], %rbx;
                    mov %rsp, [%rbp+{COROUTINE_CALLER_RSP_STORE}];
                    mov %rbx, [%rbp+{COROUTINE_CALLER_RIP_STORE}];
                    mov %rbp, [%rbp+{COROUTINE_CALLER_RBP_STORE}];
                    jmp %rbx;
            }
        }
    }

    fn handle_binary_operands(
        &mut self,
        op: BinaryOperator,
        left: &'a Expression<CompilerAnnotation>,
        right: &'a Expression<CompilerAnnotation>,
        current_func: &String,
    ) -> Result<Vec<Inst>, String> {
        use BinaryOperator::*;

        let primary = left
            .annotation()
            .scale_reg(Reg64::Rax)
            .expect("Register could not be found for expression");

        let secondary = right
            .annotation()
            .scale_reg(Reg64::Rbx)
            .expect("Register could not be found for expression");

        let mut left_code = vec![];
        self.traverse_expression(left, current_func, &mut left_code)?;
        let mut right_code = vec![];
        self.traverse_expression(right, current_func, &mut right_code)?;

        let la = left.annotation();
        let l_offset = self
            .get_expression_offset(la)
            .expect("Must be an offset for the left operand");
        self.insert_comment_from_annotations(&mut left_code, &left.to_string(), la);
        assembly! {(left_code){
            mov [%rbp-{l_offset}], %{primary};
        }};

        let ra = right.annotation();
        let r_offset = self
            .get_expression_offset(ra)
            .expect("Must be an offset for the right operand");
        self.insert_comment_from_annotations(&mut right_code, &right.to_string(), ra);
        assembly! {(right_code){
            mov [%rbp-{r_offset}], %{primary};
        }};

        let mut op_asm = vec![];
        match op {
            BinaryOperator::Add => {
                assembly! {(op_asm) {add %{primary}, %{secondary};}}
            }
            BinaryOperator::Sub => {
                assembly! {(op_asm) {sub %{primary}, %{secondary};}}
            }
            BinaryOperator::Mul => {
                assembly! {(op_asm) {imul %{primary}, %{secondary};}}
            }
            BinaryOperator::Div => {
                assembly! {(op_asm) {
                    cdq;
                    idiv %{secondary};
                }}
            }
            BinaryOperator::BAnd => {
                assembly! {(op_asm) {and %{primary}, %{secondary};}}
            }
            BinaryOperator::BOr => {
                assembly! {(op_asm) {or %{primary}, %{secondary};}}
            }
            cond => {
                let set = match cond {
                    Eq => Inst::Sete(Reg8::Al),
                    NEq => Inst::Setne(Reg8::Al),
                    Ls => Inst::Setl(Reg8::Al),
                    LsEq => Inst::Setle(Reg8::Al),
                    Gr => Inst::Setg(Reg8::Al),
                    GrEq => Inst::Setge(Reg8::Al),
                    _ => panic!("Invalid conditional operator: {}", cond),
                };
                assembly! {(op_asm){
                    cmp %{primary}, %{secondary};
                    {{[set]}}
                    and %al, 1;
                    movzx %rax, %al;
                }}
            }
        };

        let mut code = vec![];
        assembly! {(code){
            {{left_code}}
            {{right_code}}
            mov %{primary}, [%rbp - {l_offset}];
            mov %{secondary}, [%rbp - {r_offset}];
            {{op_asm}}
        }}

        Ok(code)
    }

    fn push_scope(&mut self, ast: &'a Expression<CompilerAnnotation>) {
        self.scope.push(ast.annotation())
    }

    fn pop(&mut self) {
        self.scope.pop();
    }

    fn traverse_expression(
        &mut self,
        ast: &'a Expression<CompilerAnnotation>,
        current_func: &String,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        // The registers used for passing function parameters, in the order that parameters are
        // assigned to registers
        let fn_param_registers = vec![Reg64::Rax, Reg64::Rbx, Reg64::Rcx, Reg64::Rdx];
        let co_param_registers = vec![Reg64::Rbx, Reg64::Rcx, Reg64::Rdx];

        self.push_scope(ast);
        match ast {
            Expression::Integer32(_, i) => {
                assembly! {(code) {mov %eax, {*i};}}
            }
            Expression::Integer64(_, i) => {
                assembly! {(code) {mov %rax, {*i};}}
            }
            Expression::Boolean(_, b) => {
                assembly! {(code) {mov %rax, {if *b {1} else {0}};}}
            }
            Expression::StringLiteral(_, s) => {
                let str_id = self
                    .string_pool
                    .get(s)
                    .ok_or(format!("Could not find string {} in string pool", s))?;
                assembly! {(code) {
                        lea %rax, [@{format!("str_{}", str_id)}];
                    }
                }
            }
            Expression::Identifier(m, id) => {
                let reg = ast
                    .annotation()
                    .scale_reg(Reg64::Rax)
                    .expect("Expect a register to be assigned to an identifier");
                let id_offset = self.scope.find(id).unwrap().offset;
                match m.ty() {
                    Type::Custom(_) => {
                        assembly! {(code) {lea %rax, [%rbp-{id_offset}];}}
                    }
                    _ => {
                        assembly! {(code) {mov %{reg}, [%rbp-{id_offset}];}}
                    }
                }
            }
            Expression::MemberAccess(_, src, member) => {
                self.member_access(current_func, code, src, member)?;
            }
            Expression::UnaryOp(_, op, operand) => {
                self.traverse_expression(operand, current_func, code)?;
                match op {
                    UnaryOperator::Minus => {
                        let reg = ast
                            .annotation()
                            .scale_reg(Reg64::Rax)
                            .unwrap_or(Reg::R64(Reg64::Rax));
                        assembly! {(code){
                            neg %{reg};
                        }}
                    }
                    UnaryOperator::Not => {
                        assembly! {(code){
                            xor %rax, 1;
                            movzx %rax, %al;
                        }}
                    }
                }
            }
            Expression::BinaryOp(_, op, l, r) => {
                assembly! {(code) {
                    {{self.handle_binary_operands(*op, l.as_ref(), r.as_ref(), current_func)?}}
                }}
            }
            Expression::If {
                annotation: meta,
                cond,
                if_arm,
                else_arm,
            } => {
                let mut cond_code = vec![];
                self.traverse_expression(cond, current_func, &mut cond_code)?;
                let mut if_code = vec![];
                self.traverse_expression(if_arm, current_func, &mut if_code)?;
                let mut else_code = vec![];
                if let Some(else_arm) = else_arm {
                    self.traverse_expression(else_arm, current_func, &mut else_code)?;
                }

                assembly2! {(code, meta) {
                    {{cond_code}}
                    cmp %rax, 0;
                    jz ^else_lbl;
                    {{if_code}}
                    jmp ^end_lbl;
                ^else_lbl:
                    {{else_code}}
                ^end_lbl:
                }};
            }
            Expression::ExpressionBlock(_, body, final_exp) => {
                for s in body.iter() {
                    self.traverse_statement(s, current_func, code)?;
                }
                match final_exp {
                    None => (),
                    Some(fe) => self.traverse_expression(fe, current_func, code)?,
                }
            }
            Expression::Yield(meta, ref id) => {
                assembly! {(code) {
                    {{self.yield_exp(meta, id, current_func)?}}
                }}
            }
            Expression::RoutineCall(_, RoutineCall::CoroutineInit, ref co_path, params) => {
                let co_def = self
                    .root
                    .go_to(co_path)
                    .expect("Could not find coroutine")
                    .to_routine()
                    .expect("Expected a routine");
                let total_offset = co_def.total_allocation();

                co_def.validate_parameters(params)?;

                assembly! {(code) {
                    ; {format!("Call {}", co_path)}
                    {{self.evaluate_routine_call_params(params, current_func)?}}
                    {{self.move_routine_call_params_into_registers(params, &co_param_registers)?}}
                    ; "Load the IP for the coroutine (EAX) and the stack frame allocation (EDI)"
                    lea %rax, [@{co_path.to_label()}];
                    mov %rdi, {total_offset as i64};
                    call @runtime_init_coroutine;
                    ; "move into coroutine's stack frame"
                    push %rbp;
                    mov %rbp, %rax;

                    ; "move parameters into the stack frame of the coroutine"
                    {{{
                        self.move_params_into_stackframe(co_def, &co_param_registers)?
                    }}}
                    ; "leave coroutine's stack frame"
                    pop %rbp;
                }};
            }
            Expression::RoutineCall(meta, RoutineCall::Function, ref fn_path, params) => {
                let return_type = meta.ty();
                if let Type::Custom(_) = return_type {
                    let st_sz = self
                        .type_table
                        .size_of(return_type)
                        .ok_or(format!("no size for {} found", return_type))?;

                    assembly! {(code){
                        sub %rsp, {st_sz as i64};
                    }}
                }

                // evaluate each paramater then store in registers Eax, Ebx, Ecx, Edx before
                // calling the function
                assembly! {(code) {
                    ; {{format!("Call {}", fn_path)}}
                    {{self.evaluate_routine_call_params(params, current_func)?}}
                    {{self.move_routine_call_params_into_registers(params, &fn_param_registers)?}}
                    call @{fn_path.to_label()};
                }};
            }
            Expression::StructExpression(meta, struct_name, fields) => {
                let anonymous_name = format!("!{}_{}", struct_name, meta.id());

                let anonymous_offset = self
                    .scope
                    .find(&anonymous_name)
                    .expect(&format!(
                        "Anonymous Struct Expression, {}, is not in symbol table: \n{}",
                        anonymous_name, self.scope,
                    ))
                    .offset;
                let asm = self.struct_exression(
                    current_func,
                    struct_name,
                    fields,
                    anonymous_offset,
                    false,
                )?;
                assembly! {(code){
                    {{asm}}
                }};
            }
            node => panic!("Expected an operator, found {:?}", node),
        };

        self.pop();

        Ok(())
    }

    fn traverse_module(
        &mut self,
        module: &'a Module<CompilerAnnotation>,
        current_func: &String,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        self.scope.push(module.annotation());
        for f in module.get_functions().iter() {
            self.traverse_item(f, code)?;
        }
        for co in module.get_coroutines().iter() {
            self.traverse_item(co, code)?;
        }
        for m in module.get_modules().iter() {
            self.traverse_module(m, current_func, code)?;
        }
        self.pop();

        Ok(())
    }

    fn traverse_item(
        &mut self,
        item: &'a Item<CompilerAnnotation>,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        self.scope.push(item.annotation());
        let result = match item {
            Item::Struct(_) => {
                panic!("StructDefs should have been pruned from the AST before the compiler layer")
            }
            Item::Routine(r) => self.traverse_routine_def(r, code),
        };
        self.pop();
        result
    }

    fn traverse_routine_def(
        &mut self,
        routine: &'a RoutineDef<CompilerAnnotation>,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        match routine.get_def() {
            RoutineDefType::Function => self.traverse_function_def(routine, code),
            RoutineDefType::Coroutine => self.traverse_coroutine_def(routine, code),
        }
    }

    fn traverse_function_def(
        &mut self,
        routine: &'a RoutineDef<CompilerAnnotation>,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        let fn_param_registers = vec![Reg64::Rax, Reg64::Rbx, Reg64::Rcx, Reg64::Rdx];

        if let RoutineDef {
            annotations: scope,
            def: RoutineDefType::Function,
            name: ref fn_name,
            body: stmts,
            ..
        } = routine
        {
            let total_offset = routine.total_allocation();

            assembly! {(code) {
            @{scope.canon_path().to_label()}:
                ; {{format!("Define {}", scope.canon_path())}}
                ;"Prepare stack frame for this function"
                push %rbp;
                mov %rbp, %rsp;
                sub %rsp, {total_offset as i64};
                ; "Move function parameters from registers into the stack frame"
                {{self.move_params_into_stackframe(routine, &fn_param_registers)?}}
                ; "Done moving function parameters from registers into the stack frame"
            }};

            for s in stmts.iter() {
                self.traverse_statement(s, fn_name, code)?;
            }

            assembly! {(code) {
                ; "Clean up frame before leaving function"
                mov %rsp, %rbp;
                pop %rbp;
                ret;
            }};
        } else {
            panic!("")
        }
        Ok(())
    }

    fn traverse_coroutine_def(
        &mut self,
        routine: &'a RoutineDef<CompilerAnnotation>,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        assembly! {(code) {
            @{routine.annotation().canon_path().to_label()}:
            ; {{format!("Define {}", routine.annotation().canon_path())}}
        }};

        // Prepare stack frame for this function
        let name = routine.get_name().into();
        for s in routine.get_body().iter() {
            self.traverse_statement(s, &name, code)?;
        }
        assembly! {(code) {
            mov %rbx, ^terminus;
            jmp @runtime_yield_return;
        }};
        Ok(())
    }

    fn traverse_statement(
        &mut self,
        statement: &'a Statement<CompilerAnnotation>,
        current_func: &String,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        match statement {
            Statement::Bind(b) => self.traverse_bind(b, current_func, code),
            Statement::Mutate(m) => self.traverse_mutate(m, current_func, code),
            Statement::Return(n) => self.traverse_return(n, current_func, code),
            Statement::YieldReturn(n) => self.traverse_yieldreturn(n, current_func, code),
            Statement::Expression(n) => self.traverse_expression(n, current_func, code),
        }
    }

    fn traverse_bind(
        &mut self,
        bind: &'a Bind<CompilerAnnotation>,
        current_func: &String,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        let id_offset = self
            .scope
            .find(bind.get_id())
            .expect(&format!(
                "Could not find variable {}\n{}",
                bind.get_id(),
                self.scope
            ))
            .offset;
        assembly! {(code) {
            ; {format!("Binding {}", bind.get_id())}
            {{self.bind(bind.get_rhs(), current_func, Reg64::Rbp, id_offset)?}}
        }}
        Ok(())
    }

    fn traverse_mutate(
        &mut self,
        mutate: &'a Mutate<CompilerAnnotation>,
        current_func: &String,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        let id = mutate.get_id();
        let id_offset = self
            .scope
            .find(id)
            .expect(&format!("Could not find variable {}\n{}", id, self.scope))
            .offset;
        assembly! {(code) {
            ; {format!("Binding {}", id)}
            {{self.bind(mutate.get_rhs(), current_func, Reg64::Rbp, id_offset)?}}
        }}
        Ok(())
    }

    fn traverse_yield(
        &mut self,
        y: &'a Yield<CompilerAnnotation>,
        current_func: &String,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        assembly! {(code) {
            {{self.yield_exp(y.annotation(), y.get_value(), current_func)?}}
        }}
        Ok(())
    }

    fn traverse_yieldreturn(
        &mut self,
        yr: &'a YieldReturn<CompilerAnnotation>,
        current_func: &String,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        assembly! {(code) {
            {{self.yield_return(yr.annotation(), yr.get_value(), current_func)?}}
        }}
        Ok(())
    }

    fn traverse_return(
        &mut self,
        r: &'a Return<CompilerAnnotation>,
        current_func: &String,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        assembly! {(code) {
            {{self.return_exp(r.get_value(), current_func)?}}
        }}
        Ok(())
    }

    fn member_access(
        &mut self,
        current_func: &String,
        code: &mut Vec<Inst>,
        src: &'a Expression<CompilerAnnotation>,
        member: &str,
    ) -> Result<(), String> {
        let src_annotations = src.annotation();
        let src_ty = src_annotations.ty();
        match src_ty {
            Type::Custom(struct_name) => {
                code.push(Inst::Comment(format!("{}.{}", struct_name, member)));

                self.traverse_expression(src, current_func, code)?;

                let struct_def = self.type_table.get(struct_name).ok_or(format!(
                    "Could not find struct definition for {}",
                    struct_name
                ))?;
                let field_info = struct_def
                    .get_fields()
                    .iter()
                    .find(|FieldInfo { name, .. }| name == member)
                    .ok_or(format!("Member {} not found on {}", member, struct_name))?;
                let field_offset = struct_def.get_offset_of(member).ok_or(format!(
                    "No field offset found for {}.{}",
                    struct_name, member
                ))?;

                match &field_info.ty() {
                    Type::Custom(_substruct_name) => {
                        assembly! {(code) {
                            lea %rax, [%rax+{field_offset}];
                        }}
                    }
                    ty => {
                        let reg_sz = register_assigner::register_size_for_type(ty, self.type_table)
                            .expect("There must be a size for a struct field");
                        let reg = Reg64::Rax
                            .scale(reg_sz)
                            .expect("Cannot scale a register to this field");
                        assembly! {(code) {
                            mov %{reg}, [%rax+{field_offset}];
                        }}
                    }
                }
            }
            _ => {
                return Err(format!(
                    "Attempting to access a member, {}, on a type which has no members: {}",
                    member, src_ty
                ))
            }
        }

        Ok(())
    }

    fn struct_exression(
        &mut self,
        current_func: &String,
        struct_name: &Path,
        field_values: &'a Vec<(String, Expression<CompilerAnnotation>)>,
        offset: i32,
        allocate: bool,
    ) -> Result<Vec<Inst>, String> {
        let struct_def = self.type_table.get(struct_name).expect(&format!(
            "{}, used in {}, was not found",
            struct_name, current_func
        ));
        let struct_sz = struct_def
            .size
            .expect(&format!("Size is not known for {}", struct_name));
        let field_info = struct_def
            .get_fields()
            .iter()
            .map(|FieldInfo { name, offset, .. }| (name.clone(), offset.unwrap()))
            .collect::<Vec<(String, i32)>>();

        if field_values.len() != field_info.len() {
            return Err(format!(
                "{} expected {} fields but found {}",
                struct_name,
                struct_def.get_fields().len(),
                field_values.len()
            ));
        }

        let mut code = vec![];
        code.push(Inst::Comment(format!(
            "Instantiate struct of type {} (Offset: {})",
            struct_name, offset,
        )));
        if allocate {
            assembly! {(code){
                sub %rsp, {struct_sz as i64};
            }};
        }
        for (fname, fvalue) in field_values.iter() {
            let field_offset = field_info.iter().find(|(n, _)| n == fname).unwrap().1;
            let relative_offset = offset - (struct_sz - field_offset);
            assembly! {(code) {
                {{self.bind_member(fvalue, current_func, Reg64::Rbp, relative_offset)?}}
            }}
        }

        assembly! {(code) {
            ; {format!("Done instantiating struct of type {}", struct_name)}
            lea %rax, [%rbp - {offset}];
        }};
        Ok(code)
    }

    fn bind_member(
        &mut self,
        fvalue: &'a Expression<CompilerAnnotation>,
        current_func: &String,
        dst: Reg64,
        dst_offset: i32,
    ) -> Result<Vec<Inst>, String> {
        let mut code = vec![];

        match fvalue {
            Expression::StructExpression(_, substruct_name, substruct_values) => {
                let asm = self.struct_exression(
                    current_func,
                    substruct_name,
                    substruct_values,
                    dst_offset,
                    false,
                )?;
                assembly! {(code){
                    {{asm}}
                }};
            }
            _ => {
                self.traverse_expression(fvalue, current_func, &mut code)?;
                match fvalue.annotation().ty() {
                    Type::Custom(struct_name) => {
                        let asm =
                            self.copy_struct_into(struct_name, dst, dst_offset, Reg64::Rax, 0)?;
                        assembly! {(code){
                            {{asm}}
                        }};
                    }
                    _ => {
                        let reg = fvalue
                            .annotation()
                            .scale_reg(Reg64::Rax)
                            .expect("Could not scale register");
                        assembly! {(code) {
                            mov [%{Reg::R64(dst)}-{dst_offset}], %{reg};
                        }};
                    }
                }
            }
        }
        Ok(code)
    }

    fn bind(
        &mut self,
        value: &'a Expression<CompilerAnnotation>,
        current_func: &String,
        dst: Reg64,
        dst_offset: i32,
    ) -> Result<Vec<Inst>, String> {
        let mut code = vec![];
        self.traverse_expression(value, current_func, &mut code)?;

        match value.annotation().ty() {
            Type::Custom(name) => {
                match value {
                    Expression::Identifier(..) => {
                        // If an identifier is being copied to another identifier, then just copy
                        // the data over rather than pop off of the stack
                        let asm = self.copy_struct_into(name, dst, dst_offset, Reg64::Rax, 0)?;
                        assembly! {(code){
                            {{asm}}
                        }}
                    }
                    _ => {
                        assembly! {(code){
                            // TODO: Double check the 0 here:
                            {{self.copy_struct_into(name, dst, dst_offset, Reg64::Rax, 0)?}}
                        }}
                    }
                }
            }
            _ => {
                let reg = value
                    .annotation()
                    .scale_reg(Reg64::Rax)
                    .expect("Register size not assigned");

                assembly! {(code) {
                    mov [%{Reg::R64(dst)}-{dst_offset}], %{reg};
                }};
            }
        }
        Ok(code)
    }

    fn yield_exp(
        &mut self,
        meta: &'a CompilerAnnotation,
        exp: &'a Expression<CompilerAnnotation>,
        current_func: &String,
    ) -> Result<Vec<Inst>, String> {
        let mut code = vec![];
        self.traverse_expression(exp, current_func, &mut code)?;
        match meta.ty() {
            Type::Custom(struct_name) => {
                let st = self
                    .type_table
                    .get(struct_name)
                    .ok_or(format!("no definition for {} found", struct_name))?;
                let st_sz = st
                    .size
                    .ok_or(format!("struct {} has no resolved size", struct_name))?;
                assembly! {(code){
                    sub %rsp, {st_sz as i64};
                }}
            }
            _ => (),
        }
        assembly2! {(code, meta) {
            mov %rbx, ^ret_lbl;
            jmp @runtime_yield_into_coroutine;
            ^ret_lbl:
        }};
        Ok(code)
    }

    fn yield_return(
        &mut self,
        meta: &'a CompilerAnnotation,
        exp: &'a Option<Expression<CompilerAnnotation>>,
        current_func: &String,
    ) -> Result<Vec<Inst>, String> {
        let mut code = vec![];
        if let Some(exp) = exp {
            self.traverse_expression(exp, current_func, &mut code)?;
            match exp.annotation().ty() {
                Type::Custom(struct_name) => {
                    // Copy the structure into the stack frame of the calling function
                    let asm = self.copy_struct_into(struct_name, Reg64::Rsi, 0, Reg64::Rax, 0)?;
                    assembly! {(code){
                        mov %rsi, [%rbp+{COROUTINE_CALLER_RSP_STORE}];  // Load the caller function's ESP pointer
                        {{asm}}
                    }};
                }
                _ => (),
            }
        }

        assembly2! {(code, meta) {
            mov %rbx, ^ret_lbl;
            jmp @runtime_yield_return;
            ^ret_lbl:
        }};
        Ok(code)
    }

    fn return_exp(
        &mut self,
        exp: &'a Option<Expression<CompilerAnnotation>>,
        current_func: &String,
    ) -> Result<Vec<Inst>, String> {
        let mut code = vec![];
        code.push(Inst::Label(".terminus".into()));
        match exp {
            Some(e) => {
                self.traverse_expression(e, current_func, &mut code)?;
                // If the expression is a custom type then copy the value into the stack frame
                // of the caller (or the yielder in the case of coroutines)
                match e.annotation().ty() {
                    Type::Custom(struct_name) => {
                        // Copy the structure into the stack frame of the calling function
                        let asm =
                            self.copy_struct_into(struct_name, Reg64::Rsi, 0, Reg64::Rax, 0)?;

                        let is_coroutine = self.scope.in_coroutine();
                        if is_coroutine {
                            assembly! {(code){
                                mov %rsi, [%rbp+{COROUTINE_CALLER_RSP_STORE}];  // load the caller function's ESP pointer
                                {{asm}}
                            }};
                        } else {
                            assembly! {(code){
                                lea %rsi, [%rbp+{FUNCTION_CALLER_RSP}]; // load the caller function's ESP pointer
                                {{asm}}
                            }};
                        }
                    }
                    _ => (),
                }
            }
            None => (),
        }

        Ok(code)
    }

    fn copy_struct_into(
        &self,
        struct_name: &Path,
        dst_reg: Reg64,
        dst_offset: i32,
        src_reg: Reg64,
        src_offset: i32,
    ) -> Result<Vec<Inst>, String> {
        let mut code = vec![];
        let ty_def = self
            .type_table
            .get(struct_name)
            .expect(&format!("Could not find definition for {}", struct_name));
        let struct_sz = ty_def
            .size
            .expect(&format!("Struct {} has an unknown size", struct_name));
        for FieldInfo {
            name: field_name,
            ty: field_ty,
            offset: field_offset,
        } in ty_def.get_fields().iter().rev()
        {
            let rel_field_offset = field_offset.expect(&format!(
                "CRITICAL: struct {} has member, {}, with no relative offset",
                struct_name, field_name
            ));
            let dst_field_offset = dst_offset - (struct_sz - rel_field_offset);
            match field_ty {
                Type::Custom(name) => {
                    assembly! {(code){
                        {{self.copy_struct_into(name, dst_reg, dst_field_offset, src_reg, src_offset-(struct_sz - rel_field_offset))?}}
                    }}
                }
                ty => {
                    let reg_sz = register_assigner::register_size_for_type(ty, self.type_table)
                        .expect("There must be a size for a struct field");
                    let reg = Reg64::Rdi
                        .scale(reg_sz)
                        .expect("Cannot scale a register to this field");
                    assembly! {(code) {
                        ; {format!("copy {}.{}", struct_name, field_name)}
                        mov %{reg}, [%{Reg::R64(src_reg)}-{src_offset - (struct_sz - rel_field_offset)}];
                        mov [%{Reg::R64(dst_reg)}-{dst_field_offset}], %{reg};
                    }};
                }
            }
        }

        Ok(code)
    }

    fn move_params_into_stackframe(
        &self,
        routine: &RoutineDef<CompilerAnnotation>,
        param_registers: &Vec<Reg64>,
    ) -> Result<Vec<Inst>, String> {
        let routine_name = routine.get_name();
        let params = routine.get_params();
        if params.len() > param_registers.len() {
            return Err(format!(
                "Compiler: too many parameters in function definition"
            ));
        }

        let mut code = vec![];
        let routine_sym_table = routine.annotation();
        for idx in 0..params.len() {
            let param_symbol = routine_sym_table.get(&params[idx].name).ok_or(format!(
                "Critical: could not find parameter {} in symbol table for {}",
                params[idx].name, routine_name
            ))?;

            // Don't move parameters that have no size
            if param_symbol.size == 0 {
                continue;
            }

            let param_offset = param_symbol.offset;
            match &params[idx].ty {
                Type::Custom(struct_name) => {
                    let asm = self.copy_struct_into(
                        struct_name,
                        Reg64::Rbp,
                        param_offset,
                        param_registers[idx],
                        0,
                    )?;
                    assembly! {(code){
                        {{asm}}
                    }}
                }
                _ => {
                    let param_reg = params[idx]
                        .annotation()
                        .reg_size()
                        .and_then(|sz| param_registers[idx].scale(sz))
                        .expect("Expect a register size to be assigned for a parameter");
                    assembly! {(code){
                        mov [%rbp-{param_offset}], %{param_reg};
                    }};
                }
            }
        }
        Ok(code)
    }

    fn insert_comment_from_annotations(
        &self,
        code: &mut Vec<Inst>,
        label: &str,
        annotation: &CompilerAnnotation,
    ) {
        let offset = self.get_expression_offset(annotation);
        code.push(Inst::Comment(format!(
            "{} [{}] @ {:?}",
            label,
            annotation.anonymous_name(),
            offset
        )));
    }

    fn evaluate_routine_call_params(
        &mut self,
        params: &'a Vec<Expression<CompilerAnnotation>>,
        current_func: &String,
    ) -> Result<Vec<Inst>, String> {
        let mut code = vec![];
        for param in params.iter() {
            self.traverse_expression(param, current_func, &mut code)?;
            let pa = param.annotation();
            self.insert_comment_from_annotations(&mut code, &param.to_string(), pa);
            let reg = pa.scale_reg(Reg64::Rax).expect(&format!(
                "Register could not be found for function parameter: L{}: {}",
                pa.line(),
                param
            ));
            if let Some(offset) = self.get_expression_result_location(pa).expect(&format!(
                "Could not find stack frame location for: L{}: {}",
                pa.line(),
                param
            )) {
                assembly! {(code){
                    mov [%rbp-{offset}], %{reg};
                }};
            }
        }
        Ok(code)
    }

    fn move_routine_call_params_into_registers(
        &mut self,
        params: &'a Vec<Expression<CompilerAnnotation>>,
        param_registers: &Vec<Reg64>,
    ) -> Result<Vec<Inst>, String> {
        // evaluate each paramater then store in registers Eax, Ebx, Ecx, Edx before
        // calling the function
        if params.len() > param_registers.len() {
            return Err(format!(
                "Compiler: too many parameters being passed to function.  Has {} parameters but compiler cannot support more than {}",
                params.len(), param_registers.len(),
            ));
        }

        let mut code = vec![];
        for idx in 0..params.len() {
            let pa = params[idx].annotation();
            if let Some(offset) = self.get_expression_result_location(pa).unwrap() {
                let reg = pa
                    .scale_reg(param_registers[idx])
                    .expect("Could not scale register");
                assembly! {(code){
                    mov %{reg}, [%rbp-{offset}];
                }};
            }
        }
        Ok(code)
    }

    /**
     * If an expression has a value that is stored in the stack frame then this will
     * return the location of that value.
     *
     * Ok(None) => If the expression has not value (e.g. a Unit type or an empty struct)
     * Ok(Some(offset)) => The stack frame offset
     * Err => If the given expression's anonymous name was not found in the symbol table
     */
    fn get_expression_result_location(
        &self,
        pa: &CompilerAnnotation,
    ) -> Result<Option<i32>, String> {
        let size = self
            .get_expression_size(pa)
            .ok_or("Expression could not be found in stack frame, when looking up size")?;

        if size == 0 {
            Ok(None)
        } else {
            let offset = self
                .get_expression_offset(pa)
                .ok_or("Expression could not be found in stack frame, when looking up offset")?;
            Ok(Some(offset))
        }
    }

    fn get_expression_offset(&self, annotation: &CompilerAnnotation) -> Option<i32> {
        let anonymous_name = annotation.anonymous_name();
        self.scope.find(&anonymous_name).map(|s| s.offset)
    }

    fn get_expression_size(&self, annotation: &CompilerAnnotation) -> Option<i32> {
        let anonymous_name = annotation.anonymous_name();
        self.scope.find(&anonymous_name).map(|s| s.size)
    }

    pub fn print(code: &Vec<Inst>, output: &mut dyn std::io::Write) -> std::io::Result<()> {
        for inst in code.iter() {
            writeln!(output, "{}", inst)?;
        }
        Ok(())
    }

    pub fn configure_c_extern_functions(target_os: TargetOS) -> HashMap<String, String> {
        let mut extern_functions = HashMap::new();
        match target_os {
            TargetOS::Linux => {
                extern_functions.insert("main".into(), "main".into());
                extern_functions.insert("printf".into(), "printf".into());
            }
            TargetOS::MacOS => {
                extern_functions.insert("main".into(), "_main".into());
                extern_functions.insert("printf".into(), "_printf".into());
            }
        }

        extern_functions
    }
}

impl<CompilerAnnotation> RoutineDef<CompilerAnnotation> {
    pub fn validate_parameters(
        &self,
        params: &Vec<Expression<CompilerAnnotation>>,
    ) -> braid_lang::result::Result<()> {
        let expected_params = self.get_params();
        if params.len() == expected_params.len() {
            Ok(())
        } else {
            Err(format!(
                "Critical: expected {} but got {} parameters for {}",
                expected_params.len(),
                params.len(),
                self.root_str()
            ))
        }
    }
}
