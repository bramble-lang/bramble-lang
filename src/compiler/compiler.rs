// ASM - types capturing the different assembly instructions along with functions to
// convert to text so that a compiled program can be saves as a file of assembly
// instructions
use crate::compiler::ast::ast::CompilerNode;
use crate::compiler::ast::scope::Level::Routine;
use crate::compiler::ast::scope::Scope;
use crate::compiler::ast::stack::ScopeStack;
use crate::compiler::ast::stringpool::StringPool;
use crate::compiler::x86::assembly::*;
use crate::operand;
use crate::register;
use crate::syntax::routinedef::RoutineDefType;
use crate::unary_op;
use crate::unit_op;
use crate::{assembly, syntax::path::Path};
use crate::{
    assembly2,
    syntax::{
        module::{Item, Module},
        routinedef::RoutineDef,
    },
};
use crate::{expression::Expression, semantics::semanticnode::SemanticMetadata};
use crate::{expression::RoutineCall, syntax::statement::Statement};
use crate::{
    expression::{BinaryOperator, UnaryOperator},
    syntax::ty::Type,
};
use crate::{
    binary_op,
    syntax::statement::{
        Bind, Mutate, Printbln, Printi, Printiln, Prints, Return, Yield, YieldReturn,
    },
};

use super::ast::{struct_definition::FieldInfo, struct_table::ResolvedStructTable};

// Coroutine entry/return metadata: offsets relative to the coroutine's RBP
// These live within the stack frame of the coroutine
const COROUTINE_RIP_STORE:i32 = -8;
const COROUTINE_CALLER_RSP_STORE:i32 = -16;
const COROUTINE_CALLER_RBP_STORE:i32 = -24;
const COROUTINE_CALLER_RIP_STORE:i32 = -32;
const COROUTINE_RSP_STORE:i32 = -40;

// How much space to allocate for each coroutine's stack
const COROUTINE_STACK_SIZE:i64 = 8 * 1024;

// Function entry/return metadata: offsets relative to RBP
// These live above the function's stack frame (hence they are positive)
const FUNCTION_CALLER_RSP:i32 = 16;

pub struct Compiler<'a> {
    code: Vec<Inst>,
    scope: ScopeStack<'a>,
    string_pool: StringPool,
    struct_table: &'a ResolvedStructTable,
    root: &'a Module<Scope>,
}

impl<'a> Compiler<'a> {
    pub fn print(code: &Vec<Inst>, output: &mut dyn std::io::Write) -> std::io::Result<()> {
        for inst in code.iter() {
            writeln!(output, "{}", inst)?;
        }
        Ok(())
    }

    pub fn compile(module: Module<SemanticMetadata>) -> Vec<Inst> {
        // Put user code here
        let (compiler_ast, struct_table) = CompilerNode::from(&module).unwrap();

        let mut string_pool = StringPool::new();
        string_pool.extract_from_module(&compiler_ast);

        let mut code = vec![];
        Compiler::create_base(&mut code, &string_pool);
        Compiler::coroutine_init("next_stack_addr", "stack_size", &mut code);
        Compiler::runtime_yield_into_coroutine(&mut code);
        Compiler::runtime_yield_return(&mut code);
        Compiler::print_bool(&mut code);

        let mut compiler = Compiler {
            code: vec![],
            scope: ScopeStack::new(),
            string_pool,
            root: &compiler_ast,
            struct_table: &struct_table,
        };

        let global_func = "".into();
        compiler
            .traverse_module(&compiler_ast, &global_func, &mut code)
            .unwrap();
        code
    }

    /// Creates the runtime code that will manage the entire execution of this program.
    fn create_base(code: &mut Vec<Inst>, string_pool: &StringPool) {
        assembly! {
            (code) {
                {{Compiler::write_includes()}}
                {{Compiler::write_data_section(&string_pool)}}

                section ".text";
                global main;
                @main:
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

    fn write_includes() -> Vec<Inst> {
        let mut code = vec![];
        code.push(Inst::Extern("printf".into()));
        code.push(Inst::Extern("stdout".into()));
        code.push(Inst::Extern("fputs".into()));
        code
    }

    fn write_data_section(string_pool: &StringPool) -> Vec<Inst> {
        let mut code = vec![];
        assembly! {
            (code) {
                section ".data";
                data next_stack_addr: dq 0;
                data stack_size: dq {COROUTINE_STACK_SIZE};
                {{Compiler::write_string_pool(&string_pool)}}
            }
        };

        code
    }

    fn write_string_pool(string_pool: &StringPool) -> Vec<Inst> {
        let mut code = vec![];

        code.push(Inst::DataString("_i32_fmt".into(), "%ld\\n".into()));
        code.push(Inst::DataString("_true".into(), "true\\n".into()));
        code.push(Inst::DataString("_false".into(), "false\\n".into()));

        for (s, id) in string_pool.pool.iter() {
            let lbl = format!("str_{}", id);
            code.push(Inst::DataString(lbl, s.clone()));
        }
        code
    }

    fn print_bool(code: &mut Vec<Inst>) {
        assembly! {
            (code) {
                @print_bool:
                    push %rbp;
                    mov %rbp, %rsp;
                    cmp %rax, 0;
                    jz ^false;
                    push @_true;
                    jmp ^done;
                    ^false:
                    push @_false;
                    ^done:
                    {{Compiler::make_c64_extern_call("printf", 1)}}
                    mov %rsp, %rbp;
                    pop %rbp;
                    ret;
            }
        }
    }

    fn make_c32_extern_call(c_func: &str, nparams: usize) -> Vec<Inst> {
        let mut code = vec![];
        assembly! {(code){
            {{Compiler::reverse_c32_params_on_stack(nparams)}}
            call @{c_func};
            add %rsp, {4*nparams as i32};
        }}
        code
    }

    fn make_c64_extern_call(c_func: &str, nparams: usize) -> Vec<Inst> {
        let mut code = vec![];
        assembly! {(code){
            {{Compiler::pop_params_to_c64_registers(nparams)}}
            mov %rax, 0;
            call @{c_func};
            mov %rax, 0;
        }}
        code
    }

    fn pop_params_to_c64_registers(nparams: usize) -> Vec<Inst> {
        let mut code = vec![];
        let c64_param_registers = vec![Reg64::Rdi, Reg64::Rsi, Reg64::Rdx];
        let used_registers:Vec<&Reg64> = c64_param_registers[0..nparams].into_iter().rev().collect(); 
        for pl in 0..nparams {
            assembly! {(code){
                pop %{Reg::R64(*used_registers[pl])};
            }}
        }
        code
    }

    /// The GCC32 uses a different order for parameters from the order that
    /// Braid pushes parameters onto the stack as they are evaluated.  This
    /// function reverses the order of the parameters.
    fn reverse_c32_params_on_stack(nparams: usize) -> Vec<Inst> {
        let mut code = vec![];
        for pl in 0..nparams {
            let pr = nparams - pl - 1;
            if pr <= pl {
                break;
            }
            assembly! {(code){
                mov %rsi, [%rsp+{4*pl as i32}];
                mov %rdi, [%rsp+{4*pr as i32}];
                mov [%rsp+{4*pl as i32}], %rdi;
                mov [%rsp+{4*pr as i32}], %rsi;
            }}
        }

        code
    }

    /// Writes the function which will handle initializing a new coroutine
    fn coroutine_init(
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
         * metadata)
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
                    ; "[-8]: The RIP for the coroutine"
                    ; "[-16]: The ESP for the caller"
                    ; "[-24]: The EBP for the caller"
                    ; "[-32]: The caller return address (for yield return)"
                    ; "[-40]: The coroutine ESP"
                    mov [%rsp+{COROUTINE_RIP_STORE}], %rax;
                    mov [%rsp+{COROUTINE_CALLER_RSP_STORE}], 0;
                    mov [%rsp+{COROUTINE_CALLER_RBP_STORE}], 0;
                    mov [%rsp+{COROUTINE_CALLER_RIP_STORE}], 0;
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

    fn runtime_yield_into_coroutine(code2: &mut Vec<Inst>) {
        /*
         * Input:
         * EAX - address of the coroutine instance
         * EBX - address of the return point
         */
        assembly! {
            (code2) {
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

    fn runtime_yield_return(code: &mut Vec<Inst>) {
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
        left: &'a CompilerNode,
        right: &'a CompilerNode,
        current_func: &String,
    ) -> Result<Vec<Inst>, String> {
        use BinaryOperator::*;

        let mut left_code = vec![];
        self.traverse(left, current_func, &mut left_code)?;
        let mut right_code = vec![];
        self.traverse(right, current_func, &mut right_code)?;

        let mut op_asm = vec![];
        match op {
            BinaryOperator::Add => {
                assembly! {(op_asm) {add %rax, %rbx;}}
            }
            BinaryOperator::Sub => {
                assembly! {(op_asm) {sub %rax, %rbx;}}
            }
            BinaryOperator::Mul => {
                assembly! {(op_asm) {imul %rax, %rbx;}}
            }
            BinaryOperator::Div => {
                assembly! {(op_asm) {
                    cdq;
                    idiv %rbx;
                }}
            }
            BinaryOperator::BAnd => {
                assembly! {(op_asm) {and %rax, %rbx;}}
            }
            BinaryOperator::BOr => {
                assembly! {(op_asm) {or %rax, %rbx;}}
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
                    cmp %rax, %rbx;
                    {{[set]}}
                    and %al, 1;
                }}
            }
        };

        let mut code = vec![];
        assembly! {(code){
            {{left_code}}
            push %rax;
            {{right_code}}
            push %rax;
            pop %rbx;
            pop %rax;
            {{op_asm}}
        }}

        Ok(code)
    }

    fn push_scope(&mut self, ast: &'a CompilerNode) {
        self.scope.push(ast.get_metadata())
    }

    fn pop(&mut self) {
        self.scope.pop();
    }

    fn traverse(
        &mut self,
        ast: &'a CompilerNode,
        current_func: &String,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        // The registers used for passing function parameters, in the order that parameters are
        // assigned to registers
        let fn_param_registers = vec![
            Reg64::Rax,
            Reg64::Rbx,
            Reg64::Rcx,
            Reg64::Rdx,
        ];
        let co_param_registers = vec![
            Reg64::Rbx,
            Reg64::Rcx,
            Reg64::Rdx,
        ];

        self.push_scope(ast);

        match ast {
            Expression::Integer(_, i) => {
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
                        lea %rax, @{format!("str_{}", str_id)};
                    }
                }
            }
            Expression::Identifier(m, id) => {
                let id_offset = self.scope.find(id).unwrap().offset;
                match m.ty() {
                    Type::Custom(_) => {
                        assembly! {(code) {lea %rax, [%rbp-{id_offset}];}}
                    }
                    _ => {
                        assembly! {(code) {mov %rax, [%rbp-{id_offset}];}}
                    }
                }
            }
            Expression::MemberAccess(_, src, member) => {
                self.member_access(current_func, code, src, member)?;
            }
            Expression::UnaryOp(_, op, operand) => {
                self.traverse(operand, current_func, code)?;
                match op {
                    UnaryOperator::Minus => {
                        assembly! {(code){
                            neg %rax;
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
            Expression::If(meta, ref cond, ref true_arm, ref false_arm) => {
                let mut cond_code = vec![];
                self.traverse(cond, current_func, &mut cond_code)?;
                let mut true_code = vec![];
                self.traverse(true_arm, current_func, &mut true_code)?;
                let mut false_code = vec![];
                self.traverse(false_arm, current_func, &mut false_code)?;

                assembly2! {(code, meta) {
                    {{cond_code}}
                    cmp %rax, 0;
                    jz ^else_lbl;
                    {{true_code}}
                    jmp ^end_lbl;
                ^else_lbl:
                    {{false_code}}
                ^end_lbl:
                }};
            }
            Expression::ExpressionBlock(_, body, final_exp) => {
                for s in body.iter() {
                    self.traverse_statement(s, current_func, code)?;
                }
                match final_exp {
                    None => (),
                    Some(fe) => self.traverse(fe, current_func, code)?,
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
                let total_offset = co_def
                    .get_metadata()
                    .local_allocation()
                    .ok_or(format!("Coroutine {} has no allocation size", co_path))?;

                co_def.validate_parameters(params)?;

                assembly! {(code) {
                    ; {format!("Call {}", co_path)}
                    {{self.evaluate_routine_params(params, current_func)?}}
                    {{self.move_routine_params_into_registers(params, &co_param_registers)?}}
                    ; "Load the IP for the coroutine (EAX) and the stack frame allocation (EDI)"
                    lea %rax, [@{co_path.to_label()}];
                    mov %rdi, {total_offset};
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
                // Check if function exists and if the right number of parameters are being
                // passed
                let fn_def = self
                    .root
                    .go_to(fn_path)
                    .ok_or(format!("Could not find: {}", fn_path))?
                    .to_routine()
                    .expect("Expected a routine");
                fn_def.validate_parameters(params)?;

                let return_type = meta.ty();
                if let Type::Custom(_) = return_type {
                    let st_sz = self
                        .struct_table
                        .size_of(return_type)
                        .ok_or(format!("no size for {} found", return_type))?;

                    assembly! {(code){
                        sub %rsp, {st_sz};
                    }}
                }

                // evaluate each paramater then store in registers Eax, Ebx, Ecx, Edx before
                // calling the function
                assembly! {(code) {
                    ; {{format!("Call {}", fn_path)}}
                    {{self.evaluate_routine_params(params, current_func)?}}
                    {{self.move_routine_params_into_registers(params, &fn_param_registers)?}}
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
        module: &'a Module<Scope>,
        current_func: &String,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        self.scope.push(module.get_metadata());
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

    fn traverse_item(&mut self, item: &'a Item<Scope>, code: &mut Vec<Inst>) -> Result<(), String> {
        self.scope.push(item.get_metadata());
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
        routine: &'a RoutineDef<Scope>,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        match routine.get_def() {
            RoutineDefType::Coroutine => self.traverse_coroutine_def(routine, code),
            RoutineDefType::Function => self.traverse_function_def(routine, code),
        }
    }

    fn traverse_function_def(
        &mut self,
        routine: &'a RoutineDef<Scope>,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        let fn_param_registers = vec![
            Reg64::Rax,
            Reg64::Rbx,
            Reg64::Rcx,
            Reg64::Rdx,
        ];

        if let RoutineDef {
            meta: scope,
            def: RoutineDefType::Function,
            name: ref fn_name,
            body: stmts,
            ..
        } = routine
        {
            let total_offset = match scope.level() {
                Routine { allocation, .. } => allocation,
                _ => panic!("Invalid scope for function definition"),
            };

            assembly! {(code) {
            @{scope.canon_path().to_label()}:
                ; {{format!("Define {}", scope.canon_path())}}
                ;"Prepare stack frame for this function"
                push %rbp;
                mov %rbp, %rsp;
                sub %rsp, {*total_offset};
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
        routine: &'a RoutineDef<Scope>,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        assembly! {(code) {
            @{routine.get_metadata().canon_path().to_label()}:
            ; {{format!("Define {}", routine.get_metadata().canon_path())}}
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
        statement: &'a Statement<Scope>,
        current_func: &String,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        match statement {
            Statement::Bind(b) => self.traverse_bind(b, current_func, code),
            Statement::Mutate(m) => self.traverse_mutate(m, current_func, code),
            Statement::Return(n) => self.traverse_return(n, current_func, code),
            Statement::YieldReturn(n) => self.traverse_yieldreturn(n, current_func, code),
            Statement::Printi(n) => self.traverse_printi(n, current_func, code),
            Statement::Printiln(n) => self.traverse_printiln(n, current_func, code),
            Statement::Printbln(n) => self.traverse_printbln(n, current_func, code),
            Statement::Prints(n) => self.traverse_prints(n, current_func, code),
            Statement::Expression(n) => self.traverse(n, current_func, code),
        }
    }

    fn traverse_bind(
        &mut self,
        bind: &'a Bind<Scope>,
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
        mutate: &'a Mutate<Scope>,
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

    fn traverse_printi(
        &mut self,
        p: &'a Printi<Scope>,
        current_func: &String,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        self.traverse(p.get_value(), current_func, code)?;

        assembly! {(code) {
            push @_i32_fmt;
            push %rax;
            {{Compiler::make_c64_extern_call("printf", 2)}}
        }}
        Ok(())
    }

    fn traverse_printiln(
        &mut self,
        p: &'a Printiln<Scope>,
        current_func: &String,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        self.traverse(p.get_value(), current_func, code)?;

        assembly! {(code) {
            push @_i32_fmt;
            push %rax;
            {{Compiler::make_c64_extern_call("printf", 2)}}
        }}
        Ok(())
    }

    fn traverse_printbln(
        &mut self,
        p: &'a Printbln<Scope>,
        current_func: &String,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        self.traverse(p.get_value(), current_func, code)?;

        assembly! {(code) {
            call @print_bool;
        }}
        Ok(())
    }

    fn traverse_prints(
        &mut self,
        p: &'a Prints<Scope>,
        current_func: &String,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        self.traverse(p.get_value(), current_func, code)?;

        assembly! {(code) {
            push %rax;
            push [rel @stdout];
            {{Compiler::make_c64_extern_call("fputs", 2)}}
        }}
        Ok(())
    }

    fn traverse_yield(
        &mut self,
        y: &'a Yield<Scope>,
        current_func: &String,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        assembly! {(code) {
            {{self.yield_exp(y.get_metadata(), y.get_value(), current_func)?}}
        }}
        Ok(())
    }

    fn traverse_yieldreturn(
        &mut self,
        yr: &'a YieldReturn<Scope>,
        current_func: &String,
        code: &mut Vec<Inst>,
    ) -> Result<(), String> {
        assembly! {(code) {
            {{self.yield_return(yr.get_metadata(), yr.get_value(), current_func)?}}
        }}
        Ok(())
    }

    fn traverse_return(
        &mut self,
        r: &'a Return<Scope>,
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
        src: &'a CompilerNode,
        member: &str,
    ) -> Result<(), String> {
        let src_ty = src.get_metadata().ty();
        match src_ty {
            Type::Custom(struct_name) => {
                code.push(Inst::Comment(format!("{}.{}", struct_name, member)));

                self.traverse(src, current_func, code)?;

                let struct_def = self.struct_table.get(struct_name).ok_or(format!(
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
                    _ => {
                        assembly! {(code) {
                            mov %rax, [%rax+{field_offset}];
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
        field_values: &'a Vec<(String, CompilerNode)>,
        offset: i32,
        allocate: bool,
    ) -> Result<Vec<Inst>, String> {
        let struct_def = self.struct_table.get(struct_name).expect(&format!(
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
                sub %rsp, {struct_sz};
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
        fvalue: &'a CompilerNode,
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
                self.traverse(fvalue, current_func, &mut code)?;
                match fvalue.get_metadata().ty() {
                    Type::Custom(struct_name) => {
                        let asm = self.copy_struct_into(
                            struct_name,
                            dst,
                            dst_offset,
                            Reg64::Rax,
                            0,
                        )?;
                        assembly! {(code){
                            {{asm}}
                        }};
                    }
                    _ => {
                        assembly! {(code) {
                            mov [%{Reg::R64(dst)}-{dst_offset}], %rax;
                        }};
                    }
                }
            }
        }
        Ok(code)
    }

    fn bind(
        &mut self,
        value: &'a CompilerNode,
        current_func: &String,
        dst: Reg64,
        dst_offset: i32,
    ) -> Result<Vec<Inst>, String> {
        let mut code = vec![];
        self.traverse(value, current_func, &mut code)?;

        match value.get_metadata().ty() {
            Type::Custom(name) => {
                match value {
                    Expression::Identifier(..) => {
                        // If an identifier is being copied to another identifier, then just copy
                        // the data over rather than pop off of the stack
                        let asm =
                            self.copy_struct_into(name, dst, dst_offset, Reg64::Rax, 0)?;
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
                assembly! {(code) {
                    mov [%{Reg::R64(dst)}-{dst_offset}], %rax;
                }};
            }
        }
        Ok(code)
    }

    fn yield_exp(
        &mut self,
        meta: &'a Scope,
        exp: &'a CompilerNode,
        current_func: &String,
    ) -> Result<Vec<Inst>, String> {
        let mut code = vec![];
        self.traverse(exp, current_func, &mut code)?;
        match meta.ty() {
            Type::Custom(struct_name) => {
                let st = self
                    .struct_table
                    .get(struct_name)
                    .ok_or(format!("no definition for {} found", struct_name))?;
                let st_sz = st
                    .size
                    .ok_or(format!("struct {} has no resolved size", struct_name))?;
                assembly! {(code){
                    sub %rsp, {st_sz};
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
        meta: &'a Scope,
        exp: &'a Option<CompilerNode>,
        current_func: &String,
    ) -> Result<Vec<Inst>, String> {
        let mut code = vec![];
        if let Some(exp) = exp {
            self.traverse(exp, current_func, &mut code)?;
            match exp.get_metadata().ty() {
                Type::Custom(struct_name) => {
                    // Copy the structure into the stack frame of the calling function
                    let asm =
                        self.copy_struct_into(struct_name, Reg64::Rsi, 0, Reg64::Rax, 0)?;
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
        exp: &'a Option<CompilerNode>,
        current_func: &String,
    ) -> Result<Vec<Inst>, String> {
        let mut code = vec![];
        code.push(Inst::Label(".terminus".into()));
        match exp {
            Some(e) => {
                self.traverse(e, current_func, &mut code)?;
                // If the expression is a custom type then copy the value into the stack frame
                // of the caller (or the yielder in the case of coroutines)
                match e.get_metadata().ty() {
                    Type::Custom(struct_name) => {
                        // Copy the structure into the stack frame of the calling function
                        let asm = self.copy_struct_into(
                            struct_name,
                            Reg64::Rsi,
                            0,
                            Reg64::Rax,
                            0,
                        )?;

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

    fn return_exp_temp(
        &mut self,
        exp: &'a Option<Box<CompilerNode>>,
        current_func: &String,
    ) -> Result<Vec<Inst>, String> {
        let mut code = vec![];
        code.push(Inst::Label(".terminus".into()));
        match exp {
            Some(e) => {
                self.traverse(e, current_func, &mut code)?;
                // If the expression is a custom type then copy the value into the stack frame
                // of the caller (or the yielder in the case of coroutines)
                match e.get_metadata().ty() {
                    Type::Custom(struct_name) => {
                        // Copy the structure into the stack frame of the calling function
                        let asm = self.copy_struct_into(
                            struct_name,
                            Reg64::Rsi,
                            0,
                            Reg64::Rax,
                            0,
                        )?;

                        let is_coroutine = self.scope.in_coroutine();
                        if is_coroutine {
                            assembly! {(code){
                                mov %rsi, [%rbp+{COROUTINE_CALLER_RSP_STORE}];   // Load the Caller functions ESP pointer
                                {{asm}}
                            }};
                        } else {
                            assembly! {(code){
                                lea %rsi, [%rbp+{FUNCTION_CALLER_RSP}];  // Load the caller function's ESP pointer
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

    fn pop_struct_into(&self, struct_name: &Path, id_offset: u32) -> Result<Vec<Inst>, String> {
        let mut code = vec![];
        let ty_def = self
            .struct_table
            .get(struct_name)
            .ok_or(format!("Could not find definition for {}", struct_name))?;
        for FieldInfo {
            name: field_name,
            ty: field_ty,
            ..
        } in ty_def.get_fields().iter().rev()
        {
            let rel_field_offset = ty_def.get_offset_of(field_name).expect(&format!(
                "CRITICAL: struct {} has member, {}, with no relative offset",
                struct_name, field_name,
            )) as u32;
            let field_offset = id_offset - rel_field_offset;
            match field_ty {
                Type::Custom(name) => {
                    assembly! {(code){
                        {{self.pop_struct_into(name, field_offset)?}}
                    }}
                }
                _ => {
                    assembly! {(code) {
                        pop %rax;
                        mov [%rbp-{field_offset as i32}], %rax;
                    }};
                }
            }
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
            .struct_table
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
                _ => {
                    assembly! {(code) {
                        ; {format!("copy {}.{}", struct_name, field_name)}
                        mov %rdi, [%{Reg::R64(src_reg)}-{src_offset - (struct_sz - rel_field_offset)}];
                        mov [%{Reg::R64(dst_reg)}-{dst_field_offset}], %rdi;
                    }};
                }
            }
        }

        Ok(code)
    }

    fn move_params_into_stackframe(
        &self,
        routine: &RoutineDef<Scope>,
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
        let routine_sym_table = routine.get_metadata();
        for idx in 0..params.len() {
            let param_offset = routine_sym_table
                .get(&params[idx].0)
                .ok_or(format!(
                    "Critical: could not find parameter {} in symbol table for {}",
                    params[idx].0, routine_name
                ))?
                .offset;
            match &params[idx].1 {
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
                    assembly! {(code){
                        mov [%rbp-{param_offset}], %{Reg::R64(param_registers[idx])};
                    }};
                }
            }
        }
        Ok(code)
    }

    fn evaluate_routine_params(
        &mut self,
        params: &'a Vec<CompilerNode>,
        current_func: &String,
    ) -> Result<Vec<Inst>, String> {
        let mut code = vec![];
        for param in params.iter() {
            self.traverse(param, current_func, &mut code)?;
            assembly! {(code){
                push %rax;
            }};
        }
        Ok(code)
    }

    fn move_routine_params_into_registers(
        &mut self,
        params: &'a Vec<CompilerNode>,
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
        for reg in param_registers.iter().take(params.len()).rev() {
            assembly! {(code){
                pop %{Reg::R64(*reg)};
            }};
        }
        Ok(code)
    }
}
