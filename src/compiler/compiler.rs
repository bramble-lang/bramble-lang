// ASM - types capturing the different assembly instructions along with functions to
// convert to text so that a compiled program can be saves as a file of assembly
// instructions
use super::vartable::*;
use crate::ast::Ast;
use crate::semantics::semanticnode::SemanticNode;
use crate::compiler::x86::assembly::*;
use crate::assembly;
use crate::unit_op;
use crate::unary_op;
use crate::binary_op;
use crate::operand;
use crate::register;

pub struct Compiler {
    code: Vec<Inst>,
}

impl Compiler {
    pub fn print(&self, output: &mut dyn std::io::Write) -> std::io::Result<()> {
        for inst in self.code.iter() {
            writeln!(output, "{}", inst)?;
        }
        Ok(())
    }

    pub fn compile(ast: &SemanticNode) -> Compiler {
        let mut func_table = FunctionTable::from_semantic_ast(&ast);
        let mut code = vec![];

        Compiler::create_base(&mut code);

        Compiler::coroutine_init("next_stack_addr", "stack_size", &mut code);
        Compiler::runtime_yield_into_coroutine( &mut code);
        Compiler::runtime_yield_return( &mut code);
        Compiler::print_bool( &mut code);

        // Put user code here
        let global_func = "".into();
        Compiler::traverse(ast, &global_func, &mut func_table, &mut code).unwrap();
        Compiler { code }
    }

    /// Creates the runtime code that will manage the entire execution of this program.
    fn create_base(code2: &mut Vec<Inst>) {
        assembly!{
            (code2) {
                include "io.inc";

                section ".data";
                data next_stack_addr: dd 0;
                data stack_size: dd 8*1024;

                section ".text";
                global CMAIN;
                @CMAIN:
                    push %ebp;
                    mov %ebp, %esp;
                    mov %eax, %esp;
                    sub %eax, [@stack_size];
                    mov [@next_stack_addr], %eax;

                    call @my_main;

                    mov %esp, %ebp;
                    pop %ebp;
                    ret;
            }
        };
    }

    fn print_bool(code2: &mut Vec<Inst>) {
        assembly!{
            (code2) {
                @print_bool:
                    push %ebp;
                    mov %ebp, %esp;
                    cmp %eax, 0;
                    jz ^false;
                    print_str "true";
                    jmp ^done;
                    ^false:
                    print_str "false";
                    ^done:
                    mov %esp, %ebp;
                    pop %ebp;
                    ret;
            }
        }
    }

    /// Writes the function which will handle initializing a new coroutine
    fn coroutine_init(next_stack_variable: &str, stack_increment_variable: &str, code2: &mut Vec<Inst>) {
        /*
         * Input:
         * EAX - address of the coroutine's instructions
         * EBX - EnX - initialization parameters for the coroutine
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

        assembly!{
            (code2) {
                @runtime_init_coroutine:
                    push %ebp;
                    mov %ebp, %esp;
                    mov %esp, [@{next_stack_variable}];
                    mov [%esp-4], %eax;
                    mov [%esp-8], 0;
                    mov [%esp-12], 0;
                    mov [%esp-16], 0;
                    lea %eax, [%esp-20];
                    mov [%esp-20], %eax;
                    mov %eax, %esp;
                    sub %esp, [@{stack_increment_variable}];
                    mov [@{next_stack_variable}], %esp;
                    mov %esp, %ebp;
                    pop %ebp;
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
        assembly!{
            (code2) {
                @runtime_yield_into_coroutine:
                    mov [%eax-8], %esp;
                    mov [%eax-12], %ebp;
                    mov [%eax-16], %ebx;
                    mov %ebp, %eax;
                    mov %esp, [%ebp-20];
                    jmp [%ebp-4];
            }
        }
    }

    fn runtime_yield_return(code2: &mut Vec<Inst>) {
        /*
         * Input:
         * EAX - value being returned (if any)
         * EBX - re-entry address
         */
        // When in a coroutine, return to the calling coroutine
        assembly!{
            (code2) {
                @runtime_yield_return:
                    mov [%ebp-20], %esp;
                    mov [%ebp-4], %ebx;
                    mov %esp, [%ebp-8];
                    mov %ebx, [%ebp-16];
                    mov %ebp, [%ebp-12];
                    jmp %ebx;
            }
        }
    }

    fn handle_binary_operands(left: &SemanticNode, right: &SemanticNode, current_func: &String, function_table: &mut FunctionTable) -> Result<Vec<Inst>, String> {
        let mut left_code = vec![];
        Compiler::traverse(left, current_func, function_table, &mut left_code)?;
        let mut right_code = vec![];
        Compiler::traverse(right, current_func, function_table, &mut right_code)?;

        let mut code = vec![];
        assembly!{(code){
            {{left_code}}
            push %eax;
            {{right_code}}
            push %eax;
            pop %ebx;
            pop %eax;
        }}

        Ok(code)
    }
    
    fn comparison_op(op: Inst, left: &SemanticNode, right: &SemanticNode, current_func: &String, function_table: &mut FunctionTable, code: &mut Vec<Inst>) -> Result<(), String> {
        assembly!{(code){
            {{Compiler::handle_binary_operands(left, right, current_func, function_table)?}}
            cmp %eax, %ebx;
            {{[op]}}
            and %al, 1;
        }}

        Ok(())
    }

    fn traverse(
        ast: &SemanticNode,
        current_func: &String,
        function_table: &mut FunctionTable,
        //output: &mut Vec<Instruction>,
        output: &mut Vec<Inst>,
    ) -> Result<(),String> {
        // The registers used for passing function parameters, in the order that parameters are
        // assigned to registers
        let fn_param_registers = vec![Reg::R32(Reg32::Eax), Reg::R32(Reg32::Ebx), Reg::R32(Reg32::Ecx), Reg::R32(Reg32::Edx)];
        let co_param_registers = vec![Reg::R32(Reg32::Ebx), Reg::R32(Reg32::Ecx), Reg::R32(Reg32::Edx)];

        match ast {
            Ast::Integer(_, i) => {
                let mut code = vec![];
                assembly!{(code) {mov %eax, {*i};}}
                output.append(&mut code);
            }
            Ast::Boolean(_, b) => {
                let mut code = vec![];
                assembly!{(code) {mov %eax, {if *b {1} else {0}};}}
                output.append(&mut code);
            }
            Ast::Identifier(_, id) => {
                let id_offset = function_table.get_var_offset(current_func, id).expect("Could not find variable");
                let mut code = vec![];
                assembly!{(code) {mov %eax, [%ebp-{id_offset as u32}];}}
                output.append(&mut code);
            }
            Ast::Mul(_, l, r) => {
                let mut code = vec![];
                assembly!{(code) {
                    {{Compiler::handle_binary_operands(l.as_ref(), r.as_ref(), current_func, function_table)?}}
                    imul %eax, %ebx;
                }}
                output.append(&mut code);
            }
            Ast::Add(_, l, r) => {
                let mut code = vec![];
                assembly!{(code) {
                    {{Compiler::handle_binary_operands(l.as_ref(), r.as_ref(), current_func, function_table)?}}
                    add %eax, %ebx;
                }}
                output.append(&mut code);
            }
            Ast::Eq(_, l, r) => {
                Compiler::comparison_op(Inst::Sete(Reg8::Al), l.as_ref(), r.as_ref(), current_func, function_table, output)?;
            }
            Ast::NEq(_, l, r) => {
                Compiler::comparison_op(Inst::Setne(Reg8::Al), l.as_ref(), r.as_ref(), current_func, function_table, output)?;
            }
            Ast::Ls(_, l, r) => {
                Compiler::comparison_op(Inst::Setl(Reg8::Al), l.as_ref(), r.as_ref(), current_func, function_table, output)?;
            }
            Ast::LsEq(_, l, r) => {
                Compiler::comparison_op(Inst::Setle(Reg8::Al), l.as_ref(), r.as_ref(), current_func, function_table, output)?;
            }
            Ast::Gr(_, l, r) => {
                Compiler::comparison_op(Inst::Setg(Reg8::Al), l.as_ref(), r.as_ref(), current_func, function_table, output)?;
            }
            Ast::GrEq(_, l, r) => {
                Compiler::comparison_op(Inst::Setge(Reg8::Al), l.as_ref(), r.as_ref(), current_func, function_table, output)?;
            }
            Ast::BAnd(_, l, r) => {
                let mut code = vec![];
                assembly!{(code) {
                    {{Compiler::handle_binary_operands(l.as_ref(), r.as_ref(), current_func, function_table)?}}
                    and %eax, %ebx;
                }}
                output.append(&mut code);
            }
            Ast::BOr(_, l, r) => {
                let mut code = vec![];
                assembly!{(code) {
                    {{Compiler::handle_binary_operands(l.as_ref(), r.as_ref(), current_func, function_table)?}}
                    or %eax, %ebx;
                }}
                output.append(&mut code);
            }
            Ast::Printiln(_, ref exp) => {
                Compiler::traverse(exp, current_func, function_table, output)?;
                
                let mut code = vec![];
                assembly!{(code) {
                    print_dec %eax;
                    newline;
                }}
                output.append(&mut code);
            }
            Ast::Printbln(_, ref exp) => {
                Compiler::traverse(exp, current_func, function_table, output)?;
                
                let mut code = vec![];
                assembly!{(code) {
                    call @print_bool;
                    newline;
                }}
                output.append(&mut code);
            }
            Ast::If(_, ref cond, ref true_arm, ref false_arm) => {
                let label_id = function_table.inc_label_count(current_func);
                let else_lbl = format!(
                    "if_false_{}",
                    label_id,
                );
                let end_lbl = format!(
                    "if_end_{}",
                    label_id,
                );

                Compiler::traverse(cond, current_func, function_table, output)?;
                
                let mut code = vec![];
                assembly!{(code) {
                    cmp %eax, 0;
                    jz ^{else_lbl};
                }};
                output.append(&mut code);
                Compiler::traverse(true_arm, current_func, function_table, output)?;
                
                let mut code = vec![];
                assembly!{(code) {
                    jmp ^{end_lbl};
                ^{else_lbl}:
                }};
                output.append(&mut code);
                Compiler::traverse(false_arm, current_func, function_table, output)?;
               
                let mut code = vec![];
                assembly!{(code) {
                ^{end_lbl}:
                }};
                output.append(&mut code);
            }
            Ast::ExpressionBlock(_, body) => {
                for s in body.iter() {
                    Compiler::traverse(s, current_func, function_table, output)?;
                }
            }
            Ast::Statement(_, stm) => {
                Compiler::traverse(stm, current_func, function_table, output)?;
            }
            Ast::Bind(_, id, _, ref exp) => {
                let id_offset = function_table.get_var_offset(current_func, id).expect("Could not find variable");
                Compiler::traverse(exp, current_func, function_table, output)?;
                let mut code = vec![];
                assembly!{(code) {
                    mov [%ebp-{id_offset as u32}], %eax;
                }};
                output.append(&mut code);
            }
            Ast::Module(_, functions, coroutines) => {
                for f in functions.iter() {
                    Compiler::traverse(f, current_func, function_table, output)?;
                }
                for co in coroutines.iter() {
                    Compiler::traverse(co, current_func, function_table, output)?;
                }
            }
            Ast::Return(_, ref exp) => match exp {
                Some(e) => Compiler::traverse(e, current_func, function_table, output)?,
                None => (),
            },
            Ast::Yield(_, ref id) => {
                Compiler::traverse(id, current_func, function_table, output)?;

                let label_id = function_table.inc_label_count(current_func);
                let ret_lbl = format!("lbl_{}", label_id);
                
                let mut code = vec![];
                assembly!{(code) {
                    mov %ebx, ^{ret_lbl};
                    jmp @runtime_yield_into_coroutine;
                    ^{ret_lbl}:
                }};
                output.append(&mut code);
            }
            Ast::YieldReturn(_, ref exp) => {
                if let Some(exp) = exp {
                    Compiler::traverse(exp, current_func, function_table, output)?;
                }

                let label_id = function_table.inc_label_count(current_func);
                let ret_lbl = format!("lbl_{}", label_id);

                let mut code = vec![];
                assembly!{(code) {
                    mov %ebx, ^{ret_lbl};
                    jmp @runtime_yield_return;
                    ^{ret_lbl}:
                }};
                output.append(&mut code);
            }
            Ast::CoroutineDef(_, ref fn_name, _, _, stmts) => {
                let mut code = vec![];
                assembly!{(code) {
                @{fn_name}:
                }};
                output.append(&mut code);

                // Prepare stack frame for this function
                for s in stmts.iter() {
                    Compiler::traverse(s, fn_name, function_table, output)?;
                }
                let mut code = vec![];
                assembly!{(code) {
                    jmp @runtime_yield_return;
                }};
                output.append(&mut code);
            }
            Ast::CoroutineInit(_, ref co, params) => {
                Compiler::validate_routine_call(co, params, function_table).unwrap();
                Compiler::evaluate_routine_params(params, &co_param_registers, current_func, function_table, output).unwrap();
                
                let mut code = vec![];
                assembly!{(code) {
                    lea %eax, [@{co}];
                    call @runtime_init_coroutine;
                    ; "move into coroutine's stack frame"
                    push %ebp;
                    mov %ebp, %eax;
                }};
                output.append(&mut code);

                // Move parameters into the stack frame of the coroutine
                Compiler::move_params_into_stackframe(co, params.len(), &co_param_registers, function_table, output).unwrap();
                
                let mut code = vec![];
                assembly!{(code) {
                    ; "leave coroutine's stack frame"
                    pop %ebp;
                }};
                output.append(&mut code);

            }
            Ast::FunctionDef(_, ref fn_name, params, _, stmts) => {
                let total_offset = function_table.get_total_offset(fn_name).unwrap_or(0);

                let mut code = vec![];
                assembly!{(code) {
                @{fn_name}:
                    ;"Prepare stack frame for this function"
                    push %ebp;
                    mov %ebp, %esp;
                    sub %esp, {total_offset};
                }};
                output.append(&mut code);

                // Move function parameters from registers into the stack frame
                Compiler::move_params_into_stackframe(fn_name, params.len(), &fn_param_registers, function_table, output).unwrap();

                for s in stmts.iter() {
                    Compiler::traverse(s, fn_name, function_table, output)?;
                }

                let mut code = vec![];
                assembly!{(code) {
                    ; "Clean up frame before leaving function"
                    mov %esp, %ebp;
                    pop %ebp;
                    ret;
                }};
                output.append(&mut code);

            }
            Ast::FunctionCall(_, ref fn_name, params) => {
                // Check if function exists and if the right number of parameters are being
                // passed
                Compiler::validate_routine_call(fn_name, params, function_table).unwrap();

                // evaluate each paramater then store in registers Eax, Ebx, Ecx, Edx before
                // calling the function
                Compiler::evaluate_routine_params(params, &fn_param_registers, current_func, function_table, output).unwrap();
                let mut code = vec![];
                assembly!{(code) {
                    call @{fn_name};
                }};
                output.append(&mut code);
            }
            node => panic!("Expected an operator, found {:?}", node),
        };

        Ok(())
    }

    fn move_params_into_stackframe(func: &str, num_params: usize, param_registers: &Vec<Reg>, function_table: &FunctionTable, output: &mut Vec<Inst>) -> Result<(), String>{
        if num_params > param_registers.len() {
            return Err(format!("Compiler: too many parameters in function definition"));
        }

        let mut code = vec![];
        for idx in 0..num_params {
            let param_offset = function_table.funcs[func].vars.vars[idx].frame_offset;
            assembly!{(code){
                mov [%ebp-{param_offset as u32}], %{param_registers[idx]};
            }};
        }
        output.append(&mut code);
        Ok(())
    }

    fn evaluate_routine_params(params: &Vec<SemanticNode>, param_registers: &Vec<Reg>, current_func: &String, function_table: &mut FunctionTable, output: &mut Vec<Inst>) -> Result<(), String> {
        // evaluate each paramater then store in registers Eax, Ebx, Ecx, Edx before
        // calling the function
        if params.len() > param_registers.len() {
            return Err(format!("Compiler: too many parameters being passed to function"));
        }
        for param in params.iter() {
            Compiler::traverse(param, current_func, function_table, output)?;
            let mut code = vec![];
            assembly!{(code){
                push %eax;
            }};
            output.append(&mut code);
        }
        let mut code = vec![];
        for reg in param_registers.iter().take(params.len()).rev() {
            assembly!{(code){
                pop %{*reg};
            }};
        }
        output.append(&mut code);
        Ok(())
    }

    fn validate_routine_call(func: &str, params: &Vec<SemanticNode>, function_table: &FunctionTable) -> Result<(),String>{
        // Check if function exists and if the right number of parameters are being
        // passed
        if !function_table.funcs.contains_key(func) {
            return Err(format!("Compiler: no definition found for function `{}`", func));
        }

        let expected_num_params = function_table.funcs[func].params.len();
        let got_num_params = params.len();
        if expected_num_params != got_num_params {
            Err(format!(
                "Compiler: expected {} but got {} parameters for function `{}`",
                expected_num_params, got_num_params, func
            ))
        } else {
            Ok(())
        }
    }
}
