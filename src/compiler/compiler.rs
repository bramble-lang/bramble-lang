// ASM - types capturing the different assembly instructions along with functions to
// convert to text so that a compiled program can be saves as a file of assembly
// instructions
use crate::compiler::ast::ast::CompilerNode;
use crate::compiler::ast::scope::Type::Routine;
use crate::compiler::ast::scope::ScopeStack;
use crate::ast::Primitive;
use super::vartable::*;
use crate::ast::Ast;
use crate::semantics::semanticnode::SemanticNode;
use crate::compiler::x86::assembly::*;
use crate::assembly;
use crate::assembly2;
use crate::unit_op;
use crate::unary_op;
use crate::binary_op;
use crate::operand;
use crate::register;

pub struct Compiler<'a> {
    code: Vec<Inst>,
    scope: ScopeStack<'a>,
}

impl<'a> Compiler<'a> {
    pub fn print(code: &Vec<Inst>, output: &mut dyn std::io::Write) -> std::io::Result<()> {
        for inst in code.iter() {
            writeln!(output, "{}", inst)?;
        }
        Ok(())
    }

    pub fn compile(ast: &SemanticNode) -> Vec<Inst> {
        let mut func_table = FunctionTable::from_semantic_ast(&ast);
        let mut code = vec![];

        Compiler::create_base(&mut code);

        Compiler::coroutine_init("next_stack_addr", "stack_size", &mut code);
        Compiler::runtime_yield_into_coroutine( &mut code);
        Compiler::runtime_yield_return( &mut code);
        Compiler::print_bool( &mut code);

        // Put user code here
        let global_func = "".into();
        let (compiler_ast, _) = CompilerNode::from(ast, 0);
        let mut compiler = Compiler { code:vec![], scope: ScopeStack::new() };
        compiler.traverse(&compiler_ast, &global_func, &mut func_table, &mut code).unwrap();
        code
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

    fn handle_binary_operands(&mut self, left: &'a CompilerNode, right: &'a CompilerNode, current_func: &String, function_table: &mut FunctionTable) -> Result<Vec<Inst>, String> {
        let mut left_code = vec![];
        self.traverse(left, current_func, function_table, &mut left_code)?;
        let mut right_code = vec![];
        self.traverse(right, current_func, function_table, &mut right_code)?;

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
    
    fn comparison_op(&mut self, op: Inst, left: &'a CompilerNode, right: &'a CompilerNode, current_func: &String, function_table: &mut FunctionTable, code: &mut Vec<Inst>) -> Result<(), String> {
        assembly!{(code){
            {{self.handle_binary_operands(left, right, current_func, function_table)?}}
            cmp %eax, %ebx;
            {{[op]}}
            and %al, 1;
        }}

        Ok(())
    }

    fn push_scope(&mut self, ast: &'a CompilerNode) {
        use crate::syntax::ast::Ast::*;

        match ast {
            Integer(s, ..) | Boolean(s,..) | Identifier(s, ..) | IdentifierDeclare(s,..)
            | Mul(s,..) | Add(s,..) | BAnd(s,..) | BOr(s,..) 
            | Eq(s,..) | NEq(s,..) | Ls(s,..) | LsEq(s, ..) 
            | Gr(s,..) | GrEq(s,..)
            | Printi(s,..) | Printiln(s,..) | Printbln(s,..)
            | If(s,..) | Yield(s,..) | YieldReturn(s,..) | Return(s,..)
            | Bind(s,..) | Statement(s,..) | ExpressionBlock(s,..) 
            | FunctionCall(s,..) | FunctionDef(s,..) | CoroutineDef(s,..) | CoroutineInit(s,..)
            | Module(s,..)  => self.scope.push(s),
        };
    }

    fn pop(&mut self) {
        self.scope.pop();
    }

    fn traverse(
        &mut self,
        ast: &'a CompilerNode,
        current_func: &String,
        function_table: &mut FunctionTable,
        code: &mut Vec<Inst>,
    ) -> Result<(),String> {
        // The registers used for passing function parameters, in the order that parameters are
        // assigned to registers
        let fn_param_registers = vec![Reg::R32(Reg32::Eax), Reg::R32(Reg32::Ebx), Reg::R32(Reg32::Ecx), Reg::R32(Reg32::Edx)];
        let co_param_registers = vec![Reg::R32(Reg32::Ebx), Reg::R32(Reg32::Ecx), Reg::R32(Reg32::Edx)];

        self.push_scope(ast);

        match ast {
            Ast::Integer(_, i) => {
                assembly!{(code) {mov %eax, {*i};}}
            }
            Ast::Boolean(_, b) => {
                assembly!{(code) {mov %eax, {if *b {1} else {0}};}}
            }
            Ast::Identifier(_, id) => {
                let id_offset = self.scope.find(id).unwrap().offset;
                assembly!{(code) {mov %eax, [%ebp-{id_offset as u32}];}}
            }
            Ast::Mul(_, l, r) => {
                assembly!{(code) {
                    {{self.handle_binary_operands(l.as_ref(), r.as_ref(), current_func, function_table)?}}
                    imul %eax, %ebx;
                }}
            }
            Ast::Add(_, l, r) => {
                assembly!{(code) {
                    {{self.handle_binary_operands(l.as_ref(), r.as_ref(), current_func, function_table)?}}
                    add %eax, %ebx;
                }}
            }
            Ast::Eq(_, l, r) => {
                self.comparison_op(Inst::Sete(Reg8::Al), l.as_ref(), r.as_ref(), current_func, function_table, code)?;
            }
            Ast::NEq(_, l, r) => {
                self.comparison_op(Inst::Setne(Reg8::Al), l.as_ref(), r.as_ref(), current_func, function_table, code)?;
            }
            Ast::Ls(_, l, r) => {
                self.comparison_op(Inst::Setl(Reg8::Al), l.as_ref(), r.as_ref(), current_func, function_table, code)?;
            }
            Ast::LsEq(_, l, r) => {
                self.comparison_op(Inst::Setle(Reg8::Al), l.as_ref(), r.as_ref(), current_func, function_table, code)?;
            }
            Ast::Gr(_, l, r) => {
                self.comparison_op(Inst::Setg(Reg8::Al), l.as_ref(), r.as_ref(), current_func, function_table, code)?;
            }
            Ast::GrEq(_, l, r) => {
                self.comparison_op(Inst::Setge(Reg8::Al), l.as_ref(), r.as_ref(), current_func, function_table, code)?;
            }
            Ast::BAnd(_, l, r) => {
                assembly!{(code) {
                    {{self.handle_binary_operands(l.as_ref(), r.as_ref(), current_func, function_table)?}}
                    and %eax, %ebx;
                }}
            }
            Ast::BOr(_, l, r) => {
                assembly!{(code) {
                    {{self.handle_binary_operands(l.as_ref(), r.as_ref(), current_func, function_table)?}}
                    or %eax, %ebx;
                }}
            }
            Ast::Printiln(_, ref exp) => {
                self.traverse(exp, current_func, function_table, code)?;
                
                assembly!{(code) {
                    print_dec %eax;
                    newline;
                }}
            }
            Ast::Printbln(_, ref exp) => {
                self.traverse(exp, current_func, function_table, code)?;
                
                assembly!{(code) {
                    call @print_bool;
                    newline;
                }}
            }
            Ast::If(_, ref cond, ref true_arm, ref false_arm) => {
                let mut cond_code = vec![];
                self.traverse(cond, current_func, function_table, &mut cond_code)?;
                let mut true_code = vec![];
                self.traverse(true_arm, current_func, function_table, &mut true_code)?;
                let mut false_code = vec![];
                self.traverse(false_arm, current_func, function_table, &mut false_code)?;
                
                assembly2!{(code, function_table.funcs.get_mut(current_func).unwrap()) {
                    {{cond_code}}
                    cmp %eax, 0;
                    jz ^else_lbl;
                    {{true_code}}
                    jmp ^end_lbl;
                ^else_lbl:
                    {{false_code}}
                ^end_lbl:
                }};
            }
            Ast::ExpressionBlock(_, body) => {
                for s in body.iter() {
                    self.traverse(s, current_func, function_table, code)?;
                }
            }
            Ast::Statement(_, stm) => {
                self.traverse(stm, current_func, function_table, code)?;
            }
            Ast::Bind(_, id, _, ref exp) => {
                let id_offset = self.scope.find(id).ok_or(format!("Could not find variable {}", id))?.offset;
                self.traverse(exp, current_func, function_table, code)?;
                assembly!{(code) {
                    mov [%ebp-{id_offset as u32}], %eax;
                }};
            }
            Ast::Module(_, functions, coroutines) => {
                for f in functions.iter() {
                    self.traverse(f, current_func, function_table, code)?;
                }
                for co in coroutines.iter() {
                    self.traverse(co, current_func, function_table, code)?;
                }
            }
            Ast::Return(_, ref exp) => match exp {
                Some(e) => self.traverse(e, current_func, function_table, code)?,
                None => (),
            },
            Ast::Yield(_, ref id) => {
                self.traverse(id, current_func, function_table, code)?;
                assembly2!{(code, function_table.funcs.get_mut(current_func).unwrap()) {
                    mov %ebx, ^ret_lbl;
                    jmp @runtime_yield_into_coroutine;
                    ^ret_lbl:
                }};
            }
            Ast::YieldReturn(_, ref exp) => {
                if let Some(exp) = exp {
                    self.traverse(exp, current_func, function_table, code)?;
                }
                
                assembly2!{(code, function_table.funcs.get_mut(current_func).unwrap()) {
                    mov %ebx, ^ret_lbl;
                    jmp @runtime_yield_return;
                    ^ret_lbl:
                }};
            }
            Ast::CoroutineDef(_, ref fn_name, _, _, stmts) => {
                assembly!{(code) {
                @{fn_name}:
                }};

                // Prepare stack frame for this function
                for s in stmts.iter() {
                    self.traverse(s, fn_name, function_table, code)?;
                }
                assembly!{(code) {
                    jmp @runtime_yield_return;
                }};
            }
            Ast::CoroutineInit(_, ref co, params) => {
                Compiler::validate_routine_call(co, params, function_table)?;


                assembly!{(code) {
                    {{self.evaluate_routine_params(params, &co_param_registers, current_func, function_table)?}}
                    lea %eax, [@{co}];
                    call @runtime_init_coroutine;
                    ; "move into coroutine's stack frame"
                    push %ebp;
                    mov %ebp, %eax;
                    ; "move parameters into the stack frame of the coroutine"
                    {{{
                        let codef = &function_table.funcs[co].params;
                        self.move_params_into_stackframe(co, codef, &co_param_registers, function_table)?
                    }}}
                    ; "leave coroutine's stack frame"
                    pop %ebp;
                }};
            }
            Ast::FunctionDef(scope, ref fn_name, params, _, stmts) => {
                let total_offset = match scope.ty() {
                    Routine{allocation,..} => {
                        allocation
                    },
                    _ => panic!("Invalid scope for function definition"),
                };

                assembly!{(code) {
                @{fn_name}:
                    ;"Prepare stack frame for this function"
                    push %ebp;
                    mov %ebp, %esp;
                    sub %esp, {*total_offset};
                    ; "Move function parameters from registers into the stack frame"
                    {{self.move_params_into_stackframe(fn_name, &params, &fn_param_registers, function_table)?}}
                }};


                for s in stmts.iter() {
                    self.traverse(s, fn_name, function_table, code)?;
                }

                assembly!{(code) {
                    ; "Clean up frame before leaving function"
                    mov %esp, %ebp;
                    pop %ebp;
                    ret;
                }};

            }
            Ast::FunctionCall(_, ref fn_name, params) => {
                // Check if function exists and if the right number of parameters are being
                // passed
                Compiler::validate_routine_call(fn_name, params, function_table).unwrap();

                // evaluate each paramater then store in registers Eax, Ebx, Ecx, Edx before
                // calling the function
                assembly!{(code) {
                    {{self.evaluate_routine_params(params, &fn_param_registers, current_func, function_table)?}}
                    call @{fn_name};
                }};
            }
            node => panic!("Expected an operator, found {:?}", node),
        };

        self.pop();

        Ok(())
    }

    fn move_params_into_stackframe(&self, func: &str, params: &Vec<(String,Primitive)>, param_registers: &Vec<Reg>, function_table: &FunctionTable) -> Result<Vec<Inst>, String>{
        if params.len() > param_registers.len() {
            return Err(format!("Compiler: too many parameters in function definition"));
        }

        let mut code = vec![];
        for idx in 0..params.len() {
            let param_offset = function_table.funcs[func].vars.vars[idx].frame_offset;
            assembly!{(code){
                mov [%ebp-{param_offset as u32}], %{param_registers[idx]};
            }};
        }
        Ok(code)
    }

    fn evaluate_routine_params(&mut self, params: &'a Vec<CompilerNode>, param_registers: &Vec<Reg>, current_func: &String, function_table: &mut FunctionTable) -> Result<Vec<Inst>, String> {
        // evaluate each paramater then store in registers Eax, Ebx, Ecx, Edx before
        // calling the function
        if params.len() > param_registers.len() {
            return Err(format!("Compiler: too many parameters being passed to function"));
        }
        let mut code = vec![];
        for param in params.iter() {
            self.traverse(param, current_func, function_table, &mut code)?;
            assembly!{(code){
                push %eax;
            }};
        }
        for reg in param_registers.iter().take(params.len()).rev() {
            assembly!{(code){
                pop %{*reg};
            }};
        }
        Ok(code)
    }

    fn validate_routine_call(func: &str, params: &Vec<CompilerNode>, function_table: &FunctionTable) -> Result<(),String>{
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
