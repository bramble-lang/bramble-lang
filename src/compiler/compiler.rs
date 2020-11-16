// ASM - types capturing the different assembly instructions along with functions to
// convert to text so that a compiled program can be saves as a file of assembly
// instructions
use super::vartable::*;
use crate::ast::Ast;
use crate::semantics::semanticnode::SemanticNode;
use crate::compiler::tmp_asm::*;
use crate::compiler::x86::assembly::*;
use crate::assembly;
use crate::unit_op;
use crate::unary_op;
use crate::binary_op;
use crate::operand;
use crate::register;

pub struct Compiler {
    code: Vec<Instruction>,
    code2: Vec<Inst>,
}

impl Compiler {
    pub fn print(&self, output: &mut dyn std::io::Write) -> std::io::Result<()> {
        for i in self.code2.iter() {
            writeln!(output, "{}", i)?;
        }
        print_x86(&self.code, output)
    }

    pub fn compile(ast: &SemanticNode) -> Compiler {
        let mut func_table = FunctionTable::from_semantic_ast(&ast);
        let mut code = vec![];
        let mut code2 = vec![];

        Compiler::create_base(&mut code2);

        Compiler::coroutine_init("next_stack_addr", "stack_size", &mut code2);
        Compiler::runtime_yield_into_coroutine( &mut code2);
        Compiler::runtime_yield_return( &mut code2);
        Compiler::print_bool( &mut code2);

        // Put user code here
        let global_func = "".into();
        Compiler::traverse(ast, &global_func, &mut func_table, &mut code);
        Compiler { code, code2 }
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

    fn handle_binary_operands(left: &SemanticNode, right: &SemanticNode, current_func: &String, function_table: &mut FunctionTable, output: &mut Vec<Instruction>) {
        use Instruction::*;
        use Register::*;

        Compiler::traverse(left, current_func, function_table, output);
        output.push(Push(Eax));
        Compiler::traverse(right, current_func, function_table, output);
        output.push(Push(Eax));

        output.push(Pop(Ebx));
        output.push(Pop(Eax));
    }
    
    fn comparison_op(op: Instruction, left: &SemanticNode, right: &SemanticNode, current_func: &String, function_table: &mut FunctionTable, output: &mut Vec<Instruction>) {
        use Instruction::*;
        use Register::*;

        Compiler::handle_binary_operands(left, right, current_func, function_table, output);

        output.push(Cmp(Register::Eax, Source::Register(Register::Ebx)));
        output.push(op);
        output.push(And(Register::Al, Source::Integer(1)));
        output.push(Movzx(Location::Register(Eax), Source::Register(Al)));
    }

    fn traverse(
        ast: &SemanticNode,
        current_func: &String,
        function_table: &mut FunctionTable,
        output: &mut Vec<Instruction>,
    ) {
        use Instruction::*;
        use Register::*;

        // The registers used for passing function parameters, in the order that parameters are
        // assigned to registers
        let fn_param_registers = [Eax, Ebx, Ecx, Edx];
        let co_param_registers = [Ebx, Ecx, Edx];

        match &ast {
            Ast::Integer(_, i) => {
                output.push(Mov(Location::Register(Eax), Source::Integer(*i)));
            }
            Ast::Boolean(_, b) => {
                output.push(Mov(
                    Location::Register(Eax),
                    Source::Integer(if *b { 1 } else { 0 }),
                ));
            }
            Ast::Identifier(_, id) => {
                let id_offset = {
                    let var = function_table.funcs[current_func]
                        .vars
                        .vars
                        .iter()
                        .find(|v| v.name == *id)
                        .expect("CRITICAL: identifier not found in var table");
                    var.frame_offset
                };
                output.push(Mov(
                    Location::Register(Eax),
                    Source::Memory(format!("ebp-{}", id_offset)),
                ));
            }
            Ast::Mul(_, l, r) => {
                Compiler::handle_binary_operands(l.as_ref(), r.as_ref(), current_func, function_table, output);
                output.push(IMul(Eax, Location::Register(Ebx)));
            }
            Ast::Add(_, l, r) => {
                Compiler::handle_binary_operands(l.as_ref(), r.as_ref(), current_func, function_table, output);
                output.push(Add(Eax, Source::Register(Ebx)));
            }
            Ast::Eq(_, l, r) => {
                Compiler::comparison_op(Sete(Register::Al), l.as_ref(), r.as_ref(), current_func, function_table, output);
            }
            Ast::NEq(_, l, r) => {
                Compiler::comparison_op(Setne(Register::Al), l.as_ref(), r.as_ref(), current_func, function_table, output);
            }
            Ast::Ls(_, l, r) => {
                Compiler::comparison_op(Setl(Register::Al), l.as_ref(), r.as_ref(), current_func, function_table, output);
            }
            Ast::LsEq(_, l, r) => {
                Compiler::comparison_op(Setle(Register::Al), l.as_ref(), r.as_ref(), current_func, function_table, output);
            }
            Ast::Gr(_, l, r) => {
                Compiler::comparison_op(Setg(Register::Al), l.as_ref(), r.as_ref(), current_func, function_table, output);
            }
            Ast::GrEq(_, l, r) => {
                Compiler::comparison_op(Setge(Register::Al), l.as_ref(), r.as_ref(), current_func, function_table, output);
            }
            Ast::BAnd(_, l, r) => {
                Compiler::handle_binary_operands(l.as_ref(), r.as_ref(), current_func, function_table, output);
                output.push(And(Eax, Source::Register(Ebx)));
            }
            Ast::BOr(_, l, r) => {
                Compiler::handle_binary_operands(l.as_ref(), r.as_ref(), current_func, function_table, output);
                output.push(Or(Eax, Source::Register(Ebx)));
            }
            Ast::If(_, ref cond, ref true_arm, ref false_arm) => {
                function_table
                    .funcs
                    .entry(current_func.clone())
                    .and_modify(|fi| fi.label_count += 1);
                let else_lbl = format!(
                    ".else_lbl_{}",
                    function_table.funcs[current_func].label_count
                );
                let end_lbl = format!(
                    ".if_end_lbl_{}",
                    function_table.funcs[current_func].label_count
                );

                Compiler::traverse(cond, current_func, function_table, output);
                output.push(Cmp(Register::Eax, Source::Integer(0)));
                output.push(Jz(else_lbl.clone()));
                //Compiler::traverse(true_arm, current_func, function_table, output);
                Compiler::traverse(true_arm, current_func, function_table, output);
                output.push(Jmp(end_lbl.clone()));
                output.push(Label(else_lbl));
                Compiler::traverse(false_arm, current_func, function_table, output);
                output.push(Label(end_lbl));
            }
            Ast::ExpressionBlock(_, body) => {
                for s in body.iter() {
                    Compiler::traverse(s, current_func, function_table, output);
                }
            }
            Ast::Statement(_, stm) => {
                Compiler::traverse(stm, current_func, function_table, output);
            }
            Ast::Bind(meta, id, _, ref exp) => {
                let id_offset = {
                    let var = function_table.get_var(current_func, id).expect("Could not find variable");
                    var.frame_offset
                };
                Compiler::traverse(exp, current_func, function_table, output);
                output.push(Mov(
                    Location::Memory(format!("ebp-{}", id_offset)),
                    Source::Register(Eax),
                ));
            }
            Ast::Module(_, functions, coroutines) => {
                for f in functions.iter() {
                    Compiler::traverse(f, current_func, function_table, output);
                }
                for co in coroutines.iter() {
                    Compiler::traverse(co, current_func, function_table, output);
                }
            }
            Ast::Return(_, ref exp) => match exp {
                Some(e) => Compiler::traverse(e, current_func, function_table, output),
                None => (),
            },
            Ast::Yield(_, ref id) => {
                Compiler::traverse(id, current_func, function_table, output);

                function_table
                    .funcs
                    .entry(current_func.clone())
                    .and_modify(|fi| fi.label_count += 1);
                let ret_lbl = format!(".lbl_{}", function_table.funcs[current_func].label_count);

                output.push(Mov(
                    Location::Register(Ebx),
                    Source::Address(ret_lbl.clone()),
                ));
                output.push(Jmp("runtime_yield_into_coroutine".into()));
                output.push(Label(ret_lbl.clone()));
            }
            Ast::YieldReturn(_, ref exp) => {
                if let Some(exp) = exp {
                    Compiler::traverse(exp, current_func, function_table, output);
                }

                let label_id = function_table.inc_label_count(current_func);
                let ret_lbl = format!(".lbl_{}", label_id);

                output.push(Mov(
                    Location::Register(Ebx),
                    Source::Address(ret_lbl.clone()),
                ));
                output.push(Jmp("runtime_yield_return".into()));
                output.push(Label(ret_lbl.clone()));
            }
            Ast::Printiln(_, ref exp) => {
                Compiler::traverse(exp, current_func, function_table, output);
                output.push(Print(Source::Register(Eax)));
                output.push(Newline);
            }
            Ast::Printbln(_, ref exp) => {
                Compiler::traverse(exp, current_func, function_table, output);
                //output.push(Print(Source::Register(Eax)));
                output.push(Call("print_bool".into()));
                output.push(Newline);
            }
            Ast::CoroutineDef(_, ref fn_name, _, _, stmts) => {
                output.push(Label(fn_name.clone()));

                // Prepare stack frame for this function
                for s in stmts.iter() {
                    Compiler::traverse(s, fn_name, function_table, output);
                }
                output.push(Jmp("runtime_yield_return".into()))
            }
            Ast::CoroutineInit(_, ref co, params) => {
                // Check if function exists and if the right number of parameters are being
                // passed
                Compiler::validate_routine_call(co, params, function_table).unwrap();

                // evaluate each paramater then store in registers Eax, Ebx, Ecx, Edx before
                // calling the function
                if params.len() > co_param_registers.len() {
                    panic!("Compiler: too many parameters being passed to function");
                }
                for param in params.iter() {
                    Compiler::traverse(param, current_func, function_table, output);
                    output.push(Push(Eax));
                }
                for reg in co_param_registers.iter().take(params.len()).rev() {
                    output.push(Pop(*reg));
                }

                output.push(Lea(
                    Location::Register(Eax),
                    Source::Memory(format!("{}", co)),
                ));
                output.push(Call("runtime_init_coroutine".into()));

                // Move parameters into the stack frame of the coroutine
                let mut idx = 0;
                for (_, reg) in params.iter().zip(co_param_registers.iter()) {
                    let param_offset = function_table.funcs[co].vars.vars[idx].frame_offset;
                    output.push(Mov(
                        Location::Memory(format!("eax-{}", param_offset)),
                        Source::Register(*reg),
                    ));
                    idx += 1;
                }
            }
            Ast::FunctionDef(_, ref fn_name, params, _, stmts) => {
                output.push(Label(fn_name.clone()));

                // Prepare stack frame for this function
                output.push(Push(Ebp));
                output.push(Mov(Location::Register(Ebp), Source::Register(Esp)));
                let total_offset = function_table.get_total_offset(fn_name).unwrap_or(0);
                output.push(Sub(Esp, Source::Integer(total_offset)));

                // Move function parameters from registers into the stack frame
                if params.len() > fn_param_registers.len() {
                    panic!("Compiler: too many parameters in function definition");
                }
                for (param, reg) in params.iter().zip(fn_param_registers.iter()) {
                    let param_offset = function_table.get_param_offset(fn_name, &param.0).unwrap_or(0);
                    output.push(Mov(
                        Location::Memory(format!("ebp-{}", param_offset)),
                        Source::Register(*reg),
                    ));
                }

                for s in stmts.iter() {
                    Compiler::traverse(s, fn_name, function_table, output);
                }

                // Clean up frame before exiting program
                output.push(Mov(Location::Register(Esp), Source::Register(Ebp)));
                output.push(Pop(Ebp));

                output.push(Ret);
            }
            Ast::FunctionCall(_, ref fn_name, params) => {
                // Check if function exists and if the right number of parameters are being
                // passed
                Compiler::validate_routine_call(fn_name, params, function_table).unwrap();

                // evaluate each paramater then store in registers Eax, Ebx, Ecx, Edx before
                // calling the function
                if params.len() > fn_param_registers.len() {
                    panic!("Compiler: too many parameters being passed to function");
                }
                for param in params.iter() {
                    Compiler::traverse(param, current_func, function_table, output);
                    output.push(Push(Eax));
                }
                for reg in fn_param_registers.iter().take(params.len()).rev() {
                    output.push(Pop(*reg));
                }
                output.push(Call(fn_name.clone()));
            }
            node => panic!("Expected an operator, found {:?}", node),
        }
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
