// ASM - types capturing the different assembly instructions along with functions to
// convert to text so that a compiled program can be saves as a file of assembly
// instructions
use crate::{syntax::ast::Type, ast::{BinaryOperator, UnaryOperator}};
use crate::ast::RoutineDef;
use crate::ast::RoutineCall;
use crate::compiler::ast::ast::CompilerNode;
use crate::compiler::ast::scope::Level::Routine;
use crate::compiler::ast::stack::ScopeStack;
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
use crate::reg32;

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
        let mut code = vec![];

        Compiler::create_base(&mut code);

        Compiler::coroutine_init("next_stack_addr", "stack_size", &mut code);
        Compiler::runtime_yield_into_coroutine( &mut code);
        Compiler::runtime_yield_return( &mut code);
        Compiler::print_bool( &mut code);

        // Put user code here
        let global_func = "".into();
        let (compiler_ast, _) = CompilerNode::from(ast);
        let mut compiler = Compiler { code:vec![], scope: ScopeStack::new() };
        compiler.traverse(&compiler_ast, &global_func, &mut code).unwrap();
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

    fn handle_binary_operands(&mut self, op: BinaryOperator, left: &'a CompilerNode, right: &'a CompilerNode, current_func: &String) -> Result<Vec<Inst>, String> {
        use BinaryOperator::*;

        let mut left_code = vec![];
        self.traverse(left, current_func, &mut left_code)?;
        let mut right_code = vec![];
        self.traverse(right, current_func, &mut right_code)?;

        let mut op_asm = vec![];
        match op {
                BinaryOperator::Add => {assembly!{(op_asm) {add %eax, %ebx;}}},
                BinaryOperator::Sub => {assembly!{(op_asm) {sub %eax, %ebx;}}},
                BinaryOperator::Mul => {assembly!{(op_asm) {imul %eax, %ebx;}}},
                BinaryOperator::Div => {assembly!{(op_asm) {
                    cdq;
                    idiv %ebx;
                }}},
                BinaryOperator::BAnd => {assembly!{(op_asm) {and %eax, %ebx;}}},
                BinaryOperator::BOr => {assembly!{(op_asm) {or %eax, %ebx;}}},
                cond => {
                    let set = match cond {
                        Eq => Inst::Sete(Reg8::Al),
                        NEq => Inst::Setne(Reg8::Al),
                        Ls => Inst::Setl(Reg8::Al),
                        LsEq => Inst::Setle(Reg8::Al),
                        Gr => Inst::Setg(Reg8::Al),
                        GrEq => Inst::Setge(Reg8::Al),
                        _ => panic!("Invalid conditional operator: {}", cond)
                    };
                    assembly!{(op_asm){
                        cmp %eax, %ebx;
                        {{[set]}}
                        and %al, 1;
                    }}
                }
        };

        let mut code = vec![];
        assembly!{(code){
            {{left_code}}
            push %eax;
            {{right_code}}
            push %eax;
            pop %ebx;
            pop %eax;
            {{op_asm}}
        }}

        Ok(code)
    }

    fn push_scope(&mut self, ast: &'a CompilerNode) {
        self.scope.push(ast)
    }

    fn pop(&mut self) {
        self.scope.pop();
    }

    fn traverse(
        &mut self,
        ast: &'a CompilerNode,
        current_func: &String,
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
            Ast::Identifier(m, id) => {
                let id_offset = self.scope.find(id).unwrap().offset;
                match m.ty() {
                    Type::Custom(struct_name) => {
                        let st = self.scope.find_struct(struct_name).ok_or(format!("no definition for {} found", struct_name))?;
                        let struct_sz = st.size.unwrap();
                        let offset = id_offset - struct_sz;
                        assembly!{(code) {lea %eax, [%ebp-{offset as u32}];}}
                    }
                    _ => {
                        assembly!{(code) {mov %eax, [%ebp-{id_offset as u32}];}}
                    }
                }
            }
            Ast::MemberAccess(_, src, member) => {
                let src_ty = src.get_metadata().ty();
                match src_ty {
                    Type::Custom(struct_name) => {
                        code.push(Inst::Comment(format!("{}.{}", struct_name, member)));
                        self.traverse(src, current_func, code)?;
                        let st = self.scope.find_struct(struct_name).ok_or(format!("no definition for {} found", struct_name))?;
                        let field_info = st.fields().iter().find(|(n,..)| n == member).ok_or(format!("member {} not found on {}", member, struct_name))?;
                        let field_relative_offset = field_info.2.ok_or(format!("No field offset found for {}.{}", struct_name, member))?;
                        assembly!{(code) {
                            mov %eax, [%eax-{field_relative_offset as u32}];
                        }}
                    }
                    _ => return Err(format!("Attempting to access a member on a type which has no members: {}", src_ty)),
                }
            }
            Ast::UnaryOp(_, op, operand) => {
                self.traverse(operand, current_func, code)?;
                match op {
                    UnaryOperator::Minus => {
                        assembly!{(code){
                            neg %eax;
                        }}
                    }
                    UnaryOperator::Not => {
                        assembly!{(code){
                            xor %eax, 1;
                            movzx %eax, %al;
                        }}
                    }
                }
            }
            Ast::BinaryOp(_, op, l, r) => {
                assembly!{(code) {
                    {{self.handle_binary_operands(*op, l.as_ref(), r.as_ref(), current_func)?}}
                }}
            }
            Ast::Printiln(_, ref exp) => {
                self.traverse(exp, current_func, code)?;
                
                assembly!{(code) {
                    print_dec %eax;
                    newline;
                }}
            }
            Ast::Printbln(_, ref exp) => {
                self.traverse(exp, current_func, code)?;
                
                assembly!{(code) {
                    call @print_bool;
                    newline;
                }}
            }
            Ast::If(meta, ref cond, ref true_arm, ref false_arm) => {
                let mut cond_code = vec![];
                self.traverse(cond, current_func, &mut cond_code)?;
                let mut true_code = vec![];
                self.traverse(true_arm, current_func, &mut true_code)?;
                let mut false_code = vec![];
                self.traverse(false_arm, current_func, &mut false_code)?;
                
                assembly2!{(code, meta) {
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
                    self.traverse(s, current_func, code)?;
                }
            }
            Ast::Statement(_, stm) => {
                self.traverse(stm, current_func, code)?;
            }
            Ast::Bind(_, id, .., ref exp) | Ast::Mutate(_, id, ref exp) => {
                let id_offset = self.scope.find(id).ok_or(format!("Could not find variable {}", id))?.offset;
                self.traverse(exp, current_func, code)?;

                code.push(Inst::Comment(format!("Binding {}", id)));

                match exp.get_metadata().ty() {
                    Type::Custom(name) => {
                        assembly!{(code){
                            {{self.copy_struct(name, id_offset as u32)?}}
                        }}
                    },
                    _ => {
                        assembly!{(code) {
                            mov [%ebp-{id_offset as u32}], %eax;
                        }};
                    },
                }
            }
            Ast::Module{functions, coroutines, ..} => {
                for f in functions.iter() {
                    self.traverse(f, current_func, code)?;
                }
                for co in coroutines.iter() {
                    self.traverse(co, current_func, code)?;
                }
            }
            Ast::Return(_, ref exp) => match exp {
                Some(e) => self.traverse(e, current_func, code)?,
                None => (),
            },
            Ast::Yield(meta, ref id) => {
                self.traverse(id, current_func, code)?;
                assembly2!{(code, meta) {
                    mov %ebx, ^ret_lbl;
                    jmp @runtime_yield_into_coroutine;
                    ^ret_lbl:
                }};
            }
            Ast::YieldReturn(meta, ref exp) => {
                if let Some(exp) = exp {
                    self.traverse(exp, current_func, code)?;
                }
                
                assembly2!{(code, meta) {
                    mov %ebx, ^ret_lbl;
                    jmp @runtime_yield_return;
                    ^ret_lbl:
                }};
            }
            Ast::RoutineDef(_, RoutineDef::Coroutine, ref fn_name, _, _, stmts) => {
                assembly!{(code) {
                @{fn_name}:
                }};

                // Prepare stack frame for this function
                for s in stmts.iter() {
                    self.traverse(s, fn_name, code)?;
                }
                assembly!{(code) {
                    jmp @runtime_yield_return;
                }};
            }
            Ast::RoutineCall(_, RoutineCall::CoroutineInit, ref co, params) => {
                self.validate_routine_call(co, params)?;


                assembly!{(code) {
                    {{self.evaluate_routine_params(params, &co_param_registers, current_func)?}}
                    lea %eax, [@{co}];
                    call @runtime_init_coroutine;
                    ; "move into coroutine's stack frame"
                    push %ebp;
                    mov %ebp, %eax;
                    ; "move parameters into the stack frame of the coroutine"
                    {{{
                        let codef = self.scope.find_coroutine(co).ok_or(format!("Could not find {} when looking up parameter offsets", co))?;
                        self.move_params_into_stackframe(codef, &co_param_registers)?
                    }}}
                    ; "leave coroutine's stack frame"
                    pop %ebp;
                }};
            }
            Ast::RoutineDef(scope, RoutineDef::Function, ref fn_name, _, _, stmts) => {
                let total_offset = match scope.level() {
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
                    {{self.move_params_into_stackframe(ast, &fn_param_registers)?}}
                    ; "Done moving function parameters from registers into the stack frame"
                }};


                for s in stmts.iter() {
                    self.traverse(s, fn_name, code)?;
                }

                assembly!{(code) {
                    ; "Clean up frame before leaving function"
                    mov %esp, %ebp;
                    pop %ebp;
                    ret;
                }};

            }
            Ast::RoutineCall(_, RoutineCall::Function, ref fn_name, params) => {
                // Check if function exists and if the right number of parameters are being
                // passed
                self.validate_routine_call(fn_name, params)?;

                // evaluate each paramater then store in registers Eax, Ebx, Ecx, Edx before
                // calling the function
                assembly!{(code) {
                    {{self.evaluate_routine_params(params, &fn_param_registers, current_func)?}}
                    call @{fn_name};
                }};
            }
            Ast::StructInit(m, struct_name, fields) => {
                /*let st = self.scope.find_struct(struct_name).ok_or(format!("no definition for {} found", struct_name))?;
                println!("Struct: {:?}", st);
                let struct_sz = st.size.unwrap();
                let field_info = st.fields().iter().map(|(n, _, o)| (n.clone(), o.unwrap())).collect::<Vec<(String,i32)>>();

                if fields.len() != field_info.len() {
                    return Err(format!("{} expected {} fields but found {}", struct_name, st.fields().len(), fields.len()));
                }

                code.push(Inst::Comment(format!("Instantiate struct of type {}", struct_name)));
                for (fname, fvalue) in fields.iter() {
                    let relative_offset = field_info.iter().find(|(n, _)| n == fname).unwrap().1;
                    self.traverse(fvalue, current_func, code)?;
                    // use fname to look up the offset of the given field
                    assembly!{(code) {
                        mov [%esp-{relative_offset as u32}], %eax;
                    }};
                }*/
                let (asm, _struct_sz) = self.init_struct(current_func, struct_name, fields, 0, true)?;
                assembly!{(code){
                    {{asm}}
                }};
            }
            node => panic!("Expected an operator, found {:?}", node),
        };

        self.pop();

        Ok(())
    }

    fn init_struct(&mut self, current_func: &String, struct_name: &str, field_values: &'a Vec<(String,CompilerNode)>, offset: i32, allocate: bool) -> Result<(Vec<Inst>, i32), String> {
        let st = self.scope.find_struct(struct_name).ok_or(format!("no definition for {} found", struct_name))?;
        println!("Struct: {:?}", st);
        let struct_sz = st.size.unwrap();
        let field_info = st.fields().iter().map(|(n, _, o)| (n.clone(), o.unwrap())).collect::<Vec<(String,i32)>>();

        if field_values.len() != field_info.len() {
            return Err(format!("{} expected {} fields but found {}", struct_name, st.fields().len(), field_values.len()));
        }

        let mut code = vec![];
        code.push(Inst::Comment(format!("Instantiate struct of type {}", struct_name)));
        if allocate {
            assembly!{(code){
                sub %esp, {struct_sz};
            }};
        }
        for (fname, fvalue) in field_values.iter() {
            let field_offset = field_info.iter().find(|(n, _)| n == fname).unwrap().1;
            let relative_offset = struct_sz - field_offset + offset;
            match fvalue {
                Ast::StructInit(_, substruct_name, substruct_values) => {
                    println!("{}: struct_sz: {}, field_offset: {}, rel offset: {}", substruct_name, struct_sz, field_offset, relative_offset);
                    let (asm, _) = self.init_struct(current_func, substruct_name, substruct_values, relative_offset, false)?;
                    assembly!{(code){
                        {{asm}}
                    }};
                },
                _ => {
                    self.traverse(fvalue, current_func, &mut code)?;
                    assembly!{(code) {
                        mov [%esp+{relative_offset as u32}], %eax;
                    }};
                }
            }
        }
        code.push(Inst::Comment(format!("Done instantiating struct of type {}", struct_name)));
        Ok((code, struct_sz))
    }

    fn copy_struct(&self, name: &str, id_offset: u32) -> Result<Vec<Inst>, String> {
        let mut code = vec![];
        let ty_def = self.scope.find_struct(name).ok_or(format!("Could not find definition for {}", name))?;
        let struct_sz = ty_def.size.ok_or(format!("struct {} has an unknown size", name))? as u32;
        for (_, field_ty, field_offset) in ty_def.fields().iter().rev() {
            let field_offset = field_offset.expect(&format!("CRITICAL: struct {} has field with no relative offset", name)) as u32;
            match field_ty {
                Type::Custom(name) => {
                    assembly!{(code){
                        {{self.copy_struct(name, id_offset + (struct_sz - field_offset))?}}
                    }}
                }
                _ => {
                    assembly!{(code) {
                        pop %eax;
                        mov [%ebp-{id_offset as u32 - (struct_sz - field_offset)}], %eax;
                    }};
                }
            }
        }

        Ok(code)
    }

    fn move_params_into_stackframe(&self, routine: &CompilerNode, param_registers: &Vec<Reg>) -> Result<Vec<Inst>, String>{
        let routine_name = routine.get_name().ok_or("Critical: treating a node without a name as a routine node")?;
        let params = routine.get_params().ok_or(format!("Critical: node for {} does not have a params field", routine_name))?;
        if params.len() > param_registers.len() {
            return Err(format!("Compiler: too many parameters in function definition"));
        }

        let mut code = vec![];
        let routine_sym_table = routine.get_metadata();
        for idx in 0..params.len() {
            let param_offset = routine_sym_table.get(&params[idx].0).ok_or(format!("Critical: could not find parameter {} in symbol table for {}", params[idx].0, routine_name))?.offset;
            assembly!{(code){
                mov [%ebp-{param_offset as u32}], %{param_registers[idx]};
            }};
        }
        Ok(code)
    }

    fn evaluate_routine_params(&mut self, params: &'a Vec<CompilerNode>, param_registers: &Vec<Reg>, current_func: &String) -> Result<Vec<Inst>, String> {
        // evaluate each paramater then store in registers Eax, Ebx, Ecx, Edx before
        // calling the function
        if params.len() > param_registers.len() {
            return Err(format!("Compiler: too many parameters being passed to function"));
        }
        let mut code = vec![];
        for param in params.iter() {
            self.traverse(param, current_func, &mut code)?;
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

    fn validate_routine_call(&self, routine_name: &str, params: &Vec<CompilerNode>) -> Result<(),String>{
        let routine = self.scope.find_func(routine_name).or_else(|| self.scope.find_coroutine(routine_name)).expect("Could not find routine in any symbol table in the scope stack");
        let expected_params = routine.get_params().ok_or(format!("Critical: node for {} does not have a params field", routine_name))?;

        let expected_num_params = expected_params.len();
        let got_num_params = params.len();
        if expected_num_params != got_num_params {
            Err(format!(
                "Compiler: expected {} but got {} parameters for function/coroutine `{}`",
                expected_num_params, got_num_params, routine_name
            ))
        } else {
            Ok(())
        }
    }
}
