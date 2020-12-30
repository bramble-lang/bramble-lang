// ASM - types capturing the different assembly instructions along with functions to
// convert to text so that a compiled program can be saves as a file of assembly
// instructions
use crate::assembly;
use crate::assembly2;
use crate::ast::Ast;
use crate::ast::RoutineCall;
use crate::ast::RoutineDef;
use crate::binary_op;
use crate::compiler::ast::ast::CompilerNode;
use crate::compiler::ast::scope::Level::Routine;
use crate::compiler::ast::scope::Scope;
use crate::compiler::ast::stack::ScopeStack;
use crate::compiler::ast::stringpool::StringPool;
use crate::compiler::x86::assembly::*;
use crate::operand;
use crate::reg32;
use crate::register;
use crate::semantics::semanticnode::SemanticNode;
use crate::unary_op;
use crate::unit_op;
use crate::{
    ast::{BinaryOperator, UnaryOperator},
    syntax::ast::Type,
};

pub struct Compiler<'a> {
    code: Vec<Inst>,
    scope: ScopeStack<'a>,
    string_pool: StringPool,
    anonymous_counter: u32,
}

impl<'a> Compiler<'a> {
    pub fn print(code: &Vec<Inst>, output: &mut dyn std::io::Write) -> std::io::Result<()> {
        for inst in code.iter() {
            writeln!(output, "{}", inst)?;
        }
        Ok(())
    }

    pub fn compile(ast: &SemanticNode) -> Vec<Inst> {
        // Put user code here
        let (compiler_ast, _) = CompilerNode::from(ast);

        let mut string_pool = StringPool::new();
        string_pool.extract_from(&compiler_ast);

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
            anonymous_counter: 0,
        };

        let global_func = "".into();
        compiler
            .traverse(&compiler_ast, &global_func, &mut code)
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
                data next_stack_addr: dd 0;
                data stack_size: dd 8*1024;
                {{Compiler::write_string_pool(&string_pool)}}
            }
        };

        code
    }

    fn write_string_pool(string_pool: &StringPool) -> Vec<Inst> {
        let mut code = vec![];
        code.push(Inst::DataString("_i32_fmt".into(), "%d\\n".into()));
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
                    push %ebp;
                    mov %ebp, %esp;
                    cmp %eax, 0;
                    jz ^false;
                    push @_true;
                    jmp ^done;
                    ^false:
                    push @_false;
                    ^done:
                    {{Compiler::make_c_extern_call("printf", 1)}}
                    mov %esp, %ebp;
                    pop %ebp;
                    ret;
            }
        }
    }

    fn make_c_extern_call(c_func: &str, nparams: i32) -> Vec<Inst> {
        let mut code = vec![];
        assembly! {(code){
            {{Compiler::reverse_params_on_stack(nparams)}}
            call @{c_func};
            add %esp, {4*nparams as i32};
        }}
        code
    }

    /// The GCC32 uses a different order for parameters from the order that
    /// Braid pushes parameters onto the stack as they are evaluated.  This
    /// function reverses the order of the parameters.
    fn reverse_params_on_stack(nparams: i32) -> Vec<Inst> {
        let mut code = vec![];
        for pl in 0..nparams {
            let pr = nparams - pl - 1;
            if pr <= pl {
                break;
            }
            assembly! {(code){
                mov %esi, [%esp+{4*pl as i32}];
                mov %edi, [%esp+{4*pr as i32}];
                mov [%esp+{4*pl as i32}], %edi;
                mov [%esp+{4*pr as i32}], %esi;
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
                    push %ebp;
                    mov %ebp, %esp;
                    mov %esp, [@{next_stack_variable}];
                    ; "[-4]: The RIP for the coroutine"
                    ; "[-8]: The ESP for the caller"
                    ; "[-12]: The EBP for the caller"
                    ; "[-16]: The caller return address (for yield return)"
                    ; "[-20]: The coroutine ESP"
                    mov [%esp-4], %eax;
                    mov [%esp-8], 0;
                    mov [%esp-12], 0;
                    mov [%esp-16], 0;
                    mov %eax, %esp;
                    sub %eax, %edi;
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
        assembly! {
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
                    mov [%ebp-20], %esp;
                    mov [%ebp-4], %ebx;
                    mov %esp, [%ebp-8];
                    mov %ebx, [%ebp-16];
                    mov %ebp, [%ebp-12];
                    jmp %ebx;
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
                assembly! {(op_asm) {add %eax, %ebx;}}
            }
            BinaryOperator::Sub => {
                assembly! {(op_asm) {sub %eax, %ebx;}}
            }
            BinaryOperator::Mul => {
                assembly! {(op_asm) {imul %eax, %ebx;}}
            }
            BinaryOperator::Div => {
                assembly! {(op_asm) {
                    cdq;
                    idiv %ebx;
                }}
            }
            BinaryOperator::BAnd => {
                assembly! {(op_asm) {and %eax, %ebx;}}
            }
            BinaryOperator::BOr => {
                assembly! {(op_asm) {or %eax, %ebx;}}
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
                    cmp %eax, %ebx;
                    {{[set]}}
                    and %al, 1;
                }}
            }
        };

        let mut code = vec![];
        assembly! {(code){
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
    ) -> Result<(), String> {
        // The registers used for passing function parameters, in the order that parameters are
        // assigned to registers
        let fn_param_registers = vec![
            Reg::R32(Reg32::Eax),
            Reg::R32(Reg32::Ebx),
            Reg::R32(Reg32::Ecx),
            Reg::R32(Reg32::Edx),
        ];
        let co_param_registers = vec![
            Reg::R32(Reg32::Ebx),
            Reg::R32(Reg32::Ecx),
            Reg::R32(Reg32::Edx),
        ];

        self.push_scope(ast);

        match ast {
            Ast::Integer(_, i) => {
                assembly! {(code) {mov %eax, {*i};}}
            }
            Ast::Boolean(_, b) => {
                assembly! {(code) {mov %eax, {if *b {1} else {0}};}}
            }
            Ast::StringLiteral(_, s) => {
                let str_id = self
                    .string_pool
                    .get(s)
                    .ok_or(format!("Could not find string {} in string pool", s))?;
                assembly! {(code) {
                        lea %eax, @{format!("str_{}", str_id)};
                    }
                }
            }
            Ast::Identifier(m, id) => {
                let id_offset = self.scope.find(id).unwrap().offset;
                match m.ty() {
                    Type::Custom(_) => {
                        assembly! {(code) {lea %eax, [%ebp-{id_offset}];}}
                    }
                    _ => {
                        assembly! {(code) {mov %eax, [%ebp-{id_offset}];}}
                    }
                }
            }
            Ast::MemberAccess(_, src, member) => {
                self.member_access(current_func, code, src, member)?;
            }
            Ast::UnaryOp(_, op, operand) => {
                self.traverse(operand, current_func, code)?;
                match op {
                    UnaryOperator::Minus => {
                        assembly! {(code){
                            neg %eax;
                        }}
                    }
                    UnaryOperator::Not => {
                        assembly! {(code){
                            xor %eax, 1;
                            movzx %eax, %al;
                        }}
                    }
                }
            }
            Ast::BinaryOp(_, op, l, r) => {
                assembly! {(code) {
                    {{self.handle_binary_operands(*op, l.as_ref(), r.as_ref(), current_func)?}}
                }}
            }
            Ast::Printiln(_, ref exp) => {
                self.traverse(exp, current_func, code)?;

                assembly! {(code) {
                    push @_i32_fmt;
                    push %eax;
                    {{Compiler::make_c_extern_call("printf", 2)}}
                }}
            }
            Ast::Prints(_, ref exp) => {
                self.traverse(exp, current_func, code)?;

                assembly! {(code) {
                    push %eax;
                    push [rel @stdout];
                    {{Compiler::make_c_extern_call("fputs", 2)}}
                }}
            }
            Ast::Printbln(_, ref exp) => {
                self.traverse(exp, current_func, code)?;

                assembly! {(code) {
                    call @print_bool;
                }}
            }
            Ast::If(meta, ref cond, ref true_arm, ref false_arm) => {
                let mut cond_code = vec![];
                self.traverse(cond, current_func, &mut cond_code)?;
                let mut true_code = vec![];
                self.traverse(true_arm, current_func, &mut true_code)?;
                let mut false_code = vec![];
                self.traverse(false_arm, current_func, &mut false_code)?;

                assembly2! {(code, meta) {
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
                let id_offset = self
                    .scope
                    .find(id)
                    .ok_or(format!("Could not find variable {}", id))?
                    .offset;
                assembly! {(code) {
                    ; {format!("Binding {}", id)}
                    {{self.bind(exp, current_func, Reg32::Ebp, id_offset)?}}
                }}
            }
            Ast::Module {
                functions,
                coroutines,
                ..
            } => {
                for f in functions.iter() {
                    self.traverse(f, current_func, code)?;
                }
                for co in coroutines.iter() {
                    self.traverse(co, current_func, code)?;
                }
            }
            Ast::Return(_, ref exp) => {
                assembly! {(code) {
                    {{self.return_exp(exp, current_func)?}}
                }}
            }
            Ast::Yield(meta, ref id) => {
                assembly! {(code) {
                    {{self.yield_exp(meta, id, current_func)?}}
                }}
            }
            Ast::YieldReturn(meta, ref exp) => {
                assembly! {(code) {
                    {{self.yield_return(meta, exp, current_func)?}}
                }}
            }
            Ast::RoutineDef(_, RoutineDef::Coroutine, ref fn_name, _, _, stmts) => {
                assembly! {(code) {
                @{fn_name}:
                }};

                // Prepare stack frame for this function
                for s in stmts.iter() {
                    self.traverse(s, fn_name, code)?;
                }
                assembly! {(code) {
                    jmp @runtime_yield_return;
                }};
            }
            Ast::RoutineCall(_, RoutineCall::CoroutineInit, ref co, params) => {
                let total_offset = self
                    .scope
                    .get_routine_allocation(co)
                    .ok_or(format!("Coroutine {} has not allocation size", co))?;

                self.validate_routine_call(co, params)?;

                assembly! {(code) {
                    {{self.evaluate_routine_params(params, current_func)?}}
                    {{self.move_routine_params_into_registers(params, &co_param_registers)?}}
                    ; "Load the IP for the coroutine (EAX) and the stack frame allocation (EDI)"
                    lea %eax, [@{co}];
                    mov %edi, {total_offset};
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
                    Routine { allocation, .. } => allocation,
                    _ => panic!("Invalid scope for function definition"),
                };

                assembly! {(code) {
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

                assembly! {(code) {
                    ; "Clean up frame before leaving function"
                    mov %esp, %ebp;
                    pop %ebp;
                    ret;
                }};
            }
            Ast::RoutineCall(meta, RoutineCall::Function, ref fn_name, params) => {
                // Check if function exists and if the right number of parameters are being
                // passed
                self.validate_routine_call(fn_name, params)?;
                self.scope.find_func(fn_name);

                let return_type = meta.ty();
                if let Type::Custom(_) = return_type {
                    let st_sz = self
                        .scope
                        .size_of(return_type)
                        .ok_or(format!("no size for {} found", return_type))?;

                    assembly! {(code){
                        sub %esp, {st_sz};
                    }}
                }

                // evaluate each paramater then store in registers Eax, Ebx, Ecx, Edx before
                // calling the function
                assembly! {(code) {
                    {{self.evaluate_routine_params(params, current_func)?}}
                    {{self.move_routine_params_into_registers(params, &fn_param_registers)?}}
                    call @{fn_name};
                }};
            }
            Ast::StructExpression(_, struct_name, fields) => {
                let anonymous_name = format!("!{}_{}", struct_name, self.anonymous_counter);
                self.anonymous_counter += 1;
                //let line = m.line();
                //println!("Struct:{}: {}: {}", line, anonymous_name, struct_name);
                let anonymous_offset = self
                    .scope
                    .find(&anonymous_name)
                    .expect(&format!(
                        "Anonymous Struct Expression, {}, is not in symbol table: \n{}",
                        anonymous_name,
                        self.scope,
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

                let st = self
                    .scope
                    .get_struct(struct_name)
                    .ok_or(format!("no definition for {} found", struct_name))?;
                let field_info = st
                    .get_fields()
                    .iter()
                    .find(|(n, ..)| n == member)
                    .ok_or(format!("member {} not found on {}", member, struct_name))?;
                let field_offset = st.get_offset_of(member).ok_or(format!(
                    "No field offset found for {}.{}",
                    struct_name, member
                ))?;

                match &field_info.1 {
                    Type::Custom(_substruct_name) => {
                        assembly! {(code) {
                            lea %eax, [%eax+{field_offset}];
                        }}
                    }
                    _ => {
                        assembly! {(code) {
                            mov %eax, [%eax+{field_offset}];
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
        struct_name: &str,
        field_values: &'a Vec<(String, CompilerNode)>,
        offset: i32,
        allocate: bool,
    ) -> Result<Vec<Inst>, String> {
        let st = self
            .scope
            .get_struct(struct_name)
            .ok_or(format!("no definition for {} found", struct_name))?;
        let struct_sz = st.size.unwrap();
        let field_info = st
            .get_fields()
            .iter()
            .map(|(n, _, o)| (n.clone(), o.unwrap()))
            .collect::<Vec<(String, i32)>>();

        if field_values.len() != field_info.len() {
            return Err(format!(
                "{} expected {} fields but found {}",
                struct_name,
                st.get_fields().len(),
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
                sub %esp, {struct_sz};
            }};
        }
        for (fname, fvalue) in field_values.iter() {
            let field_offset = field_info.iter().find(|(n, _)| n == fname).unwrap().1;
            let relative_offset = offset - (struct_sz - field_offset);
            assembly! {(code) {
                {{self.bind_member(fvalue, current_func, Reg32::Ebp, relative_offset)?}}
            }}
        }

        assembly! {(code) {
            ; {format!("Done instantiating struct of type {}", struct_name)}
            lea %eax, [%ebp - {offset}];
        }};
        Ok(code)
    }

    fn bind_member(
        &mut self,
        fvalue: &'a CompilerNode,
        current_func: &String,
        dst: Reg32,
        dst_offset: i32,
    ) -> Result<Vec<Inst>, String> {
        let mut code = vec![];

        match fvalue {
            Ast::StructExpression(_, substruct_name, substruct_values) => {
                self.anonymous_counter += 1;
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
                            Reg::R32(Reg32::Eax),
                            0,
                        )?;
                        assembly! {(code){
                            {{asm}}
                        }};
                    }
                    _ => {
                        assembly! {(code) {
                            mov [%{Reg::R32(dst)}-{dst_offset}], %eax;
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
        dst: Reg32,
        dst_offset: i32,
    ) -> Result<Vec<Inst>, String> {
        let mut code = vec![];
        self.traverse(value, current_func, &mut code)?;

        match value.get_metadata().ty() {
            Type::Custom(name) => {
                match value {
                    Ast::Identifier(..) => {
                        // If an identifier is being copied to another identifier, then just copy
                        // the data over rather than pop off of the stack
                        let asm =
                            self.copy_struct_into(name, dst, dst_offset, Reg::R32(Reg32::Eax), 0)?;
                        assembly! {(code){
                            {{asm}}
                        }}
                    }
                    _ => {
                        assembly! {(code){
                            // TODO: This seems wrong?
                            //{{self.pop_struct_into(name, dst_offset as u32)?}}
                            {{self.copy_struct_into(name, dst, dst_offset, Reg::R32(Reg32::Eax), 0)?}}
                        }}
                    }
                }
            }
            _ => {
                assembly! {(code) {
                    mov [%{Reg::R32(dst)}-{dst_offset}], %eax;
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
                    .scope
                    .get_struct(struct_name)
                    .ok_or(format!("no definition for {} found", struct_name))?;
                let st_sz = st
                    .size
                    .ok_or(format!("struct {} has no resolved size", struct_name))?;
                assembly! {(code){
                    sub %esp, {st_sz};
                }}
            }
            _ => (),
        }
        assembly2! {(code, meta) {
            mov %ebx, ^ret_lbl;
            jmp @runtime_yield_into_coroutine;
            ^ret_lbl:
        }};
        Ok(code)
    }

    fn yield_return(
        &mut self,
        meta: &'a Scope,
        exp: &'a Option<Box<CompilerNode>>,
        current_func: &String,
    ) -> Result<Vec<Inst>, String> {
        let mut code = vec![];
        if let Some(exp) = exp {
            self.traverse(exp, current_func, &mut code)?;
            match exp.get_metadata().ty() {
                Type::Custom(struct_name) => {
                    // Copy the structure into the stack frame of the calling function
                    let asm =
                        self.copy_struct_into(struct_name, Reg32::Esi, 0, Reg::R32(Reg32::Eax), 0)?;
                    assembly! {(code){
                        mov %esi, [%ebp-8];
                        {{asm}}
                    }};
                }
                _ => (),
            }
        }

        assembly2! {(code, meta) {
            mov %ebx, ^ret_lbl;
            jmp @runtime_yield_return;
            ^ret_lbl:
        }};
        Ok(code)
    }

    fn return_exp(
        &mut self,
        exp: &'a Option<Box<CompilerNode>>,
        current_func: &String,
    ) -> Result<Vec<Inst>, String> {
        let mut code = vec![];
        match exp {
            Some(e) => {
                self.traverse(e, current_func, &mut code)?;
                match e.get_metadata().ty() {
                    Type::Custom(struct_name) => {
                        // Copy the structure into the stack frame of the calling function
                        let asm = self.copy_struct_into(
                            struct_name,
                            Reg32::Esi,
                            0,
                            Reg::R32(Reg32::Eax),
                            0,
                        )?;

                        let is_coroutine = self.scope.find_coroutine(current_func).is_some();
                        if is_coroutine {
                            assembly! {(code){
                                mov %esi, [%ebp-8];
                                {{asm}}
                            }};
                        } else {
                            assembly! {(code){
                                lea %esi, [%ebp+8];
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

    fn pop_struct_into(&self, struct_name: &str, id_offset: u32) -> Result<Vec<Inst>, String> {
        let mut code = vec![];
        let ty_def = self
            .scope
            .get_struct(struct_name)
            .ok_or(format!("Could not find definition for {}", struct_name))?;
        for (field_name, field_ty, _) in ty_def.get_fields().iter().rev() {
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
                        pop %eax;
                        mov [%ebp-{field_offset as i32}], %eax;
                    }};
                }
            }
        }

        Ok(code)
    }

    fn copy_struct_into(
        &self,
        struct_name: &str,
        dst_reg: Reg32,
        dst_offset: i32,
        src_reg: Reg,
        src_offset: i32,
    ) -> Result<Vec<Inst>, String> {
        let mut code = vec![];
        let ty_def = self
            .scope
            .get_struct(struct_name)
            .ok_or(format!("Could not find definition for {}", struct_name))?;
        let struct_sz = ty_def
            .size
            .ok_or(format!("struct {} has an unknown size", struct_name))?;
        for (field_name, field_ty, field_offset) in ty_def.get_fields().iter().rev() {
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
                        mov %edi, [%{src_reg}-{src_offset - (struct_sz - rel_field_offset)}];
                        mov [%{Reg::R32(dst_reg)}-{dst_field_offset}], %edi;
                    }};
                }
            }
        }

        Ok(code)
    }

    fn move_params_into_stackframe(
        &self,
        routine: &CompilerNode,
        param_registers: &Vec<Reg>,
    ) -> Result<Vec<Inst>, String> {
        let routine_name = routine
            .get_name()
            .ok_or("Critical: treating a node without a name as a routine node")?;
        let params = routine.get_params().ok_or(format!(
            "Critical: node for {} does not have a params field",
            routine_name
        ))?;
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
                        Reg32::Ebp,
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
                        mov [%ebp-{param_offset}], %{param_registers[idx]};
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
                push %eax;
            }};
        }
        Ok(code)
    }

    fn move_routine_params_into_registers(
        &mut self,
        params: &'a Vec<CompilerNode>,
        param_registers: &Vec<Reg>,
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
                pop %{*reg};
            }};
        }
        Ok(code)
    }

    fn validate_routine_call(
        &self,
        routine_name: &str,
        params: &Vec<CompilerNode>,
    ) -> Result<(), String> {
        let routine = self
            .scope
            .find_func(routine_name)
            .or_else(|| self.scope.find_coroutine(routine_name))
            .expect("Could not find routine in any symbol table in the scope stack");
        let expected_params = routine.get_params().ok_or(format!(
            "Critical: node for {} does not have a params field",
            routine_name
        ))?;

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
