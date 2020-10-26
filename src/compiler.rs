// ASM - types capturing the different assembly instructions along with functions to
// convert to text so that a compiled program can be saves as a file of assembly
// instructions
#[derive(Debug, Copy, Clone)]
enum Register {
    Eax,
    Ebx,
    Ecx,
    Edx,
    Ebp,
    Esp,
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::Eax => f.write_str("eax"),
            Register::Ebx => f.write_str("ebx"),
            Register::Ecx => f.write_str("ecx"),
            Register::Edx => f.write_str("edx"),
            Register::Ebp => f.write_str("ebp"),
            Register::Esp => f.write_str("esp"),
        }
    }
}

#[derive(Debug)]
enum Location {
    Register(Register),
    Memory(String),
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Location::Register(reg) => {
                let s = format!("{}", reg);
                f.write_str(&s)
            }
            Location::Memory(m) => {
                let s = format!("[{}]", m);
                f.write_str(&s)
            }
        }
    }
}

#[derive(Debug)]
enum Source {
    Register(Register),
    Memory(String),
    Address(String),
    Integer(i32),
}

impl std::fmt::Display for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Source::Register(reg) => {
                let s = format!("{}", reg);
                f.write_str(&s)
            }
            Source::Memory(m) => {
                let s = format!("[{}]", m);
                f.write_str(&s)
            }
            Source::Address(m) => {
                let s = format!("{}", m);
                f.write_str(&s)
            }
            Source::Integer(i) => {
                let s = format!("{}", i);
                f.write_str(&s)
            }
        }
    }
}

type Label = String;

#[derive(Debug)]
enum Instr {
    Jmp(Label),
    Call(Label),
    Ret,
    Mov(Location, Source),
    Lea(Location, Source),
    Add(Register, Source),
    Sub(Register, Source),
    IMul(Register, Location),
    Push(Register),
    Pop(Register),
    Print(Source),
    Newline,
}

#[derive(Debug)]
enum Assembly {
    Label(Label),
    Instr(Instr),
    Include(String),
    Section(String),
    Global(String),
    Data(String, i32),
}

pub struct Compiler {
    code: Vec<Assembly>,
}

impl Compiler {
    pub fn print(&self, output: &mut dyn std::io::Write) -> std::io::Result<()> {
        for inst in self.code.iter() {
            match inst {
                Assembly::Include(include) => {
                    writeln!(output, "%include \"{}\"", include)?;
                }
                Assembly::Section(section) => {
                    writeln!(output, "\nsection {}", section)?;
                }
                Assembly::Global(global) => {
                    writeln!(output, "global {}", global)?;
                }
                Assembly::Data(label, value) => {
                    writeln!(output, "{}: dd {}", label, value)?;
                }
                Assembly::Label(label) => {
                    if !label.starts_with(".") {
                        writeln!(output)?;
                    }
                    writeln!(output, "{}:", label)?;
                }
                Assembly::Instr(inst) => {
                    write!(output, "    ")?;
                    match inst {
                        Instr::Mov(l, Source::Memory(s)) => {
                            writeln!(output, "mov {}, DWORD [{}]", l, s)?
                        }
                        Instr::Mov(l, Source::Address(s)) => writeln!(output, "mov {}, {}", l, s)?,
                        Instr::Mov(l, Source::Integer(s)) => {
                            writeln!(output, "mov {}, DWORD {}", l, s)?
                        }
                        Instr::Mov(l, Source::Register(s)) => writeln!(output, "mov {}, {}", l, s)?,
                        Instr::Lea(l, s) => writeln!(output, "lea {}, {}", l, s)?,
                        Instr::Push(reg) => {
                            writeln!(output, "push {}", reg)?;
                        }
                        Instr::Pop(reg) => {
                            writeln!(output, "pop {}", reg)?;
                        }
                        Instr::IMul(reg, s) => {
                            writeln!(output, "imul {}, {}", reg, s)?;
                        }
                        Instr::Add(reg, s) => {
                            writeln!(output, "add {}, {}", reg, s)?;
                        }
                        Instr::Sub(reg, s) => {
                            writeln!(output, "sub {}, {}", reg, s)?;
                        }
                        Instr::Jmp(loc) => {
                            writeln!(output, "jmp {}", loc)?;
                        }
                        Instr::Call(label) => writeln!(output, "call {}", label)?,
                        Instr::Ret => writeln!(output, "ret")?,
                        Instr::Print(s) => {
                            writeln!(output, "PRINT_DEC 4, {}", s)?;
                        }
                        Instr::Newline => {
                            writeln!(output, "NEWLINE")?;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub fn compile(ast: &super::Node, funcs: &mut super::FunctionTable) -> Compiler {
        let mut code = vec![];

        Compiler::create_base(&mut code);

        Compiler::coroutine_init("next_stack_addr", "stack_size", &mut code);
        Compiler::runtime_yield_into_coroutine(&mut code);
        Compiler::runtime_yield_return(&mut code);

        // Put user code here
        let global_func = "".into();
        Compiler::traverse(ast, &global_func, funcs, &mut code);
        Compiler { code }
    }

    /// Creates the runtime code that will manage the entire execution of this program.
    fn create_base(output: &mut Vec<Assembly>) {
        use Register::*;
        // %include "io.inc"
        // section .data
        // next_stack_addr dd 0
        // stack_size 2048
        output.push(Assembly::Include("io.inc".into()));
        output.push(Assembly::Section(".data".into()));
        output.push(Assembly::Data("next_stack_addr".into(), 0));
        output.push(Assembly::Data("stack_size".into(), 2 * 1024));

        // section .text
        // global CMAIN
        // CMAIN
        output.push(Assembly::Section(".text".into()));
        output.push(Assembly::Global("CMAIN".into()));
        output.push(Assembly::Label("CMAIN".into()));

        // Setup stack frame for the base/runtime layer
        // this will create any runtime administrative logic
        // and also call the users `main` function.
        output.push(Assembly::Instr(Instr::Push(Ebp)));
        output.push(Assembly::Instr(Instr::Mov(
            Location::Register(Ebp),
            Source::Register(Esp),
        )));

        output.push(Assembly::Instr(Instr::Mov(
            Location::Register(Eax),
            Source::Register(Esp),
        )));
        output.push(Assembly::Instr(Instr::Sub(
            Eax,
            Source::Memory("stack_size".into()),
        )));
        output.push(Assembly::Instr(Instr::Mov(
            Location::Memory("next_stack_addr".into()),
            Source::Register(Eax),
        )));

        // Call main function
        output.push(Assembly::Instr(Instr::Call("my_main".into())));

        // Clean up frame before exiting program
        output.push(Assembly::Instr(Instr::Mov(
            Location::Register(Esp),
            Source::Register(Ebp),
        )));
        output.push(Assembly::Instr(Instr::Pop(Ebp)));
        output.push(Assembly::Instr(Instr::Ret));
    }

    /// Writes the function which will handle initializing a new coroutine
    fn coroutine_init(ns: &str, sinc: &str, output: &mut Vec<Assembly>) {
        use Register::*;
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

        // Create new stack frame
        output.push(Assembly::Label("runtime_init_coroutine".into()));
        output.push(Assembly::Instr(Instr::Push(Ebp)));
        output.push(Assembly::Instr(Instr::Mov(
            Location::Register(Ebp),
            Source::Register(Esp),
        )));

        // Create coroutine's stack
        output.push(Assembly::Instr(Instr::Mov(
            Location::Register(Esp),
            Source::Memory(ns.into()),
        )));

        output.push(Assembly::Instr(Instr::Mov(
            Location::Memory(format!("{}-4", Esp)),
            Source::Register(Eax),
        ))); // Store the coroutine's current next instruction to execute
        output.push(Assembly::Instr(Instr::Mov(
            Location::Memory(format!("{}-8", Esp)),
            Source::Integer(0),
        ))); // store the return ESP
        output.push(Assembly::Instr(Instr::Mov(
            Location::Memory(format!("{}-12", Esp)),
            Source::Integer(0),
        ))); // store the return EBP
        output.push(Assembly::Instr(Instr::Mov(
            Location::Memory(format!("{}-16", Esp)),
            Source::Integer(0),
        ))); // store the return Instruction address

        output.push(Assembly::Instr(Instr::Lea(
            Location::Register(Eax),
            Source::Memory(format!("{}-20", Esp)),
        )));
        output.push(Assembly::Instr(Instr::Mov(
            Location::Memory(format!("{}-20", Esp)),
            Source::Register(Eax),
        )));

        // Move satck address into EAX for return
        output.push(Assembly::Instr(Instr::Mov(
            Location::Register(Eax),
            Source::Register(Esp),
        )));

        output.push(Assembly::Instr(Instr::Sub(
            Esp,
            Source::Memory(sinc.into()),
        )));
        output.push(Assembly::Instr(Instr::Mov(
            Location::Memory(ns.into()),
            Source::Register(Esp),
        )));

        // clean up stack frame
        output.push(Assembly::Instr(Instr::Mov(
            Location::Register(Esp),
            Source::Register(Ebp),
        )));
        output.push(Assembly::Instr(Instr::Pop(Ebp)));
        output.push(Assembly::Instr(Instr::Ret));
    }

    fn runtime_yield_into_coroutine(output: &mut Vec<Assembly>) {
        use Register::*;
        /*
         * Input:
         * EAX - address of the coroutine instance
         * EBX - address of the return point
         */
        output.push(Assembly::Label("runtime_yield_into_coroutine".into()));

        // mov ESP into metadata (return ESP)
        // mov EBP into metadata (return EBP)
        // mov return address into metadata (return address)
        output.push(Assembly::Instr(Instr::Mov(
            Location::Memory(format!("{}-8", Eax)),
            Source::Register(Esp),
        ))); // store the return ESP
        output.push(Assembly::Instr(Instr::Mov(
            Location::Memory(format!("{}-12", Eax)),
            Source::Register(Ebp),
        ))); // store the return EBP
        output.push(Assembly::Instr(Instr::Mov(
            Location::Memory(format!("{}-16", Eax)),
            Source::Register(Ebx),
        ))); // store the return Instruction address

        // Load the address of the coroutine into EBP (the base of the stack frame)
        output.push(Assembly::Instr(Instr::Mov(
            Location::Register(Ebp),
            Source::Register(Eax),
        )));

        // Load the coroutines current stack location into ESP
        output.push(Assembly::Instr(Instr::Mov(
            Location::Register(Esp),
            Source::Memory(format!("{}-20", Ebp)),
        )));

        // Re/enter the coroutine
        output.push(Assembly::Instr(Instr::Jmp(format!(
            "{}",
            Source::Memory(format!("{}-4", Ebp))
        ))));
    }

    fn runtime_yield_return(output: &mut Vec<Assembly>) {
        use Register::*;
        /*
         * Input:
         * EAX - value being returned (if any)
         * EBX - re-entry address
         */
        // When in a coroutine, return to the calling coroutine
        output.push(Assembly::Label("runtime_yield_return".into()));

        // Store the current ESP into metadata
        output.push(Assembly::Instr(Instr::Mov(
            Location::Memory(format!("{}-20", Ebp)),
            Source::Register(Esp),
        )));
        // Store the re-entry address into metadata
        output.push(Assembly::Instr(Instr::Mov(
            Location::Memory(format!("{}-4", Ebp)),
            Source::Register(Ebx),
        )));

        // Get the return ESP from metadata
        output.push(Assembly::Instr(Instr::Mov(
            Location::Register(Esp),
            Source::Memory(format!("{}-8", Ebp)),
        )));
        // Get the return address from metadata
        output.push(Assembly::Instr(Instr::Mov(
            Location::Register(Ebx),
            Source::Memory(format!("{}-16", Ebp)),
        )));
        // Get the return EBP from metadata
        output.push(Assembly::Instr(Instr::Mov(
            Location::Register(Ebp),
            Source::Memory(format!("{}-12", Ebp)),
        )));

        // jmp to the return address
        output.push(Assembly::Instr(Instr::Jmp(format!(
            "{}",
            Source::Register(Ebx)
        ))));
    }

    fn traverse(
        ast: &super::Node,
        current_func: &String,
        function_table: &mut super::FunctionTable,
        output: &mut Vec<Assembly>,
    ) {
        use Register::*;

        println!("Compile @ {:?}", ast);
        match ast {
            super::Node::Integer(i) => {
                output.push(Assembly::Instr(Instr::Mov(
                    Location::Register(Eax),
                    Source::Integer(*i),
                )));
            }
            super::Node::Identifier(id) => {
                let id_offset = {
                    let var = function_table.funcs[current_func]
                        .vars
                        .vars
                        .iter()
                        .find(|v| v.0 == *id)
                        .expect("CRITICAL: identifier not found in var table");
                    var.2
                };
                output.push(Assembly::Instr(Instr::Mov(
                    Location::Register(Eax),
                    Source::Memory(format!("ebp-{}", id_offset)),
                )));
            }
            super::Node::Mul(l, r) => {
                let left = l.as_ref();
                Compiler::traverse(left, current_func, function_table, output);
                output.push(Assembly::Instr(Instr::Push(Eax)));
                let right = r.as_ref();
                Compiler::traverse(right, current_func, function_table, output);
                output.push(Assembly::Instr(Instr::Push(Eax)));

                output.push(Assembly::Instr(Instr::Pop(Ebx)));
                output.push(Assembly::Instr(Instr::Pop(Eax)));
                output.push(Assembly::Instr(Instr::IMul(
                    Eax,
                    Location::Register(Ebx),
                )));
            }
            super::Node::Add(l, r) => {
                let left = l.as_ref();
                Compiler::traverse(left, current_func, function_table, output);
                output.push(Assembly::Instr(Instr::Push(Eax)));
                let right = r.as_ref();
                Compiler::traverse(right, current_func, function_table, output);
                output.push(Assembly::Instr(Instr::Push(Eax)));

                output.push(Assembly::Instr(Instr::Pop(Ebx)));
                output.push(Assembly::Instr(Instr::Pop(Eax)));
                output.push(Assembly::Instr(Instr::Add(
                    Eax,
                    Source::Register(Ebx),
                )));
            }
            super::Node::Bind(id, exp) => {
                let id_offset = {
                    let var = function_table.funcs[current_func]
                        .vars
                        .vars
                        .iter()
                        .find(|v| v.0 == *id)
                        .expect("CRITICAL: identifier not found in var table");
                    var.2
                };
                Compiler::traverse(exp, current_func, function_table, output);
                output.push(Assembly::Instr(Instr::Mov(
                    Location::Memory(format!("ebp-{}", id_offset)),
                    Source::Register(Eax),
                )));
            }
            super::Node::FunctionDef(fn_name, params, stmts) => {
                output.push(Assembly::Label(fn_name.clone()));

                // Prepare stack frame for this function
                output.push(Assembly::Instr(Instr::Push(Ebp)));
                output.push(Assembly::Instr(Instr::Mov(
                    Location::Register(Ebp),
                    Source::Register(Esp),
                )));
                let total_offset = function_table.funcs[fn_name].vars.vars.last().unwrap().2;
                output.push(Assembly::Instr(Instr::Sub(
                    Esp,
                    Source::Integer(total_offset),
                )));

                // Move function parameters from registers into the stack frame
                let param_registers = [Eax, Ebx, Ecx, Edx];
                if params.len() > param_registers.len() {
                    panic!("Compiler: too many parameters in function definition");
                }
                for (param, reg) in params.iter().zip(param_registers.iter()) {
                    let param_offset = function_table.funcs[fn_name]
                        .vars
                        .vars
                        .iter()
                        .find(|(id, _, _)| id == param)
                        .unwrap()
                        .2;
                    output.push(Assembly::Instr(Instr::Mov(
                        Location::Memory(format!("ebp-{}", param_offset)),
                        Source::Register(*reg),
                    )));
                }

                for s in stmts.iter() {
                    Compiler::traverse(s, fn_name, function_table, output);
                }

                // Clean up frame before exiting program
                output.push(Assembly::Instr(Instr::Mov(
                    Location::Register(Esp),
                    Source::Register(Ebp),
                )));
                output.push(Assembly::Instr(Instr::Pop(Ebp)));

                output.push(Assembly::Instr(Instr::Ret));
            }
            super::Node::CoroutineDef(fn_name, stmts) => {
                output.push(Assembly::Label(fn_name.clone()));

                // Prepare stack frame for this function
                for s in stmts.iter() {
                    Compiler::traverse(s, fn_name, function_table, output);
                }
                output.push(Assembly::Instr(Instr::Jmp("runtime_yield_return".into())))
            }
            super::Node::Module(functions, coroutines) => {
                for f in functions.iter() {
                    Compiler::traverse(f, current_func, function_table, output);
                }
                for co in coroutines.iter() {
                    Compiler::traverse(co, current_func, function_table, output);
                }
            }
            super::Node::Return(exp) => match exp {
                Some(e) => Compiler::traverse(e, current_func, function_table, output),
                None => (),
            },
            super::Node::CoroutineInit(id) => {
                output.push(Assembly::Instr(Instr::Lea(
                    Location::Register(Eax),
                    Source::Memory(format!("{}", id)),
                )));
                output.push(Assembly::Instr(Instr::Call(
                    "runtime_init_coroutine".into(),
                )))
            }
            super::Node::Yield(id) => {
                Compiler::traverse(id, current_func, function_table, output);

                function_table
                    .funcs
                    .entry(current_func.clone())
                    .and_modify(|fi| fi.label_count += 1);
                let ret_lbl = format!(".lbl_{}", function_table.funcs[current_func].label_count);

                output.push(Assembly::Instr(Instr::Mov(
                    Location::Register(Ebx),
                    Source::Address(ret_lbl.clone()),
                )));
                output.push(Assembly::Instr(Instr::Jmp(
                    "runtime_yield_into_coroutine".into(),
                )));
                output.push(Assembly::Label(ret_lbl.clone()));
            }
            super::Node::YieldReturn(exp) => {
                if let Some(exp) = exp {
                    Compiler::traverse(exp, current_func, function_table, output);
                }

                function_table
                    .funcs
                    .entry(current_func.clone())
                    .and_modify(|fi| fi.label_count += 1);
                let ret_lbl = format!(".lbl_{}", function_table.funcs[current_func].label_count);

                output.push(Assembly::Instr(Instr::Mov(
                    Location::Register(Ebx),
                    Source::Address(ret_lbl.clone()),
                )));
                output.push(Assembly::Instr(Instr::Jmp("runtime_yield_return".into())));
                output.push(Assembly::Label(ret_lbl.clone()));
            }
            super::Node::Println(exp) => {
                Compiler::traverse(exp, current_func, function_table, output);
                output.push(Assembly::Instr(Instr::Print(Source::Register(
                    Eax,
                ))));
                output.push(Assembly::Instr(Instr::Newline));
            }
            super::Node::FunctionCall(fn_name, params) => {
                // Check if function exists and if the right number of parameters are being
                // passed
                if !function_table.funcs.contains_key(fn_name) {
                    panic!("Compiler: no definition found for function `{}`", fn_name);
                }

                let expected_num_params = function_table.funcs[fn_name].params.len();
                let got_num_params = params.len();
                if expected_num_params != got_num_params {
                    panic!(
                        "Compiler: expected {} but got {} parameters for function `{}`",
                        expected_num_params, got_num_params, fn_name
                    );
                }

                // evaluate each paramater then store in registers Eax, Ebx, Ecx, Edx before
                // calling the function
                let param_registers = [Eax, Ebx, Ecx, Edx];
                if params.len() > param_registers.len() {
                    panic!("Compiler: too many parameters being passed to function");
                }
                for param in params.iter() {
                    Compiler::traverse(param, current_func, function_table, output);
                    output.push(Assembly::Instr(Instr::Push(Eax)));
                }
                for reg in param_registers.iter().take(params.len()).rev() {
                    output.push(Assembly::Instr(Instr::Pop(*reg)));
                }
                output.push(Assembly::Instr(Instr::Call(fn_name.clone())));
            }
            node => println!("Expected an operator, found {:?}", node),
        }
    }
}
