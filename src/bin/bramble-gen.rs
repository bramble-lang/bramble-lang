use std::process::exit;

use rand::prelude::*;

/**
A tool for generating random, syntactically correct Bramble source code. For the
purposes of helping to test all possible correct combinations of code.
 */

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 3 {
        eprintln!("Takes two arguments: maximum breadth and maximum depth");
        exit(1)
    }

    let max_breadth: u32 = args[1].parse::<u32>().unwrap();
    let max_depth: u32 = args[1].parse::<u32>().unwrap();

    let mut gen = SyntaxGenerator::new();
    gen.module(max_breadth, max_depth, 0);
}

macro_rules! tprint {
    ($depth: expr , $($arg:tt)*) => {
        for _ in 0..$depth {
            print!("  ");
        }
        (print!($($arg)*))
    };
}

struct SyntaxGenerator {
    next_id: u32,
}

/*
Module => mod Identifier { Item* } | Item*
Item => Function | Struct | Extern
Struct => struct Identifier {ParameterList}
Function => fn Identifier (ParameterList) [-> Type] { Block }
Block => Statement*
ParameterList => IdDeclarationList*
IdDeclarationList => IdentifierDeclare*
FunctionCall => Identifier (Expression*)
Path => [::]Identifier[::Identifier]*
Type
ArrayType
IdDeclaration => Identifier: Type
Expression
Statement => LetBind | Mutate | Expression | Return
*/
impl SyntaxGenerator {
    fn new() -> SyntaxGenerator {
        SyntaxGenerator { next_id: 0 }
    }

    fn module(&mut self, max_breadth: u32, max_depth: u32, depth: u32) {
        if depth >= max_depth {
            return;
        }

        tprint!(depth, "mod test {{\n");
        for _ in 0..breadth(max_breadth) {
            let max_depth = new_max_depth(depth, max_depth);
            match choice(3) {
                0 => self.func(max_breadth, max_depth, depth + 1),
                1 => self.structure(max_breadth, max_depth, depth + 1),
                2 => self.module(max_breadth, max_depth, depth + 1),
                _ => panic!("Invalid choice"),
            }
        }
        tprint!(depth, "}}\n");
    }

    fn func(&mut self, max_breadth: u32, max_depth: u32, depth: u32) {
        if depth >= max_depth {
            return;
        }

        let id = self.get_id();
        tprint!(depth, "fn func{}(", id);
        self.parameter_list(max_breadth);
        print!(")");

        if choice(2) == 1 {
            print!(" -> ");
            self.ty();
        }

        print!(" {{\n");
        for _ in 0..breadth(max_breadth) {
            let max_depth = new_max_depth(depth, max_depth);
            self.statement(max_depth, depth + 1);
        }
        tprint!(depth, "return;");
        tprint!(depth, "}}\n");
    }

    fn structure(&mut self, max_breadth: u32, max_depth: u32, depth: u32) {
        if depth >= max_depth {
            return;
        }

        let id = self.get_id();
        tprint!(depth, "struct MyStruct{}{{", id);
        self.parameter_list(max_breadth);
        tprint!(depth, "}}\n");
    }

    fn parameter_list(&mut self, max_breadth: u32) {
        for _ in 0..breadth(max_breadth) {
            print!("x: ");
            self.ty();
            print!(", ");
        }
    }

    fn statement(&mut self, max_depth: u32, depth: u32) {
        if depth >= max_depth {
            return;
        }

        tprint!(depth, "let x: ");
        self.ty();
        print!(" := ");
        self.expression(max_depth, depth);
        print!(";\n");
    }

    fn expression(&mut self, max_depth: u32, depth: u32) {
        self.or(max_depth, depth);
    }

    fn or(&mut self, max_depth: u32, depth: u32) {
        self.and(max_depth, depth + 1);
        if depth < max_depth && choice(2) == 0 {
            print!("||");
            self.expression(max_depth, depth + 1);
        }
    }

    fn and(&mut self, max_depth: u32, depth: u32) {
        self.comparison(max_depth, depth + 1);
        if depth < max_depth && choice(2) == 0 {
            print!("&&");
            self.expression(max_depth, depth + 1);
        }
    }

    fn comparison(&mut self, max_depth: u32, depth: u32) {
        self.sum(max_depth, depth + 1);
        if depth < max_depth && choice(2) == 0 {
            pick_op(&["<", "<=", "==", "!=", ">", ">="]);
            self.expression(max_depth, depth + 1);
        }
    }

    fn sum(&mut self, max_depth: u32, depth: u32) {
        self.term(max_depth, depth + 1);
        if depth < max_depth && choice(2) == 0 {
            pick_op(&["+", "-"]);
            self.expression(max_depth, depth + 1);
        }
    }

    fn term(&mut self, max_depth: u32, depth: u32) {
        self.negate(max_depth, depth + 1);
        if depth < max_depth && choice(2) == 0 {
            pick_op(&["*", "/"]);
            self.expression(max_depth, depth + 1);
        }
    }

    fn negate(&mut self, max_depth: u32, depth: u32) {
        pick_op(&["-", "!", ""]);
        self.factor(max_depth, depth + 1);
    }

    fn factor(&mut self, max_depth: u32, depth: u32) {
        match choice(3) {
            0 => pick_val(&["0", "10", "true", "false"]),
            1 => {
                print!("(");
                self.expression(max_depth, depth);
                print!(")");
            }
            2 => self.if_expr(max_depth, depth),
            _ => panic!("Invalid choice"),
        }
    }

    fn if_expr(&mut self, max_depth: u32, depth: u32) {
        print!("if (");
        self.expression(max_depth, depth);
        print!(") {{");
        self.statement(max_depth, depth + 1);
        print!("}}");

        if choice(2) == 0 {
            print!(" else {{");
            self.statement(max_depth, depth);
            print!("}}");
        }
    }

    fn ty(&mut self) {
        if choice(2) == 0 {
            let ty = match choice(10) {
                0 => "u8",
                1 => "u16",
                2 => "u32",
                3 => "u64",
                4 => "i8",
                5 => "i16",
                6 => "i32",
                7 => "i64",
                8 => "bool",
                9 => "string",
                _ => panic!("Invalid choice"),
            };
            print!("{}", ty);
        } else {
            self.path()
        }
    }

    fn path(&mut self) {
        if choice(2) == 0 {
            print!("::");
        }

        print!("test");

        for _ in 0..choice(6) {
            print!("::test");
        }
    }

    fn get_id(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}

fn breadth(max: u32) -> u32 {
    let mut rng = rand::thread_rng();
    rng.gen_range(max / 2..max)
}

fn new_max_depth(current: u32, max: u32) -> u32 {
    if current < max / 2 {
        max
    } else {
        let mut rng = rand::thread_rng();
        rng.gen_range(current..max)
    }
}

fn choice(max: u32) -> u32 {
    let mut rng = rand::thread_rng();
    rng.gen_range(0..max)
}

fn pick_op(ops: &[&str]) {
    let len = ops.len();
    print!(" {}", ops[choice(len as u32) as usize]);
}

fn pick_val(vals: &[&str]) {
    let len = vals.len();
    print!("{}", vals[choice(len as u32) as usize]);
}
