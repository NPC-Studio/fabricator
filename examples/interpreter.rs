use std::{fs::File, io::Read, path::PathBuf, string::String as StdString};

use clap::{Parser, Subcommand};
use fabricator::{
    callback::Callback,
    closure::Closure,
    compiler::{
        codegen::codegen,
        compile::{compile, optimize_ir},
        frontend::compile_ir,
        parser::parse,
        string_interner::StringInterner,
    },
    object::Object,
    string::String,
    thread::Thread,
    value::Value,
};
use gc_arena::{arena, Gc, Mutation};

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    Run { path: PathBuf },
    Dump { path: PathBuf },
}

fn main() {
    let cli = Cli::parse();
    arena::rootless_mutate(|mc| match cli.command {
        Command::Run { path } => {
            let mut code = StdString::new();
            File::open(path).unwrap().read_to_string(&mut code).unwrap();

            let globals = Object::new(mc);
            let assert = Callback::from_fn(mc, |_, stack| {
                if !stack.get(0).to_bool() {
                    Err("assert failed".into())
                } else {
                    Ok(())
                }
            });
            let print = Callback::from_fn(mc, |_, stack| {
                for i in 0..stack.len() {
                    print!("{:?}", stack.get(i));
                    if i != stack.len() - 1 {
                        print!("\t");
                    }
                }
                println!();
                Ok(())
            });

            globals.set(mc, String::new(mc, "assert"), Value::Callback(assert));
            globals.set(mc, String::new(mc, "print"), Value::Callback(print));

            let prototype = compile(mc, &code).unwrap();
            let closure = Closure::new(mc, Gc::new(mc, prototype), globals);

            let mut thread = Thread::default();
            println!("returns: {:?}", thread.exec(mc, closure).unwrap());
        }
        Command::Dump { path } => {
            let mut code = StdString::new();
            File::open(path).unwrap().read_to_string(&mut code).unwrap();

            struct Interner<'a, 'gc>(&'a Mutation<'gc>);

            impl<'a, 'gc> StringInterner for Interner<'a, 'gc> {
                type String = String<'gc>;

                fn intern(&mut self, s: &str) -> String<'gc> {
                    String::new(self.0, s)
                }
            }

            let parsed = parse(&code, Interner(mc)).unwrap();
            let mut ir = compile_ir(&parsed).unwrap();
            println!("Compiled IR: {ir:?}");
            optimize_ir(&mut ir).expect("Internal Compiler Error");
            println!("Optimized IR: {ir:?}");
            let prototype = codegen(ir).unwrap();
            println!("Bytecode: {prototype:?}");
        }
    });
}
