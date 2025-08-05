use std::{fs::File, io::Read, path::PathBuf};

use clap::{Parser, Subcommand};
use fabricator_compiler::{
    CompileSettings,
    compiler::{Compiler, ImportItems},
};
use fabricator_stdlib::StdlibContext as _;
use fabricator_vm as vm;

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: Command,
    #[arg(short, long, default_value_t = 2)]
    opt_level: u8,
}

#[derive(Subcommand)]
enum Command {
    Run { path: PathBuf },
    Dump { path: PathBuf },
}

fn main() {
    let cli = Cli::parse();
    let mut interpreter = vm::Interpreter::new();
    interpreter.enter(|ctx| match cli.command {
        Command::Run { path } => {
            let mut code = String::new();
            File::open(&path)
                .unwrap()
                .read_to_string(&mut code)
                .unwrap();

            let settings = CompileSettings::from_path(&path).set_optimization_passes(cli.opt_level);

            let (proto, _, _) = Compiler::compile_chunk(
                ctx,
                "default",
                ImportItems::from_magic(ctx.testing_stdlib()),
                settings,
                path.to_string_lossy().into_owned(),
                &code,
            )
            .unwrap();
            let closure = vm::Closure::new(&ctx, proto, vm::Value::Undefined).unwrap();

            let thread = vm::Thread::new(&ctx);
            println!("returns: {:?}", thread.exec(ctx, closure).unwrap());
        }
        Command::Dump { path } => {
            let mut code = String::new();
            File::open(&path)
                .unwrap()
                .read_to_string(&mut code)
                .unwrap();

            let settings = CompileSettings::from_path(&path).set_optimization_passes(cli.opt_level);

            let (_, _, debug) = Compiler::compile_chunk(
                ctx,
                "default",
                ImportItems::from_magic(ctx.testing_stdlib()),
                settings,
                path.to_string_lossy().into_owned(),
                &code,
            )
            .unwrap();

            for (proto, ir) in debug {
                let chunk = proto.chunk();
                match proto.reference() {
                    vm::FunctionRef::Named(ref_name, span) => {
                        println!(
                            "==[Function named {ref_name} at line {}]==",
                            chunk.line_number(span.start())
                        );
                    }
                    vm::FunctionRef::Expression(span) => {
                        println!(
                            "==[Function expression at line {}]==",
                            chunk.line_number(span.start())
                        );
                    }
                    vm::FunctionRef::Chunk => {
                        println!("==[Chunk function]==");
                    }
                }
                println!();
                println!("IR: {:#?}", ir);
                println!("Bytecode: {:#?}", proto);
            }
        }
    });
}
