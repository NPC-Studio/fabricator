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

            let settings = CompileSettings::from_path(&path);

            let (output, _) = Compiler::compile_chunk(
                ctx,
                "default",
                ImportItems::from_magic(ctx.testing_stdlib()),
                settings,
                path.to_string_lossy().into_owned(),
                &code,
            )
            .unwrap();
            let closure = vm::Closure::new(&ctx, output.prototype, vm::Value::Undefined).unwrap();

            let thread = vm::Thread::new(&ctx);
            println!("returns: {:?}", thread.exec(ctx, closure).unwrap());
        }
        Command::Dump { path } => {
            let mut code = String::new();
            File::open(&path)
                .unwrap()
                .read_to_string(&mut code)
                .unwrap();

            let settings = CompileSettings::from_path(&path);

            let (output, _) = Compiler::compile_chunk(
                ctx,
                "default",
                ImportItems::from_magic(ctx.testing_stdlib()),
                settings,
                path.to_string_lossy().into_owned(),
                &code,
            )
            .unwrap();

            println!("Compiled IR: {:#?}", output.unoptimized_ir);
            println!("Optimized IR: {:#?}", output.optimized_ir);
            println!("Bytecode: {:#?}", output.prototype);
        }
    });
}
