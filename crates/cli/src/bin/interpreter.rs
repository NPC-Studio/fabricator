use std::{fs::File, io::Read, path::PathBuf, string::String as StdString};

use clap::{Parser, Subcommand};
use fabricator_compiler::{
    codegen::codegen,
    compile::{SourceChunk, VmMagic, compile, optimize_ir},
    frontend::FrontendSettings,
    parser::ParseSettings,
    string_interner::VmInterner,
};
use fabricator_stdlib::StdlibContext as _;
use fabricator_vm as vm;
use gc_arena::Gc;

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
            let mut code = StdString::new();
            File::open(&path)
                .unwrap()
                .read_to_string(&mut code)
                .unwrap();

            let prototype = compile(
                ctx,
                ctx.testing_stdlib(),
                path.to_string_lossy().as_ref(),
                &code,
            )
            .unwrap();
            let closure = vm::Closure::new(
                &ctx,
                Gc::new(&ctx, prototype),
                ctx.testing_stdlib(),
                vm::Value::Undefined,
            )
            .unwrap();

            let thread = vm::Thread::new(&ctx);
            println!("returns: {:?}", thread.exec(ctx, closure).unwrap());
        }
        Command::Dump { path } => {
            let stdlib = VmMagic::new(ctx.stdlib());

            let mut code = StdString::new();
            File::open(&path)
                .unwrap()
                .read_to_string(&mut code)
                .unwrap();

            let chunk = vm::Chunk::new_static(
                &ctx,
                SourceChunk::new(path.to_string_lossy().as_ref(), &code),
            );

            let parsed = ParseSettings::default()
                .parse(&code, VmInterner(ctx))
                .unwrap();

            let mut ir = FrontendSettings::default()
                .compile_ir(&parsed, stdlib)
                .unwrap();

            println!("Compiled IR: {ir:#?}");
            optimize_ir(&mut ir).expect("Internal Compiler Error");
            println!("Optimized IR: {ir:#?}");
            let prototype = codegen(&ctx, &ir, chunk, stdlib).unwrap();
            println!("Bytecode: {prototype:#?}");
        }
    });
}
