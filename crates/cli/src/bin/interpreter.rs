use std::{fs::File, io::Read, path::PathBuf, string::String as StdString};

use clap::{Parser, Subcommand};
use fabricator_compiler::{
    CompileSettings, CompilerError,
    compiler::{CompilerErrorKind, SourceChunk, compile, optimize_ir},
    ir_gen::MagicMode,
    lexer::Lexer,
    proto_gen::gen_prototype,
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
                CompileSettings::full(),
                ctx.testing_stdlib(),
                path.to_string_lossy().as_ref(),
                &code,
            )
            .unwrap();
            let closure =
                vm::Closure::new(&ctx, Gc::new(&ctx, prototype), vm::Value::Undefined).unwrap();

            let thread = vm::Thread::new(&ctx);
            println!("returns: {:?}", thread.exec(ctx, closure).unwrap());
        }
        Command::Dump { path } => {
            let mut code = StdString::new();
            File::open(&path)
                .unwrap()
                .read_to_string(&mut code)
                .unwrap();

            let chunk = vm::Chunk::new_static(
                &ctx,
                SourceChunk::new(path.to_string_lossy().as_ref(), &code),
            );

            let settings = if path
                .extension()
                .is_some_and(|e| e.eq_ignore_ascii_case("gml"))
            {
                CompileSettings::compat()
            } else {
                CompileSettings::full()
            };

            let mut tokens = Vec::new();
            Lexer::tokenize(VmInterner::new(ctx), &code, &mut tokens)
                .map_err(|e| {
                    let line_number = chunk.line_number(e.span.start());
                    CompilerError {
                        kind: CompilerErrorKind::Lexing(e),
                        line_number,
                    }
                })
                .unwrap();

            let parsed = settings
                .parse
                .parse(tokens)
                .map_err(|e| {
                    let line_number = chunk.line_number(e.span.start());
                    CompilerError {
                        kind: CompilerErrorKind::Parsing(e),
                        line_number,
                    }
                })
                .unwrap();

            let mut ir = settings
                .ir_gen
                .gen_ir(&parsed, |m| {
                    let i = ctx.stdlib().find(m)?;
                    Some(if ctx.stdlib().get(i).unwrap().read_only() {
                        MagicMode::ReadOnly
                    } else {
                        MagicMode::ReadWrite
                    })
                })
                .map_err(|e| {
                    let line_number = chunk.line_number(e.span.start());
                    CompilerError {
                        kind: CompilerErrorKind::IrGen(e),
                        line_number,
                    }
                })
                .unwrap();

            println!("Compiled IR: {ir:#?}");
            optimize_ir(&mut ir).expect("Internal Compiler Error");
            println!("Optimized IR: {ir:#?}");
            let prototype = gen_prototype(&ctx, &ir, chunk, ctx.stdlib()).unwrap();
            println!("Bytecode: {prototype:#?}");
        }
    });
}
