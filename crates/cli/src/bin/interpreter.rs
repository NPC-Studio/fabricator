use std::{fs::File, io::Read, path::PathBuf};

use clap::{Parser, Subcommand};
use fabricator_compiler::{
    CompileError, CompileSettings,
    code_gen::gen_prototype,
    compiler::{CompileErrorKind, Compiler, ImportItems, SourceChunk, optimize_ir, verify_ir},
    ir_gen::MagicMode,
    lexer::Lexer,
    line_numbers::LineNumbers,
    string_interner::VmInterner,
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

            let (prototype, _) = Compiler::compile_chunk(
                ctx,
                "default",
                ImportItems::from_magic(ctx.testing_stdlib()),
                CompileSettings::full(),
                path.to_string_lossy().into_owned(),
                &code,
            )
            .unwrap();
            let closure = vm::Closure::new(&ctx, prototype, vm::Value::Undefined).unwrap();

            let thread = vm::Thread::new(&ctx);
            println!("returns: {:?}", thread.exec(ctx, closure).unwrap());
        }
        Command::Dump { path } => {
            let mut code = String::new();
            File::open(&path)
                .unwrap()
                .read_to_string(&mut code)
                .unwrap();

            let chunk_name = vm::RefName::new(path.to_string_lossy().into_owned());

            let chunk = vm::Chunk::new_static(
                &ctx,
                SourceChunk {
                    name: chunk_name.clone(),
                    line_numbers: LineNumbers::new(&code),
                },
            );

            let settings = CompileSettings::from_path(&path);

            let mut tokens = Vec::new();
            Lexer::tokenize(VmInterner::new(ctx), &code, &mut tokens)
                .map_err(|e| {
                    let line_number = chunk.line_number(e.span.start());
                    CompileError {
                        kind: CompileErrorKind::Lexing(e),
                        chunk_name: chunk_name.clone(),
                        line_number,
                    }
                })
                .unwrap();

            let parsed = settings
                .parse
                .parse(tokens)
                .map_err(|e| {
                    let line_number = chunk.line_number(e.span.start());
                    CompileError {
                        kind: CompileErrorKind::Parsing(e),
                        chunk_name: chunk_name.clone(),
                        line_number,
                    }
                })
                .unwrap();

            let mut ir = settings
                .ir_gen
                .gen_ir(&parsed, vm::FunctionRef::Chunk, &[], |m| {
                    let i = ctx.stdlib().find(m)?;
                    Some(if ctx.stdlib().get(i).unwrap().read_only() {
                        MagicMode::ReadOnly
                    } else {
                        MagicMode::ReadWrite
                    })
                })
                .map_err(|e| {
                    let line_number = chunk.line_number(e.span.start());
                    CompileError {
                        kind: CompileErrorKind::IrGen(e),
                        chunk_name: chunk_name.clone(),
                        line_number,
                    }
                })
                .unwrap();

            verify_ir(&ir).expect("Internal IR generation error");
            println!("Compiled IR: {ir:#?}");
            optimize_ir(&mut ir);
            verify_ir(&ir).expect("Internal IR optimization error");
            println!("Optimized IR: {ir:#?}");
            let prototype =
                gen_prototype(&ir, |m| ctx.stdlib().find(m)).expect("Internal Codegen Error");
            println!("Bytecode: {prototype:#?}");
        }
    });
}
