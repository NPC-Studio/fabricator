use std::{fs::File, io::Read, path::PathBuf, string::String as StdString};

use clap::{Parser, Subcommand};
use fabricator_compiler::{
    codegen::codegen,
    compile::{compile, optimize_ir},
    frontend::FrontendSettings,
    magic_dict::{MagicDict, MagicMode},
    parser::ParseSettings,
    string_interner::StringInterner,
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
            File::open(path).unwrap().read_to_string(&mut code).unwrap();

            let prototype = compile(ctx, ctx.testing_stdlib(), &code).unwrap();
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
            let mut code = StdString::new();
            File::open(path).unwrap().read_to_string(&mut code).unwrap();

            struct Interner<'gc>(vm::Context<'gc>);

            impl<'gc> StringInterner for Interner<'gc> {
                type String = vm::String<'gc>;

                fn intern(&mut self, s: &str) -> vm::String<'gc> {
                    self.0.intern(s)
                }
            }

            let parsed = ParseSettings::default()
                .parse(&code, Interner(ctx))
                .unwrap();

            struct MDict<'gc>(Gc<'gc, vm::MagicSet<'gc>>);

            impl<'gc> MagicDict<vm::String<'gc>> for MDict<'gc> {
                fn magic_mode(&self, ident: &vm::String<'gc>) -> Option<MagicMode> {
                    let index = self.0.find(ident.as_str())?;
                    let magic = self.0.get(index).unwrap();
                    Some(if magic.read_only() {
                        MagicMode::ReadOnly
                    } else {
                        MagicMode::ReadWrite
                    })
                }

                fn magic_index(&self, ident: &vm::String<'gc>) -> Option<usize> {
                    self.0.find(ident.as_str())
                }
            }

            let mut ir = FrontendSettings::default()
                .compile_ir(&parsed, MDict(ctx.stdlib()))
                .unwrap();

            println!("Compiled IR: {ir:#?}");
            optimize_ir(&mut ir).expect("Internal Compiler Error");
            println!("Optimized IR: {ir:#?}");
            let prototype = codegen(&ctx, &ir, MDict(ctx.stdlib())).unwrap();
            println!("Bytecode: {prototype:#?}");
        }
    });
}
