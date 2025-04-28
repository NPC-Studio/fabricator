use std::{fs::File, io::Read, path::PathBuf, string::String as StdString};

use clap::{Parser, Subcommand};
use fabricator::{
    closure::Closure,
    compiler::{
        codegen::codegen,
        compile::{compile, optimize_ir},
        frontend::FrontendSettings,
        magic_dict::{MagicDict, MagicMode},
        parser::ParseSettings,
        string_interner::StringInterner,
    },
    interpreter::Interpreter,
    magic::MagicSet,
    string::String,
    thread::Thread,
    value::Value,
};
use gc_arena::{Gc, Mutation};

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
    let mut interpreter = Interpreter::testing();
    interpreter.enter(|ctx| match cli.command {
        Command::Run { path } => {
            let mut code = StdString::new();
            File::open(path).unwrap().read_to_string(&mut code).unwrap();

            let prototype = compile(&ctx, ctx.stdlib(), &code).unwrap();
            let closure = Closure::new(
                &ctx,
                Gc::new(&ctx, prototype),
                ctx.stdlib(),
                Value::Undefined,
            )
            .unwrap();

            let thread = Thread::new(&ctx);
            println!("returns: {:?}", thread.exec(ctx, closure).unwrap());
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

            let parsed = ParseSettings::default()
                .parse(&code, Interner(&ctx))
                .unwrap();

            struct MDict<'gc>(Gc<'gc, MagicSet<'gc>>);

            impl<'gc> MagicDict<String<'gc>> for MDict<'gc> {
                fn magic_mode(&self, ident: &String<'gc>) -> Option<MagicMode> {
                    let index = self.0.find(ident.as_str())?;
                    let magic = self.0.get(index).unwrap();
                    Some(if magic.read_only() {
                        MagicMode::ReadOnly
                    } else {
                        MagicMode::ReadWrite
                    })
                }

                fn magic_index(&self, ident: &String<'gc>) -> Option<usize> {
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
