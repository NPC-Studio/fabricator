use std::{
    fs::{File, read_dir},
    io::{self, Write, stdout},
};

use anyhow::Error;
use fabricator_cli::TestingStdlibContext as _;
use fabricator_compiler as compiler;
use fabricator_vm as vm;
use gc_arena::Collect;
use thiserror::Error;

fn run_code(
    name: &str,
    code: &str,
    compile_settings: compiler::CompileSettings,
) -> Result<bool, Error> {
    const FRAME_LIMIT: u32 = 64;
    const INST_LIMIT: u32 = 32678;

    #[derive(Debug, Error)]
    #[error("vm limit reached")]
    struct VmLimitError;

    #[derive(Default, Collect)]
    #[collect(require_static)]
    struct VmLimiter {
        frame_count: u32,
    }

    impl<'gc> vm::Hook<'gc> for VmLimiter {
        fn on_call(
            &mut self,
            _ctx: vm::Context<'gc>,
            _backtrace: vm::Backtrace<'gc, '_>,
        ) -> Result<(), vm::RuntimeError> {
            self.frame_count += 1;
            if self.frame_count < FRAME_LIMIT {
                Ok(())
            } else {
                Err(VmLimitError.into())
            }
        }

        fn on_return(&mut self, _ctx: vm::Context<'gc>, _backtrace: vm::Backtrace<'gc, '_>) {
            self.frame_count -= 1;
        }

        fn on_step_count(&self, _ctx: vm::Context<'gc>) -> u32 {
            INST_LIMIT
        }

        fn on_step(
            &mut self,
            _ctx: vm::Context<'gc>,
            _backtrace: vm::Backtrace<'gc, '_>,
        ) -> Result<(), vm::RuntimeError> {
            Err(VmLimitError.into())
        }
    }

    let interpreter = vm::Interpreter::new();

    interpreter.enter(|ctx| {
        let output = compiler::Compiler::compile_chunk(
            ctx,
            "default",
            compiler::ImportItems::with_magic(&ctx, ctx.testing_stdlib()),
            compile_settings,
            name,
            code,
        )?;
        let closure = vm::Closure::new(&ctx, output.chunk_prototype, vm::Value::Undefined).unwrap();

        let thread = vm::Thread::new(&ctx);
        thread.set_hook(ctx, VmLimiter { frame_count: 0 });
        thread.exec(ctx, |mut exec| {
            exec.call(ctx, closure)?;
            Ok(exec.stack().get(0) == vm::Value::Boolean(true))
        })
    })
}

fn try_scripts(dir: &str) {
    let _ = writeln!(stdout(), "trying all scripts in {dir:?}");

    for dir in read_dir(dir).expect("could not list dir contents") {
        let path = dir.expect("could not read dir entry").path();
        let code = io::read_to_string(File::open(&path).unwrap()).unwrap();
        if let Some(ext) = path.extension() {
            if ext.eq_ignore_ascii_case("fml") || ext.eq_ignore_ascii_case("gml") {
                let _ = writeln!(stdout(), "trying {:?}", path);
                let _ = run_code(
                    path.to_string_lossy().as_ref(),
                    &code,
                    if ext.eq_ignore_ascii_case("gml") {
                        compiler::CompileSettings::compat()
                    } else {
                        compiler::CompileSettings::modern()
                    },
                );
            }
        } else {
            let _ = writeln!(stdout(), "skipping file {:?}", path);
        }
    }
}

#[test]
fn test_scripts_nopanic() {
    try_scripts("./tests/scripts_nopanic");
}
