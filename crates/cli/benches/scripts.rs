use std::{
    fs::{File, read_dir},
    io::{self, Write, stdout},
};

use criterion::{Criterion, criterion_group, criterion_main};
use fabricator_compiler as compiler;
use fabricator_stdlib::StdlibContext;
use fabricator_vm as vm;

fn benchmark_script(c: &mut Criterion, name: &str, code: &str) {
    let mut interpreter = vm::Interpreter::new();

    let (thread, closure) = interpreter.enter(|ctx| {
        let mut compiler = compiler::Compiler::new(
            ctx,
            "default",
            compiler::ImportItems::with_magic(ctx.stdlib()),
        );
        compiler
            .add_chunk(compiler::CompileSettings::modern(), name, code)
            .expect("compile error");
        let output = compiler
            .compile()
            .expect("compile error")
            .optimize_and_generate(&ctx);
        let closure = vm::Closure::new(&ctx, output.chunks[0], vm::Value::Undefined).unwrap();

        let thread = vm::Thread::new(&ctx);
        (ctx.stash(thread), ctx.stash(closure))
    });

    c.bench_function(name, move |b| {
        b.iter(|| {
            interpreter.enter(|ctx| {
                let thread = ctx.fetch(&thread);
                let closure = ctx.fetch(&closure);
                thread.with_exec(ctx, |mut exec| {
                    exec.call_closure(ctx, closure).expect("execution error");
                    assert!(
                        exec.stack().get(0) == vm::Value::Boolean(true),
                        "script did not return `true`"
                    );
                });
            });
        });
    });
}

pub fn benchmark_scripts(c: &mut Criterion) {
    for dir in read_dir("./benches/scripts").expect("could not list dir contents") {
        let path = dir.expect("could not read dir entry").path();
        let code = io::read_to_string(File::open(&path).unwrap()).unwrap();
        if let Some(ext) = path.extension() {
            if ext == "fml" {
                let _ = writeln!(stdout(), "running {:?}", path);
                benchmark_script(c, path.to_string_lossy().as_ref(), &code);
            }
        } else {
            let _ = writeln!(stdout(), "skipping file {:?}", path);
        }
    }
}

criterion_group!(benches, benchmark_scripts);
criterion_main!(benches);
