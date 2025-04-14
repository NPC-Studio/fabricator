use std::{
    fs::{read_dir, File},
    io::{self, stdout, Write},
};

use criterion::{criterion_group, criterion_main, Criterion};
use fabricator::{closure::Closure, compiler::compile, context::Interpreter, thread::Thread};
use gc_arena::Gc;

fn benchmark_script(c: &mut Criterion, name: &str, code: &str) {
    let mut interpreter = Interpreter::testing();

    let (thread, closure) = interpreter.enter(|ctx| {
        let prototype = compile(&ctx, &code).expect("compile error");
        let closure = Closure::new(&ctx, Gc::new(&ctx, prototype), ctx.globals()).unwrap();

        let thread = Thread::new(&ctx);
        (ctx.stash(thread), ctx.stash(closure))
    });

    c.bench_function(name, move |b| {
        b.iter(|| {
            interpreter.enter(|ctx| {
                let thread = ctx.fetch(&thread);
                let closure = ctx.fetch(&closure);
                thread.exec(&ctx, closure).expect("execution error");
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
