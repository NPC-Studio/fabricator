use std::{
    fs::{File, read_dir},
    io::{self, Write, stdout},
};

use fabricator_compiler::{compile, compile_compat};
use fabricator_interpreter::{
    closure::Closure, error::Error, interpreter::Interpreter, thread::Thread, value::Value,
};
use gc_arena::Gc;

fn run_code(code: &str, compat: bool) -> Result<(), Error> {
    let mut interpreter = Interpreter::testing();

    interpreter.enter(|ctx| {
        let prototype = if compat {
            compile_compat(&ctx, ctx.stdlib(), &code)?
        } else {
            compile(&ctx, ctx.stdlib(), &code)?
        };
        let closure = Closure::new(
            &ctx,
            Gc::new(&ctx, prototype),
            ctx.stdlib(),
            Value::Undefined,
        )
        .unwrap();

        let thread = Thread::new(&ctx);
        thread.exec(ctx, closure)?;
        Ok(())
    })
}

fn run_tests(dir: &str) -> bool {
    let _ = writeln!(stdout(), "running all test scripts in {dir:?}");

    let mut all_passed = true;
    for dir in read_dir(dir).expect("could not list dir contents") {
        let path = dir.expect("could not read dir entry").path();
        let code = io::read_to_string(File::open(&path).unwrap()).unwrap();
        if let Some(ext) = path.extension() {
            if ext == "fml" || ext == "gml" {
                let _ = writeln!(stdout(), "running {:?}", path);
                if let Err(err) = run_code(&code, ext == "gml") {
                    let _ = writeln!(stdout(), "error encountered running {:?}: {:?}", path, err);
                    all_passed = false;
                }
            }
        } else {
            let _ = writeln!(stdout(), "skipping file {:?}", path);
        }
    }
    all_passed
}

#[test]
fn test_scripts() {
    if !run_tests("./tests/scripts") {
        panic!("one or more errors occurred");
    }
}
