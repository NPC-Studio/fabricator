use std::{
    fs::{File, read_dir},
    io::{self, Write, stdout},
};

use fabricator_compiler as compiler;
use fabricator_stdlib::StdlibContext as _;
use fabricator_vm as vm;
use gc_arena::Gc;

fn run_code(name: &str, code: &str, compat: bool) -> Result<(), vm::Error> {
    let mut interpreter = vm::Interpreter::new();

    interpreter.enter(|ctx| {
        let prototype = compiler::compile(
            ctx,
            if compat {
                compiler::CompileSettings::compat()
            } else {
                compiler::CompileSettings::full()
            },
            ctx.testing_stdlib(),
            name,
            &code,
        )?;
        let closure =
            vm::Closure::new(&ctx, Gc::new(&ctx, prototype), vm::Value::Undefined).unwrap();

        let thread = vm::Thread::new(&ctx);
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
            if ext.eq_ignore_ascii_case("fml") || ext.eq_ignore_ascii_case("gml") {
                let _ = writeln!(stdout(), "running {:?}", path);
                if let Err(err) = run_code(
                    path.to_string_lossy().as_ref(),
                    &code,
                    ext.eq_ignore_ascii_case("gml"),
                ) {
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
