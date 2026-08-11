use std::{
    fs::{File, read_dir},
    io::{self, Write, stdout},
};

use fabricator_cli::run_code;
use fabricator_compiler as compiler;

fn run_tests(dir: &str) -> bool {
    let _ = writeln!(stdout(), "running all test scripts in {dir:?}");

    let mut all_passed = true;
    for dir in read_dir(dir).expect("could not list dir contents") {
        let path = dir.expect("could not read dir entry").path();
        if let Some(ext) = path.extension() {
            if ext.eq_ignore_ascii_case("fml") || ext.eq_ignore_ascii_case("gml") {
                let code = io::read_to_string(File::open(&path).unwrap()).unwrap();
                let _ = writeln!(stdout(), "running {:?}", path);
                match run_code(
                    path.to_string_lossy().as_ref(),
                    &code,
                    if ext.eq_ignore_ascii_case("gml") {
                        compiler::CompileSettings::compat()
                    } else {
                        compiler::CompileSettings::modern()
                    },
                ) {
                    Ok(ret_true) => {
                        if !ret_true {
                            let _ = writeln!(stdout(), "script {:?} did not return `true`", path);
                            all_passed = false;
                        }
                    }
                    Err(err) => {
                        let _ = writeln!(stdout(), "error encountered running {:?}: {}", path, err);
                        all_passed = false;
                    }
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
    if !run_tests("./tests/scripts_success") {
        panic!("one or more errors occurred");
    }
}
