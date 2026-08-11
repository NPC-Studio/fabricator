use std::{
    fs::{File, read_dir},
    io::{self, Write, stdout},
};

use fabricator_cli::run_code;
use fabricator_compiler as compiler;

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
