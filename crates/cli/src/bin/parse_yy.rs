use std::{fs::File, io::Read as _, path::PathBuf};

use clap::Parser;
use fabricator_yy_format as yy;

#[derive(Parser)]
struct Cli {
    file: PathBuf,
}

fn main() {
    let cli = Cli::parse();
    let mut file = File::open(&cli.file).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();

    let val = yy::Value::parse(&contents).unwrap();
    println!("{}", val.to_string_pretty());
}
