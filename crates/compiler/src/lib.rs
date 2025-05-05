pub mod analysis;
pub mod codegen;
pub mod compile;
pub mod constant;
pub mod frontend;
pub mod graph;
pub mod ir;
pub mod lexer;
pub mod magic_dict;
pub mod parser;
pub mod string_interner;

pub use compile::{CompilerError, compile, compile_compat};
