pub mod analysis;
pub mod compile;
pub mod constant;
pub mod graph;
pub mod ir;
pub mod ir_gen;
pub mod lexer;
pub mod line_numbers;
pub mod macros;
pub mod magic_dict;
pub mod parser;
pub mod proto_gen;
pub mod string_interner;

pub use compile::{CompileSettings, CompilerError, compile};
