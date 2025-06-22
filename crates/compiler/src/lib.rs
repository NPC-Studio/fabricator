pub mod analysis;
pub mod compiler;
pub mod constant;
pub mod graph;
pub mod ir;
pub mod ir_gen;
pub mod lexer;
pub mod line_numbers;
pub mod macros;
pub mod parser;
pub mod proto_gen;
pub mod string_interner;

pub use compiler::{CompileSettings, CompilerError, compile};
