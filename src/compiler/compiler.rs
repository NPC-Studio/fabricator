use gc_arena::{Gc, Mutation};
use thiserror::Error;

use crate::{
    closure::Prototype,
    compiler::{
        analysis::ssa_conversion::convert_to_ssa,
        codegen::codegen,
        frontend::{compile_ir, FrontendError},
        parser::{parse, ParseError},
        string_interner::StringInterner,
    },
    value::String,
};

#[derive(Debug, Error)]
pub enum CompilerError {
    #[error(transparent)]
    Parsing(#[from] ParseError),
    #[error(transparent)]
    Frontend(#[from] FrontendError),
}

pub fn compile<'gc>(mc: &Mutation<'gc>, src: &str) -> Result<Prototype<'gc>, CompilerError> {
    struct Interner<'a, 'gc>(&'a Mutation<'gc>);

    impl<'a, 'gc> StringInterner for Interner<'a, 'gc> {
        type String = String<'gc>;

        fn intern(&mut self, s: &str) -> String<'gc> {
            String(Gc::new(self.0, s.to_string()))
        }
    }

    let parsed = parse(src, Interner(mc)).unwrap();
    let mut compiled = compile_ir(&parsed).unwrap();
    convert_to_ssa(&mut compiled);
    let prototype = codegen(compiled).unwrap();

    Ok(prototype)
}
