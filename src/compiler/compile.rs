use gc_arena::{Gc, Mutation};
use thiserror::Error;

use crate::{
    closure::Prototype,
    compiler::{
        analysis::{
            dead_code_elim::eliminate_dead_code, eliminate_copies::eliminate_copies,
            remove_dead_upsilons::remove_dead_upsilons, shadow_liveness::ShadowVerificationError,
            ssa_conversion::convert_to_ssa,
        },
        codegen::codegen,
        frontend::{compile_ir, FrontendError},
        ir,
        parser::{parse, ParseError},
        string_interner::StringInterner,
    },
    value::String,
};

use super::analysis::cleanup::{clean_instructions, clean_unreachable_blocks};

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
    let mut ir = compile_ir(&parsed).unwrap();
    optimize_ir(&mut ir).expect("Internal Compiler Error");
    let prototype = codegen(ir).unwrap();

    Ok(prototype)
}

#[derive(Debug, Error)]
pub enum IrError {
    #[error(transparent)]
    ShadowVariables(#[from] ShadowVerificationError),
}

pub fn optimize_ir<S: AsRef<str>>(ir: &mut ir::Function<S>) -> Result<(), IrError> {
    convert_to_ssa(ir);
    eliminate_copies(ir);
    eliminate_dead_code(ir);
    remove_dead_upsilons(ir)?;
    clean_instructions(ir);
    clean_unreachable_blocks(ir);
    Ok(())
}
