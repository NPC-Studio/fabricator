use gc_arena::Mutation;
use thiserror::Error;

use crate::{
    closure::Prototype,
    compiler::{
        analysis::{
            dead_code_elim::eliminate_dead_code, eliminate_copies::eliminate_copies,
            shadow_liveness::ShadowVerificationError, shadow_reduction::reduce_shadows,
            ssa_conversion::convert_to_ssa,
        },
        codegen::codegen,
        frontend::{compile_ir, FrontendError},
        ir,
        parser::{parse, ParseError},
        string_interner::StringInterner,
    },
    string::String,
};

use super::analysis::{
    block_merge::merge_blocks,
    cleanup::{clean_instructions, clean_unreachable_blocks},
    constant_folding::fold_constants,
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
            String::new(self.0, s)
        }
    }

    let parsed = parse(src, Interner(mc)).unwrap();
    let mut ir = compile_ir(&parsed).unwrap();
    optimize_ir(&mut ir).expect("Internal Compiler Error");
    let prototype = codegen(ir).unwrap();

    Ok(prototype)
}

#[derive(Debug, Error)]
pub enum OptimizationError {
    #[error(transparent)]
    ShadowVariables(#[from] ShadowVerificationError),
}

pub fn optimize_ir<S: Clone>(ir: &mut ir::Function<S>) -> Result<(), OptimizationError> {
    convert_to_ssa(ir);
    fold_constants(ir);
    eliminate_dead_code(ir);
    reduce_shadows(ir)?;
    eliminate_copies(ir);
    fold_constants(ir);
    eliminate_dead_code(ir);
    merge_blocks(ir);
    clean_instructions(ir);
    clean_unreachable_blocks(ir);
    Ok(())
}
