use gc_arena::{Gc, Mutation};
use thiserror::Error;

use fabricator_vm as vm;

use crate::{
    analysis::{
        block_merge::merge_blocks,
        cleanup::clean_unused_variables,
        cleanup::{clean_instructions, clean_unreachable_blocks, clean_unused_functions},
        constant_folding::fold_constants,
        dead_code_elim::eliminate_dead_code,
        eliminate_copies::eliminate_copies,
        shadow_liveness::ShadowVerificationError,
        shadow_reduction::reduce_shadows,
        ssa_conversion::convert_to_ssa,
    },
    codegen::codegen,
    frontend::{FrontendError, FrontendSettings},
    ir,
    magic_dict::{MagicDict, MagicMode},
    parser::{ParseError, ParseSettings},
    string_interner::StringInterner,
};

#[derive(Debug, Error)]
pub enum CompilerError {
    #[error(transparent)]
    Parsing(#[from] ParseError),
    #[error(transparent)]
    Frontend(#[from] FrontendError),
}

struct Interner<'a, 'gc>(&'a Mutation<'gc>);

impl<'a, 'gc> StringInterner for Interner<'a, 'gc> {
    type String = vm::String<'gc>;

    fn intern(&mut self, s: &str) -> vm::String<'gc> {
        vm::String::new(self.0, s)
    }
}

struct MDict<'gc>(Gc<'gc, vm::MagicSet<'gc>>);

impl<'gc> MagicDict<vm::String<'gc>> for MDict<'gc> {
    fn magic_mode(&self, ident: &vm::String<'gc>) -> Option<MagicMode> {
        let index = self.0.find(ident.as_str())?;
        let magic = self.0.get(index).unwrap();
        Some(if magic.read_only() {
            MagicMode::ReadOnly
        } else {
            MagicMode::ReadWrite
        })
    }

    fn magic_index(&self, ident: &vm::String<'gc>) -> Option<usize> {
        self.0.find(ident.as_str())
    }
}

pub fn compile<'gc>(
    mc: &Mutation<'gc>,
    stdlib: Gc<'gc, vm::MagicSet<'gc>>,
    src: &str,
) -> Result<vm::Prototype<'gc>, CompilerError> {
    let parsed = ParseSettings::default().parse(src, Interner(mc))?;
    let mut ir = FrontendSettings::default().compile_ir(&parsed, MDict(stdlib))?;

    optimize_ir(&mut ir).expect("Internal Optimization Error");
    let prototype = codegen(mc, &ir, MDict(stdlib)).expect("Internal Codegen Error");

    Ok(prototype)
}

pub fn compile_compat<'gc>(
    mc: &Mutation<'gc>,
    stdlib: Gc<'gc, vm::MagicSet<'gc>>,
    src: &str,
) -> Result<vm::Prototype<'gc>, CompilerError> {
    let parsed = ParseSettings { strict: false }.parse(src, Interner(mc))?;
    let mut ir = FrontendSettings {
        lexical_scoping: false,
        allow_closures: false,
    }
    .compile_ir(&parsed, MDict(stdlib))?;

    optimize_ir(&mut ir).expect("Internal Optimization Error");
    let prototype = codegen(mc, &ir, MDict(stdlib)).expect("Internal Codegen Error");

    Ok(prototype)
}

#[derive(Debug, Error)]
pub enum OptimizationError {
    #[error(transparent)]
    ShadowVariables(#[from] ShadowVerificationError),
}

pub fn optimize_ir<S: Clone + AsRef<str>>(
    ir: &mut ir::Function<S>,
) -> Result<(), OptimizationError> {
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
    clean_unused_functions(ir);
    clean_unused_variables(ir);

    for func in ir.functions.values_mut() {
        optimize_ir(func)?;
    }

    Ok(())
}
