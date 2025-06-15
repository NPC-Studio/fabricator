use fabricator_vm as vm;
use gc_arena::Gc;
use thiserror::Error;

use crate::{
    analysis::{
        block_merge::merge_blocks,
        cleanup::{
            clean_instructions, clean_unreachable_blocks, clean_unused_functions,
            clean_unused_variables,
        },
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
    line_numbers::LineNumbers,
    magic_dict::{MagicDict, MagicMode},
    parser::{ParseError, ParseSettings},
    string_interner::VmInterner,
};

#[derive(Debug, Error)]
pub enum CompilerError {
    #[error(transparent)]
    Parsing(#[from] ParseError),
    #[error(transparent)]
    Frontend(#[from] FrontendError),
}

pub struct SourceChunk {
    name: String,
    line_numbers: LineNumbers,
}

impl SourceChunk {
    pub fn new(name: &str, src: &str) -> Self {
        Self {
            name: name.to_owned(),
            line_numbers: LineNumbers::new(src),
        }
    }
}

impl vm::ChunkData for SourceChunk {
    fn name(&self) -> &str {
        self.name.as_str()
    }

    fn line_number(&self, byte_offset: usize) -> vm::LineNumber {
        self.line_numbers.line(byte_offset)
    }
}

#[derive(Copy, Clone)]
pub struct VmMagic<'gc>(Gc<'gc, vm::MagicSet<'gc>>);

impl<'gc> VmMagic<'gc> {
    pub fn new(magic: Gc<'gc, vm::MagicSet<'gc>>) -> Self {
        Self(magic)
    }
}

impl<'gc> MagicDict<vm::String<'gc>> for VmMagic<'gc> {
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
    ctx: vm::Context<'gc>,
    stdlib: Gc<'gc, vm::MagicSet<'gc>>,
    chunk_name: &str,
    src: &str,
) -> Result<vm::Prototype<'gc>, CompilerError> {
    let stdlib = VmMagic::new(stdlib);

    let parsed = ParseSettings::default().parse(src, VmInterner(ctx))?;

    let mut ir = FrontendSettings::default().compile_ir(&parsed, stdlib)?;
    optimize_ir(&mut ir).expect("Internal Optimization Error");

    let chunk = vm::Chunk::new_static(&ctx, SourceChunk::new(chunk_name, src));

    let prototype = codegen(&ctx, &ir, chunk, stdlib).expect("Internal Codegen Error");

    Ok(prototype)
}

pub fn compile_compat<'gc>(
    ctx: vm::Context<'gc>,
    stdlib: Gc<'gc, vm::MagicSet<'gc>>,
    chunk_name: &str,
    src: &str,
) -> Result<vm::Prototype<'gc>, CompilerError> {
    let stdlib = VmMagic::new(stdlib);

    let parsed = ParseSettings { strict: false }.parse(src, VmInterner(ctx))?;

    let mut ir = FrontendSettings {
        lexical_scoping: false,
        allow_closures: false,
    }
    .compile_ir(&parsed, stdlib)?;
    optimize_ir(&mut ir).expect("Internal Optimization Error");

    let chunk = vm::Chunk::new_static(&ctx, SourceChunk::new(chunk_name, src));

    let prototype = codegen(&ctx, &ir, chunk, stdlib).expect("Internal Codegen Error");

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
