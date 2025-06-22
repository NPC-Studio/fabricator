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
    ir,
    ir_gen::{IrGenError, IrGenSettings},
    lexer::{LexError, Lexer},
    line_numbers::LineNumbers,
    magic_dict::{MagicDict, MagicMode},
    parser::{ParseError, ParseSettings},
    proto_gen::gen_prototype,
    string_interner::VmInterner,
};

#[derive(Debug, Error)]
pub enum CompilerErrorKind {
    #[error("lex error: {0}")]
    Lexing(#[source] LexError),
    #[error("parse error: {0}")]
    Parsing(#[source] ParseError),
    #[error("IR gen error: {0}")]
    IrGen(#[source] IrGenError),
}

#[derive(Debug, Error)]
#[error("{kind} at line {line_number}")]
pub struct CompilerError {
    #[source]
    pub kind: CompilerErrorKind,
    pub line_number: vm::LineNumber,
}

pub struct CompileSettings {
    pub parse: ParseSettings,
    pub ir_gen: IrGenSettings,
}

impl CompileSettings {
    pub fn compat() -> Self {
        Self {
            parse: ParseSettings::compat(),
            ir_gen: IrGenSettings::compat(),
        }
    }

    pub fn full() -> Self {
        Self {
            parse: ParseSettings::strict(),
            ir_gen: IrGenSettings::full(),
        }
    }
}

pub struct SourceChunk {
    pub name: vm::RefName,
    pub line_numbers: LineNumbers,
}

impl SourceChunk {
    pub fn new(name: &str, src: &str) -> Self {
        Self {
            name: vm::RefName::new(name),
            line_numbers: LineNumbers::new(src),
        }
    }
}

impl vm::ChunkData for SourceChunk {
    fn name(&self) -> &vm::RefName {
        &self.name
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
    settings: CompileSettings,
    stdlib: Gc<'gc, vm::MagicSet<'gc>>,
    chunk_name: &str,
    src: &str,
) -> Result<vm::Prototype<'gc>, CompilerError> {
    let stdlib = VmMagic::new(stdlib);
    let chunk = vm::Chunk::new_static(&ctx, SourceChunk::new(chunk_name, src));

    let mut tokens = Vec::new();
    Lexer::tokenize(VmInterner::new(ctx), src, &mut tokens).map_err(|e| {
        let line_number = chunk.line_number(e.span.start());
        CompilerError {
            kind: CompilerErrorKind::Lexing(e),
            line_number,
        }
    })?;

    let parsed = settings.parse.parse(tokens).map_err(|e| {
        let line_number = chunk.line_number(e.span.start());
        CompilerError {
            kind: CompilerErrorKind::Parsing(e),
            line_number,
        }
    })?;

    let mut ir = settings.ir_gen.gen_ir(&parsed, stdlib).map_err(|e| {
        let line_number = chunk.line_number(e.span.start());
        CompilerError {
            kind: CompilerErrorKind::IrGen(e),
            line_number,
        }
    })?;

    optimize_ir(&mut ir).expect("Internal Optimization Error");

    let prototype = gen_prototype(&ctx, &ir, chunk, stdlib).expect("Internal Codegen Error");

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
