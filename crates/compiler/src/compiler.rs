use std::path::Path;

use fabricator_vm as vm;
use gc_arena::{Collect, Gc};
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
    enums::{BadEnumVariant, EnumError, EnumSet},
    ir,
    ir_gen::{IrGenError, IrGenSettings, MagicMode},
    lexer::{LexError, Lexer, Token},
    line_numbers::LineNumbers,
    macros::{MacroError, MacroSet, RecursiveMacro},
    parser::{ParseError, ParseSettings},
    proto_gen::gen_prototype,
    string_interner::VmInterner,
};

#[derive(Debug, Error)]
pub enum CompileErrorKind {
    #[error("lex error: {0}")]
    Lexing(#[source] LexError),
    #[error("macro error: {0}")]
    Macro(#[source] MacroError),
    #[error("recursive macro: {0}")]
    RecursiveMacro(#[source] RecursiveMacro),
    #[error("enum error: {0}")]
    Enum(#[source] EnumError),
    #[error("enum error: {0}")]
    BadEnumVariant(#[source] BadEnumVariant),
    #[error("parse error: {0}")]
    Parsing(#[source] ParseError),
    #[error("IR gen error: {0}")]
    IrGen(#[source] IrGenError),
}

#[derive(Debug, Error)]
#[error("{kind} at {chunk_name}:{line_number}")]
pub struct CompileError {
    #[source]
    pub kind: CompileErrorKind,
    pub chunk_name: vm::RefName,
    pub line_number: vm::LineNumber,
}

#[derive(Debug, Copy, Clone)]
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

    /// If the given path has a (case-insensitive) `.gml` extension, then compile in compat mode,
    /// otherwise full.
    pub fn from_path(path: &Path) -> Self {
        if path
            .extension()
            .is_some_and(|e| e.eq_ignore_ascii_case("gml"))
        {
            Self::compat()
        } else {
            Self::full()
        }
    }
}

pub struct SourceChunk {
    pub name: vm::RefName,
    pub line_numbers: LineNumbers,
}

impl vm::ChunkData for SourceChunk {
    fn name(&self) -> &vm::RefName {
        &self.name
    }

    fn line_number(&self, byte_offset: usize) -> vm::LineNumber {
        self.line_numbers.line(byte_offset)
    }
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

/// Items shared across compilation units.
///
/// These will be accumulated by the compiler for separate compilation units and are part of the
/// compiler output.
///
/// These can be shared across different instances of a [`Compiler`] to control sharing between
/// different logical sets of FML scripts.
#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct GlobalItems<'gc> {
    macros: Option<Gc<'gc, MacroSet<vm::String<'gc>>>>,
    magic: Gc<'gc, vm::MagicSet<'gc>>,
}

impl<'gc> GlobalItems<'gc> {
    pub fn from_magic(magic: Gc<'gc, vm::MagicSet<'gc>>) -> Self {
        Self {
            macros: None,
            magic,
        }
    }
}

/// Compile FML code.
///
/// Compiles separate code units together in multiple phases to allow for cross referencing.
pub struct Compiler<'gc> {
    ctx: vm::Context<'gc>,
    macros: MacroSet<vm::String<'gc>>,
    magic: vm::MagicSet<'gc>,
    chunks: Vec<Chunk<'gc>>,
}

struct Chunk<'gc> {
    name: vm::RefName,
    line_numbers: LineNumbers,
    tokens: Vec<Token<vm::String<'gc>>>,
    compile_settings: CompileSettings,
}

impl<'gc> Compiler<'gc> {
    /// Compile a single chunk.
    ///
    /// Returns the chunk prototype as well as a merged `GlobalItems` set. This is a convenience
    /// method for creating a `Compiler` instance and compiling only a single chunk.
    pub fn compile_chunk(
        ctx: vm::Context<'gc>,
        globals: GlobalItems<'gc>,
        settings: CompileSettings,
        chunk_name: impl Into<String>,
        code: &str,
    ) -> Result<(Gc<'gc, vm::Prototype<'gc>>, GlobalItems<'gc>), CompileError> {
        let mut this = Self::new(ctx, globals);
        this.add_chunk(settings, chunk_name, code)?;
        let (mut prototypes, globals) = this.compile()?;
        Ok((prototypes.pop().unwrap(), globals))
    }

    pub fn new(ctx: vm::Context<'gc>, globals: GlobalItems<'gc>) -> Self {
        let macros = if let Some(macros) = globals.macros {
            (*macros).clone()
        } else {
            MacroSet::default()
        };

        let magic = (*globals.magic).clone();

        Self {
            ctx,
            macros,
            magic,
            chunks: Vec::new(),
        }
    }

    pub fn add_chunk(
        &mut self,
        settings: CompileSettings,
        chunk_name: impl Into<String>,
        code: &str,
    ) -> Result<(), CompileError> {
        let chunk_name = vm::RefName::new(chunk_name);
        let line_numbers = LineNumbers::new(code);

        let mut tokens = Vec::new();
        if let Err(err) = Lexer::tokenize(VmInterner::new(self.ctx), code, &mut tokens) {
            let line_number = line_numbers.line(err.span.start());
            return Err(CompileError {
                kind: CompileErrorKind::Lexing(err),
                chunk_name,
                line_number,
            });
        }
        self.chunks.push(Chunk {
            name: chunk_name,
            line_numbers,
            tokens,
            compile_settings: settings,
        });

        Ok(())
    }

    pub fn compile(
        self,
    ) -> Result<(Vec<Gc<'gc, vm::Prototype<'gc>>>, GlobalItems<'gc>), CompileError> {
        let Self {
            ctx,
            mut macros,
            magic,
            mut chunks,
        } = self;

        let magic = Gc::new(&ctx, magic);

        // List of starting macro indexes per chunk, to identify which macros come from which
        // chunks.
        let mut macro_chunk_indexes = Vec::new();

        for chunk in &mut chunks {
            macro_chunk_indexes.push(macros.len());
            if let Err(err) = macros.extract(&mut chunk.tokens) {
                let line_number = chunk.line_numbers.line(err.span.start());
                return Err(CompileError {
                    kind: CompileErrorKind::Macro(err),
                    chunk_name: chunk.name.clone(),
                    line_number,
                });
            }
        }

        if let Err(err) = macros.resolve_dependencies() {
            let makro = macros.get(err.0).unwrap();
            let chunk_index = match macro_chunk_indexes.binary_search_by(|i| i.cmp(&err.0)) {
                Ok(i) => macro_chunk_indexes[i],
                Err(i) => {
                    assert!(
                        i > 0,
                        "pre-existing macros should not have recursion errors"
                    );
                    macro_chunk_indexes[i - 1]
                }
            };
            let chunk = &chunks[macro_chunk_indexes[chunk_index]];
            let line_number = chunk.line_numbers.line(makro.span.start());
            return Err(CompileError {
                kind: CompileErrorKind::RecursiveMacro(err),
                chunk_name: chunk.name.clone(),
                line_number,
            });
        }

        let mut parsed_chunks = Vec::new();

        for chunk in chunks {
            let Chunk {
                name,
                line_numbers,
                mut tokens,
                compile_settings,
            } = chunk;
            macros.expand(&mut tokens);

            let chunk = vm::Chunk::new_static(
                &ctx,
                SourceChunk {
                    name: name.clone(),
                    line_numbers,
                },
            );

            let block = compile_settings.parse.parse(tokens).map_err(|e| {
                let line_number = chunk.line_number(e.span.start());
                CompileError {
                    kind: CompileErrorKind::Parsing(e),
                    chunk_name: name.clone(),
                    line_number,
                }
            })?;

            parsed_chunks.push((chunk, block, compile_settings));
        }

        let mut enums = EnumSet::new();

        for (chunk, block, _) in &mut parsed_chunks {
            if let Err(err) = enums.extract(block) {
                let line_number = chunk.line_number(err.span.start());
                return Err(CompileError {
                    kind: CompileErrorKind::Enum(err),
                    chunk_name: chunk.name().clone(),
                    line_number,
                });
            }
        }

        let mut prototypes = Vec::new();

        for (chunk, mut block, compile_settings) in parsed_chunks {
            if let Err(err) = enums.expand_block(&mut block) {
                let line_number = chunk.line_number(err.span.start());
                return Err(CompileError {
                    kind: CompileErrorKind::BadEnumVariant(err),
                    chunk_name: chunk.name().clone(),
                    line_number,
                });
            }

            let mut ir = compile_settings
                .ir_gen
                .gen_ir(&block, |m| {
                    let i = magic.find(m)?;
                    Some(if magic.get(i).unwrap().read_only() {
                        MagicMode::ReadOnly
                    } else {
                        MagicMode::ReadWrite
                    })
                })
                .map_err(|e| {
                    let line_number = chunk.line_number(e.span.start());
                    CompileError {
                        kind: CompileErrorKind::IrGen(e),
                        chunk_name: chunk.name().clone(),
                        line_number,
                    }
                })?;

            optimize_ir(&mut ir).expect("Internal Optimization Error");

            prototypes.push(Gc::new(
                &ctx,
                gen_prototype(&ctx, &ir, chunk, magic).expect("Internal Codegen Error"),
            ));
        }

        let globals = GlobalItems {
            macros: Some(Gc::new(&ctx, macros)),
            magic,
        };

        Ok((prototypes, globals))
    }
}
