use fabricator_vm as vm;
use thiserror::Error;

use crate::{
    ast,
    enums::{EnumError, EnumEvaluationError, EnumResolutionError, EnumSet, EnumSetBuilder},
    exports::{DuplicateExportError, ExportSet, ExportSettings},
    lexer::{LexError, Lexer},
    line_numbers::LineNumbers,
    macros::{MacroError, MacroSet, RecursiveMacro},
    parser::{ParseError, ParseSettings},
    string_interner::VmInterner,
    tokens::Token,
};

#[derive(Debug, Error)]
#[error("enum or export `{name}` conflicts with an existing special")]
pub struct ShadowsSpecialError {
    pub name: String,
    pub span: vm::Span,
}

#[derive(Debug, Error)]
pub enum PreprocessErrorKind {
    #[error("macro error: {0}")]
    Macro(#[source] MacroError),
    #[error("recursive macro: {0}")]
    RecursiveMacro(#[source] RecursiveMacro),
    #[error("parse error: {0}")]
    Parsing(#[source] ParseError),
    #[error("enum error: {0}")]
    Enum(#[source] EnumError),
    #[error("enum error: {0}")]
    EnumResolution(#[source] EnumResolutionError),
    #[error("enum error: {0}")]
    EnumEvaluation(#[source] EnumEvaluationError),
    #[error("duplicate export error: {0}")]
    DuplicateExport(#[source] DuplicateExportError),
    #[error("shadows special error: {0}")]
    ShadowsSpecial(#[source] ShadowsSpecialError),
}

#[derive(Debug, Error)]
#[error("{kind} at {chunk_name}:{line_number}")]
pub struct PreprocessError {
    #[source]
    pub kind: PreprocessErrorKind,
    pub chunk_name: vm::SharedStr,
    pub line_number: vm::LineNumber,
}

#[derive(Debug, Error)]
#[error("{error} at {chunk_name}:{line_number}")]
pub struct ChunkLexError {
    #[source]
    pub error: LexError,
    pub chunk_name: vm::SharedStr,
    pub line_number: vm::LineNumber,
}

/// Extracts and resolves macros, then extracts and resolves enums, then extracts exported functions
/// and globalvar declarations.
pub struct Preprocessor<'gc> {
    ctx: vm::Context<'gc>,
    config: String,
    macros: MacroSet<vm::String<'gc>>,
    enums: EnumSetBuilder<vm::String<'gc>>,
    chunk_inputs: Vec<ChunkInput<'gc>>,
}

impl<'gc> Preprocessor<'gc> {
    /// Lex the given chunk and produce the token stream and a `vm::Chunk` identifier.
    pub fn lex(
        ctx: vm::Context<'gc>,
        chunk_name: impl Into<vm::SharedStr>,
        code: &str,
    ) -> Result<LexedChunk<'gc>, ChunkLexError> {
        let chunk = vm::Chunk::new_static(
            &ctx,
            SourceChunk {
                name: chunk_name.into(),
                line_numbers: LineNumbers::new(code),
            },
        );

        let mut tokens = Vec::new();
        if let Err(error) = Lexer::tokenize(VmInterner::new(ctx), code, &mut tokens) {
            let line_number = chunk.line_number(error.span.start());
            return Err(ChunkLexError {
                error,
                chunk_name: chunk.name().clone(),
                line_number,
            });
        }

        Ok(LexedChunk { chunk, tokens })
    }

    pub fn new(
        ctx: vm::Context<'gc>,
        config: impl Into<String>,
        external_macros: MacroSet<vm::String<'gc>>,
        external_enums: EnumSet<vm::String<'gc>>,
    ) -> Self {
        Self {
            ctx,
            config: config.into(),
            macros: external_macros,
            enums: external_enums.into_builder(),
            chunk_inputs: Vec::new(),
        }
    }

    pub fn add_chunk(
        &mut self,
        parse_settings: ParseSettings,
        export_top_level_funcs: bool,
        chunk_name: impl Into<vm::SharedStr>,
        code: &str,
    ) -> Result<(), ChunkLexError> {
        self.add_lexed_chunk(
            Self::lex(self.ctx, chunk_name, code)?,
            parse_settings,
            export_top_level_funcs,
        );
        Ok(())
    }

    pub fn add_lexed_chunk(
        &mut self,
        lexed_chunk: LexedChunk<'gc>,
        parse_settings: ParseSettings,
        export_top_level_funcs: bool,
    ) {
        self.chunk_inputs.push(ChunkInput {
            chunk: lexed_chunk.chunk,
            tokens: lexed_chunk.tokens,
            parse_settings,
            export_top_level_funcs,
        });
    }

    pub fn chunk_len(&self) -> usize {
        self.chunk_inputs.len()
    }

    /// Preprocess all added chunks.
    ///
    /// The `is_special` callback is expected to return `true` when an identifier should be
    /// considered reserved for enums and exports, such as the names of magic variables or exports
    /// in previous compilation units.
    ///
    /// It is possible for a `ShadowsSpecial` error to be returned for a new export shadowing either
    /// an external enum or a newly parsed enum, as those are also considered special. Thus, it is
    /// not necessary for the `is_special` callback to check for identifiers for enums passed in
    /// as `external_enums`.
    pub fn preprocess(
        self,
        is_special: impl Fn(vm::String<'gc>) -> bool,
    ) -> Result<PreprocessOutput<'gc>, PreprocessError> {
        let Self {
            ctx,
            config,
            mut macros,
            mut enums,
            mut chunk_inputs,
        } = self;

        // Extract all macro definitions from all chunks.

        // List of starting macro indexes per chunk, to identify which macros come from which chunk.
        let mut macro_chunk_indexes = Vec::new();

        for input in &mut chunk_inputs {
            macro_chunk_indexes.push(macros.len());
            if let Err(err) = macros.extract(&mut input.tokens) {
                let line_number = input.chunk.line_number(err.span.start());
                return Err(PreprocessError {
                    kind: PreprocessErrorKind::Macro(err),
                    chunk_name: input.chunk.name().clone(),
                    line_number,
                });
            }
        }

        // Apply a config and resolve all macro interdependencies.

        let resolved_macros =
            match macros
                .clone()
                .resolve_with_skip_recursive(&ctx.intern(&config), |&token| {
                    // GMS2 doesn't expand macro tokens that match a defined macro if the token name
                    // is also a part of the stdlib. This allows re-defining builtins with macros
                    // (at the cost of making the macro system even more complicated and special
                    // case).
                    //
                    // We do something similar here, except we skip expansion for *all* specials
                    // (exports defined in a previous compilation unit or magic variables).
                    !is_special(token)
                }) {
                Ok(macros) => macros,
                Err(err) => {
                    let macro_ = macros.get(err.0).unwrap();
                    let chunk_index = match macro_chunk_indexes.binary_search_by(|i| i.cmp(&err.0))
                    {
                        Ok(i) => i,
                        Err(i) => i
                            .checked_sub(1)
                            .expect("pre-existing macros should not have recursion errors"),
                    };
                    let chunk_input = &chunk_inputs[chunk_index];
                    let line_number = chunk_input.chunk.line_number(macro_.span.start());
                    return Err(PreprocessError {
                        kind: PreprocessErrorKind::RecursiveMacro(err),
                        chunk_name: chunk_input.chunk.name().clone(),
                        line_number,
                    });
                }
            };

        // Use macro definitions to replace macro instances in every chunk, then parse the resulting
        // token list.

        let mut preprocessing_chunks = Vec::new();

        for chunk in chunk_inputs {
            let ChunkInput {
                chunk,
                mut tokens,
                parse_settings,
                export_top_level_funcs,
            } = chunk;
            resolved_macros.expand(&mut tokens);

            let block = parse_settings.parse(tokens).map_err(|e| {
                let line_number = chunk.line_number(e.span.start());
                PreprocessError {
                    kind: PreprocessErrorKind::Parsing(e),
                    chunk_name: chunk.name().clone(),
                    line_number,
                }
            })?;

            preprocessing_chunks.push(((block, chunk), export_top_level_funcs));
        }

        // Extract all enum definitions from every parsed AST, and make sure none of the enum names
        // conflict with any pre-existing special.

        // List of starting enum indexes per chunk, to identify which enums come from which chunk.
        let mut enum_chunk_indexes = Vec::new();

        for ((block, chunk), _) in &mut preprocessing_chunks {
            let prev_enum_len = enums.len();
            enum_chunk_indexes.push(prev_enum_len);

            if let Err(err) = enums.extract(block) {
                let line_number = chunk.line_number(err.span.start());
                return Err(PreprocessError {
                    kind: PreprocessErrorKind::Enum(err),
                    chunk_name: chunk.name().clone(),
                    line_number,
                });
            }

            for i in prev_enum_len..enums.len() {
                let enum_ = enums.get(i).unwrap();
                // Enums are not allowed to shadow names of existing specials.
                if is_special(enum_.name.inner) {
                    let line_number = chunk.line_number(enum_.span.start());
                    return Err(PreprocessError {
                        kind: PreprocessErrorKind::ShadowsSpecial(ShadowsSpecialError {
                            name: enum_.name.as_str().to_owned(),
                            span: enum_.span,
                        }),
                        chunk_name: chunk.name().clone(),
                        line_number,
                    });
                }
            }
        }

        // Resolve all enum interdependencies.

        let enums = enums.clone().resolve().map_err(|err| {
            let enum_ = enums.get(err.enum_index).unwrap();
            let chunk_index = match enum_chunk_indexes.binary_search_by(|i| i.cmp(&err.enum_index))
            {
                Ok(i) => i,
                Err(i) => i
                    .checked_sub(1)
                    .expect("pre-existing enums should not have recursion errors"),
            };
            let (_, chunk) = &preprocessing_chunks[chunk_index].0;
            let line_number = chunk.line_number(enum_.span.start());
            PreprocessError {
                kind: PreprocessErrorKind::EnumResolution(err),
                chunk_name: chunk.name().clone(),
                line_number,
            }
        })?;

        // Check and expand any references to enums in every AST.

        for ((block, chunk), _) in &mut preprocessing_chunks {
            if let Err(err) = enums.expand(block) {
                let line_number = chunk.line_number(err.span.start());
                return Err(PreprocessError {
                    kind: PreprocessErrorKind::EnumEvaluation(err),
                    chunk_name: chunk.name().clone(),
                    line_number,
                });
            }
        }

        // Gather all exported items from every AST, making sure that the exported name does not
        // conflict with any pre-existing special.

        let mut exports = ExportSet::new();

        // List of starting export indexes per chunk, to identify which exports come from which
        // chunk.
        let mut export_chunk_indexes = Vec::new();

        for &mut ((ref mut block, ref mut chunk), export_top_level_funcs) in
            &mut preprocessing_chunks
        {
            let prev_exports_len = exports.len();
            export_chunk_indexes.push(prev_exports_len);

            if let Err(err) = exports.extract(
                block,
                ExportSettings {
                    export_top_level_functions: export_top_level_funcs,
                },
            ) {
                let line_number = chunk.line_number(err.span.start());
                return Err(PreprocessError {
                    kind: PreprocessErrorKind::DuplicateExport(err),
                    chunk_name: chunk.name().clone(),
                    line_number,
                });
            }

            for i in prev_exports_len..exports.len() {
                let export = exports.get(i).unwrap();
                let export_name = export.name();
                let export_span = export.span();

                // Exports are not allowed to shadow names of existing specials.
                if is_special(*export_name) || enums.find(export_name).is_some() {
                    let line_number = chunk.line_number(export_span.start());
                    return Err(PreprocessError {
                        kind: PreprocessErrorKind::ShadowsSpecial(ShadowsSpecialError {
                            name: export_name.as_str().to_owned(),
                            span: export_span,
                        }),
                        chunk_name: chunk.name().clone(),
                        line_number,
                    });
                }
            }
        }

        Ok(PreprocessOutput {
            preprocessed_chunks: preprocessing_chunks.into_iter().map(|(c, _)| c).collect(),
            macros,
            macro_chunk_indexes,
            enums,
            enum_chunk_indexes,
            exports,
            export_chunk_indexes,
        })
    }
}

pub struct PreprocessOutput<'gc> {
    pub preprocessed_chunks: Vec<(ast::Block<vm::String<'gc>>, vm::Chunk<'gc>)>,

    pub macros: MacroSet<vm::String<'gc>>,
    pub macro_chunk_indexes: Vec<usize>,

    pub enums: EnumSet<vm::String<'gc>>,
    pub enum_chunk_indexes: Vec<usize>,

    pub exports: ExportSet<vm::String<'gc>>,
    pub export_chunk_indexes: Vec<usize>,
}

pub struct SourceChunk {
    pub name: vm::SharedStr,
    pub line_numbers: LineNumbers,
}

impl vm::debug::ChunkData for SourceChunk {
    fn name(&self) -> &vm::SharedStr {
        &self.name
    }

    fn line_number(&self, byte_offset: usize) -> vm::LineNumber {
        self.line_numbers.line(byte_offset)
    }
}

#[derive(Clone)]
pub struct LexedChunk<'gc> {
    pub chunk: vm::Chunk<'gc>,
    pub tokens: Vec<Token<vm::String<'gc>>>,
}

struct ChunkInput<'gc> {
    chunk: vm::Chunk<'gc>,
    tokens: Vec<Token<vm::String<'gc>>>,
    parse_settings: ParseSettings,
    export_top_level_funcs: bool,
}
