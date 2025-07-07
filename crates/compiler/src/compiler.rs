use std::path::Path;

use fabricator_vm::{self as vm, magic::MagicConstant};
use gc_arena::{Collect, Gc};
use thiserror::Error;

use crate::{
    analysis::{
        block_merge::merge_blocks,
        cleanup::{
            clean_instructions, clean_unreachable_blocks, clean_unused_functions,
            clean_unused_shadow_vars, clean_unused_this_scopes, clean_unused_variables,
        },
        constant_folding::fold_constants,
        dead_code_elim::eliminate_dead_code,
        eliminate_copies::eliminate_copies,
        instruction_liveness::{InstructionLiveness, InstructionVerificationError},
        shadow_liveness::{ShadowLiveness, ShadowVerificationError},
        shadow_reduction::reduce_shadows,
        ssa_conversion::convert_to_ssa,
        variable_liveness::{VariableLiveness, VariableVerificationError},
        verify_references::{ReferenceVerificationError, verify_references},
        verify_scopes::{ScopeVerificationError, verify_scopes},
    },
    code_gen::gen_prototype,
    enums::{EnumError, EnumEvaluationError, EnumSet},
    exports::{DuplicateExportError, ExportKind, ExportSet},
    ir,
    ir_gen::{IrGenError, IrGenSettings, MagicMode},
    lexer::{LexError, Lexer},
    line_numbers::LineNumbers,
    macros::{MacroError, MacroSet, RecursiveMacro},
    parser::{ParseError, ParseSettings},
    string_interner::VmInterner,
    tokens::Token,
};

#[derive(Debug, Error)]
pub enum CompileErrorKind {
    #[error("lex error: {0}")]
    Lexing(#[source] LexError),
    #[error("macro error: {0}")]
    Macro(#[source] MacroError),
    #[error("recursive macro: {0}")]
    RecursiveMacro(#[source] RecursiveMacro),
    #[error("parse error: {0}")]
    Parsing(#[source] ParseError),
    #[error("enum error: {0}")]
    Enum(#[source] EnumError),
    #[error("enum error: {0}")]
    EnumEvaluation(#[source] EnumEvaluationError),
    #[error("duplicate export error: {0}")]
    DuplicateExport(#[source] DuplicateExportError),
    #[error("item `{name}` shares a name with an existing magic variable")]
    ItemShadowsMagic { name: String, span: vm::Span },
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

impl vm::debug::ChunkData for SourceChunk {
    fn name(&self) -> &vm::RefName {
        &self.name
    }

    fn line_number(&self, byte_offset: usize) -> vm::LineNumber {
        self.line_numbers.line(byte_offset)
    }
}

#[derive(Debug, Error)]
pub enum IrVerificationError {
    #[error(transparent)]
    ReferenceVerification(#[from] ReferenceVerificationError),
    #[error(transparent)]
    InstructionVerification(#[from] InstructionVerificationError),
    #[error(transparent)]
    ShadowVerification(#[from] ShadowVerificationError),
    #[error(transparent)]
    VariableVerification(#[from] VariableVerificationError),
    #[error(transparent)]
    ScopeVerification(#[from] ScopeVerificationError),
}

/// Verify that IR is well-formed.
pub fn verify_ir<S: Clone>(ir: &ir::Function<S>) -> Result<(), IrVerificationError> {
    verify_references(ir)?;
    InstructionLiveness::compute(ir)?;
    ShadowLiveness::compute(ir)?;
    VariableLiveness::compute(ir)?;
    verify_scopes(ir)?;
    Ok(())
}

/// Run optimization passes on IR.
///
/// # Panics
///
/// May panic if the provided IR is not well-formed.
pub fn optimize_ir<S: Clone>(ir: &mut ir::Function<S>) {
    // Try and eliminate any unused child functions before optimizing them.
    eliminate_dead_code(ir);
    clean_unreachable_blocks(ir);
    clean_unused_functions(ir);

    // Optimize all child functions first, which may remove variable references to this parent
    // function, allowing for more SSA conversion.
    for func in ir.functions.values_mut() {
        optimize_ir(func);
    }

    convert_to_ssa(ir);

    fold_constants(ir);
    eliminate_dead_code(ir);
    reduce_shadows(ir).unwrap();

    eliminate_copies(ir);
    fold_constants(ir);
    eliminate_dead_code(ir);
    merge_blocks(ir);

    clean_unreachable_blocks(ir);
    clean_instructions(ir);
    clean_unused_variables(ir);
    clean_unused_shadow_vars(ir);
    clean_unused_this_scopes(ir);
    clean_unused_functions(ir);
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
pub struct ImportItems<'gc> {
    macros: Option<Gc<'gc, MacroSet<vm::String<'gc>>>>,
    enums: Option<Gc<'gc, EnumSet<vm::String<'gc>>>>,
    magic: Gc<'gc, vm::MagicSet<'gc>>,
}

impl<'gc> ImportItems<'gc> {
    pub fn from_magic(magic: Gc<'gc, vm::MagicSet<'gc>>) -> Self {
        Self {
            macros: None,
            enums: None,
            magic,
        }
    }
}

/// Compile FML code.
///
/// Compiles separate code units together in multiple phases to allow for cross referencing.
pub struct Compiler<'gc> {
    ctx: vm::Context<'gc>,
    config: String,
    macros: MacroSet<vm::String<'gc>>,
    enums: EnumSet<vm::String<'gc>>,
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
    /// Returns the chunk prototype as well as a merged `ImportItems` set. This is a convenience
    /// method for creating a `Compiler` instance and compiling only a single chunk.
    pub fn compile_chunk(
        ctx: vm::Context<'gc>,
        config: impl Into<String>,
        imports: ImportItems<'gc>,
        settings: CompileSettings,
        chunk_name: impl Into<String>,
        code: &str,
    ) -> Result<(Gc<'gc, vm::Prototype<'gc>>, ImportItems<'gc>), CompileError> {
        let mut this = Self::new(ctx, config, imports);
        this.add_chunk(settings, chunk_name, code)?;
        let (mut prototypes, imports) = this.compile()?;
        Ok((prototypes.pop().unwrap(), imports))
    }

    pub fn new(
        ctx: vm::Context<'gc>,
        config: impl Into<String>,
        imports: ImportItems<'gc>,
    ) -> Self {
        let macros = imports.macros.as_deref().cloned().unwrap_or_default();
        let enums = imports.enums.as_deref().cloned().unwrap_or_default();
        let magic = imports.magic.as_ref().clone();

        Self {
            ctx,
            config: config.into(),
            macros,
            enums,
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
    ) -> Result<(Vec<Gc<'gc, vm::Prototype<'gc>>>, ImportItems<'gc>), CompileError> {
        let Self {
            ctx,
            config,
            mut macros,
            mut enums,
            mut magic,
            mut chunks,
        } = self;

        // Extract all macro definitions from all chunks.

        // List of starting macro indexes per chunk, to identify which macros come from which
        // chunk.
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

        // Apply a config and resolve all macro interdependencies.

        let resolved_macros = match macros.clone().resolve(Some(config.as_str())) {
            Ok(macros) => macros,
            Err(err) => {
                let makro = macros.get(err.0).unwrap();
                let chunk_index = match macro_chunk_indexes.binary_search_by(|i| i.cmp(&err.0)) {
                    Ok(i) => i,
                    Err(i) => i
                        .checked_sub(1)
                        .expect("pre-existing macros should not have recursion errors"),
                };
                let chunk = &chunks[chunk_index];
                let line_number = chunk.line_numbers.line(makro.span.start());
                return Err(CompileError {
                    kind: CompileErrorKind::RecursiveMacro(err),
                    chunk_name: chunk.name.clone(),
                    line_number,
                });
            }
        };

        // Use macro definitions to replace macro instances in every chunk, then parse the resulting
        // token list.

        let mut parsed_chunks = Vec::new();

        for chunk in chunks {
            let Chunk {
                name,
                line_numbers,
                mut tokens,
                compile_settings,
            } = chunk;
            resolved_macros.expand(&mut tokens);

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

        // Extract all enum definitions from every parsed AST, and make sure none of the enum names
        // conflict with a magic variable name.

        for &mut (chunk, ref mut block, _) in &mut parsed_chunks {
            let prev_enum_len = enums.len();

            if let Err(err) = enums.extract(block) {
                let line_number = chunk.line_number(err.span.start());
                return Err(CompileError {
                    kind: CompileErrorKind::Enum(err),
                    chunk_name: chunk.name().clone(),
                    line_number,
                });
            }

            for i in prev_enum_len..enums.len() {
                let enom = enums.get(i).unwrap();
                if magic.find(&enom.name).is_some() {
                    let line_number = chunk.line_number(enom.span.start());
                    return Err(CompileError {
                        kind: CompileErrorKind::ItemShadowsMagic {
                            name: enom.name.as_str().to_owned(),
                            span: enom.span,
                        },
                        chunk_name: chunk.name().clone(),
                        line_number,
                    });
                }
            }
        }

        // Check and expand any references to enums in every AST.

        for &mut (chunk, ref mut block, _) in &mut parsed_chunks {
            if let Err(err) = enums.expand(block) {
                let line_number = chunk.line_number(err.span.start());
                return Err(CompileError {
                    kind: CompileErrorKind::EnumEvaluation(err),
                    chunk_name: chunk.name().clone(),
                    line_number,
                });
            }
        }

        // Gather all exported items from every AST, then produce a new read-only *stub* magic
        // variable for each export, making sure that the exported name does not conflict with any
        // pre-existing magic variable.

        let mut exports = ExportSet::new();

        // List of starting export indexes per chunk, to identify which exports come from which
        // chunk.
        let mut export_chunk_indexes = Vec::new();

        // The magic index for each exported item.
        let mut export_magic_indexes = Vec::new();

        let stub_magic = MagicConstant::new_ptr(&ctx, vm::Value::Undefined);

        for &mut (chunk, ref mut block, _) in &mut parsed_chunks {
            let prev_exports_len = exports.len();
            export_chunk_indexes.push(prev_exports_len);

            if let Err(err) = exports.extract(block) {
                let line_number = chunk.line_number(err.span.start());
                return Err(CompileError {
                    kind: CompileErrorKind::DuplicateExport(err),
                    chunk_name: chunk.name().clone(),
                    line_number,
                });
            }

            for i in prev_exports_len..exports.len() {
                let export = exports.get(i).unwrap();
                let (index, inserted) = magic.insert(export.name, stub_magic);

                if !inserted {
                    let line_number = chunk.line_number(export.span.start());
                    return Err(CompileError {
                        kind: CompileErrorKind::ItemShadowsMagic {
                            name: export.name.as_str().to_owned(),
                            span: export.span,
                        },
                        chunk_name: chunk.name().clone(),
                        line_number,
                    });
                }

                export_magic_indexes.push(index);
            }
        }

        let magic = Gc::new(&ctx, magic);
        let magic_write = Gc::write(&ctx, magic);

        // For each exported item, produce a real magic value for it.

        for (i, export) in exports.iter().enumerate() {
            let chunk_index = match export_chunk_indexes.binary_search_by(|i| i.cmp(&i)) {
                Ok(i) => i,
                Err(i) => i.checked_sub(1).unwrap(),
            };
            let (chunk, _, compile_settings) = parsed_chunks[chunk_index];

            let magic_index = export_magic_indexes[i];

            match &export.kind {
                ExportKind::Function(func_stmt) => {
                    assert!(
                        !func_stmt.is_constructor && func_stmt.inherit.is_none(),
                        "constructor functions are not supported yet"
                    );

                    let mut ir = compile_settings
                        .ir_gen
                        .gen_ir(
                            vm::FunctionRef::Chunk,
                            &func_stmt.parameters,
                            &func_stmt.body,
                            |m| {
                                let i = magic.find(m)?;
                                Some(if magic.get(i).unwrap().read_only() {
                                    MagicMode::ReadOnly
                                } else {
                                    MagicMode::ReadWrite
                                })
                            },
                        )
                        .map_err(|e| {
                            let line_number = chunk.line_number(e.span.start());
                            CompileError {
                                kind: CompileErrorKind::IrGen(e),
                                chunk_name: chunk.name().clone(),
                                line_number,
                            }
                        })?;

                    verify_ir(&ir).expect("Internal IR generation error");
                    optimize_ir(&mut ir);
                    verify_ir(&ir).expect("Internal IR optimization error");
                    let proto =
                        gen_prototype(&ir, |n| magic.find(n)).expect("Internal Codegen Error");

                    let vm_proto = proto.into_vm(&ctx, chunk, magic);
                    let closure = vm::Closure::new(&ctx, vm_proto, vm::Value::Undefined).unwrap();

                    vm::MagicSet::replace(
                        magic_write,
                        magic_index,
                        MagicConstant::new_ptr(&ctx, closure.into()),
                    )
                    .unwrap();
                }
            }
        }

        let mut prototypes = Vec::new();

        for (chunk, block, compile_settings) in parsed_chunks {
            let mut ir = compile_settings
                .ir_gen
                .gen_ir(vm::FunctionRef::Chunk, &[], &block, |m| {
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

            verify_ir(&ir).expect("Internal IR generation error");
            optimize_ir(&mut ir);
            verify_ir(&ir).expect("Internal IR optimization error");
            let proto = gen_prototype(&ir, |n| magic.find(n)).expect("Internal Codegen Error");

            let vm_proto = proto.into_vm(&ctx, chunk, magic);

            prototypes.push(vm_proto);
        }

        let imports = ImportItems {
            macros: if resolved_macros.is_empty() {
                None
            } else {
                Some(Gc::new(&ctx, macros))
            },
            enums: if enums.is_empty() {
                None
            } else {
                Some(Gc::new(&ctx, enums))
            },
            magic,
        };

        Ok((prototypes, imports))
    }
}
