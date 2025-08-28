use std::{collections::HashSet, path::Path};

use fabricator_util::index_containers::IndexMap;
use fabricator_vm as vm;
use gc_arena::{Collect, DynamicRoot, DynamicRootSet, Gc, Mutation, Rootable};
use thiserror::Error;

use crate::{
    analysis::{
        block_simplification::{block_branch_to_jump, merge_blocks, redirect_empty_blocks},
        cleanup::{
            clean_instructions, clean_unreachable_blocks, clean_unused_call_scopes,
            clean_unused_functions, clean_unused_shadow_vars, clean_unused_this_scopes,
            clean_unused_variables,
        },
        constant_folding::fold_constants,
        dead_code_elim::eliminate_dead_code,
        eliminate_copies::eliminate_copies,
        instruction_liveness::{InstructionLiveness, InstructionVerificationError},
        nested_scope_liveness::{ThisScopeLiveness, ThisScopeVerificationError},
        shadow_liveness::{ShadowLiveness, ShadowVerificationError},
        shadow_reduction::reduce_shadows,
        ssa_conversion::convert_to_ssa,
        variable_liveness::{VariableLiveness, VariableVerificationError},
        verify_arguments::{ArgumentVerificationError, verify_arguments},
        verify_references::{ReferenceVerificationError, verify_references},
    },
    code_gen::{Prototype, gen_prototype},
    enums::{EnumError, EnumEvaluationError, EnumResolutionError, EnumSet, EnumSetBuilder},
    exports::{DuplicateExportError, Export, ExportSet},
    ir,
    ir_gen::{FreeVarMode, IrGenError, IrGenSettings, VarDict},
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
    EnumResolution(#[source] EnumResolutionError),
    #[error("enum error: {0}")]
    EnumEvaluation(#[source] EnumEvaluationError),
    #[error("duplicate export error: {0}")]
    DuplicateExport(#[source] DuplicateExportError),
    #[error("enum or export `{name}` shares a name with a conflicting enum or export")]
    ShadowsSpecial { name: String, span: vm::Span },
    #[error("IR gen error: {0}")]
    IrGen(#[source] IrGenError),
}

#[derive(Debug, Error)]
#[error("{kind} at {chunk_name}:{line_number}")]
pub struct CompileError {
    #[source]
    pub kind: CompileErrorKind,
    pub chunk_name: vm::SharedStr,
    pub line_number: vm::LineNumber,
}

#[derive(Debug, Copy, Clone)]
pub struct CompileSettings {
    pub parse: ParseSettings,
    pub ir_gen: IrGenSettings,
    pub optimization_passes: u8,
}

impl CompileSettings {
    pub fn compat() -> Self {
        Self {
            parse: ParseSettings::compat(),
            ir_gen: IrGenSettings::compat(),
            optimization_passes: 2,
        }
    }

    pub fn modern() -> Self {
        Self {
            parse: ParseSettings::strict(),
            ir_gen: IrGenSettings::modern(),
            optimization_passes: 2,
        }
    }

    /// If the given path has a (case-insensitive) `.gml` extension, then compile in compat mode,
    /// otherwise modern.
    pub fn from_path(path: &Path) -> Self {
        if path
            .extension()
            .is_some_and(|e| e.eq_ignore_ascii_case("gml"))
        {
            Self::compat()
        } else {
            Self::modern()
        }
    }

    pub fn set_optimization_passes(mut self, passes: u8) -> Self {
        self.optimization_passes = passes;
        self
    }
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

#[derive(Debug, Error)]
pub enum IrVerificationError {
    #[error(transparent)]
    ReferenceVerification(#[from] ReferenceVerificationError),
    #[error(transparent)]
    ArgumentVerification(#[from] ArgumentVerificationError),
    #[error(transparent)]
    InstructionVerification(#[from] InstructionVerificationError),
    #[error(transparent)]
    ShadowVerification(#[from] ShadowVerificationError),
    #[error(transparent)]
    VariableVerification(#[from] VariableVerificationError),
    #[error(transparent)]
    ThisScopeVerification(#[from] ThisScopeVerificationError),
}

/// Verify that IR is well-formed.
pub fn verify_ir<S: Clone>(ir: &ir::Function<S>) -> Result<(), IrVerificationError> {
    verify_references(ir)?;
    verify_arguments(ir)?;
    InstructionLiveness::compute(ir)?;
    ShadowLiveness::compute(ir)?;
    VariableLiveness::compute(ir)?;
    ThisScopeLiveness::compute(ir)?;

    for func in ir.functions.values() {
        verify_ir(func)?;
    }
    Ok(())
}

/// Run optimization passes on IR.
///
/// # Panics
///
/// May panic if the provided IR is not well-formed.
pub fn optimize_ir<S: Eq + Clone>(ir: &mut ir::Function<S>) {
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

    block_branch_to_jump(ir);
    redirect_empty_blocks(ir);
    clean_unreachable_blocks(ir);
    merge_blocks(ir);

    clean_unreachable_blocks(ir);
    clean_instructions(ir);
    clean_unused_variables(ir);
    clean_unused_shadow_vars(ir);
    clean_unused_this_scopes(ir);
    clean_unused_call_scopes(ir);
    clean_unused_functions(ir);
}

/// Items shared across compilation units.
///
/// These will be accumulated by the compiler during a compilation unit and are part of the compiler
/// output.
///
/// These can be shared across different instances of a [`Compiler`] to control sharing between
/// different logical sets of FML scripts (compilation units).
#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct ImportItems<'gc> {
    macros: Option<Gc<'gc, MacroSet<vm::String<'gc>>>>,
    enums: Option<Gc<'gc, EnumSet<vm::String<'gc>>>>,
    global_vars: Option<Gc<'gc, HashSet<vm::String<'gc>>>>,
    magic: Gc<'gc, vm::MagicSet<'gc>>,
}

impl<'gc> ImportItems<'gc> {
    pub fn from_magic(magic: Gc<'gc, vm::MagicSet<'gc>>) -> Self {
        Self {
            macros: None,
            enums: None,
            global_vars: None,
            magic,
        }
    }
}

#[derive(Clone)]
pub struct StashedImportItems {
    macros: Option<DynamicRoot<Rootable![MacroSet<vm::String<'_>>]>>,
    enums: Option<DynamicRoot<Rootable![EnumSet<vm::String<'_>>]>>,
    global_vars: Option<DynamicRoot<Rootable![HashSet<vm::String<'_>>]>>,
    magic: vm::StashedMagicSet,
}

impl<'gc> vm::Stashable<'gc> for ImportItems<'gc> {
    type Stashed = StashedImportItems;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed {
        StashedImportItems {
            macros: self
                .macros
                .map(|macros| roots.stash::<Rootable![MacroSet<vm::String<'_>>]>(mc, macros)),
            enums: self
                .enums
                .map(|enums| roots.stash::<Rootable![EnumSet<vm::String<'_>>]>(mc, enums)),
            global_vars: self
                .global_vars
                .map(|globals| roots.stash::<Rootable![HashSet<vm::String<'_>>]>(mc, globals)),
            magic: vm::Stashable::stash(self.magic, mc, roots),
        }
    }
}

impl vm::Fetchable for StashedImportItems {
    type Fetched<'gc> = ImportItems<'gc>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> ImportItems<'gc> {
        ImportItems {
            macros: self.macros.as_ref().map(|macros| roots.fetch(&macros)),
            enums: self.enums.as_ref().map(|enums| roots.fetch(&enums)),
            global_vars: self
                .global_vars
                .as_ref()
                .map(|globals| roots.fetch(&globals)),
            magic: self.magic.fetch(roots),
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
    enums: EnumSetBuilder<vm::String<'gc>>,
    global_vars: HashSet<vm::String<'gc>>,
    magic: vm::MagicSet<'gc>,
    chunks: Vec<Chunk<'gc>>,
}

pub type DebugOutput<'gc> = Vec<(Gc<'gc, vm::Prototype<'gc>>, ir::Function<vm::String<'gc>>)>;

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
    ) -> Result<
        (
            Gc<'gc, vm::Prototype<'gc>>,
            ImportItems<'gc>,
            DebugOutput<'gc>,
        ),
        CompileError,
    > {
        let mut this = Self::new(ctx, config, imports);
        this.add_chunk(settings, chunk_name, code)?;
        let (mut outputs, imports, debug_output) = this.compile()?;
        Ok((outputs.pop().unwrap(), imports, debug_output))
    }

    pub fn new(
        ctx: vm::Context<'gc>,
        config: impl Into<String>,
        imports: ImportItems<'gc>,
    ) -> Self {
        let macros = imports.macros.as_deref().cloned().unwrap_or_default();
        let enums = imports.enums.as_deref().cloned().unwrap_or_default();
        let global_vars = imports.global_vars.as_deref().cloned().unwrap_or_default();
        let magic = imports.magic.as_ref().clone();

        Self {
            ctx,
            config: config.into(),
            macros,
            enums: enums.into_builder(),
            global_vars,
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
        let chunk_name = vm::SharedStr::new(chunk_name);
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
    ) -> Result<
        (
            Vec<Gc<'gc, vm::Prototype<'gc>>>,
            ImportItems<'gc>,
            DebugOutput<'gc>,
        ),
        CompileError,
    > {
        let Self {
            ctx,
            config,
            mut macros,
            mut enums,
            mut global_vars,
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

        let resolved_macros =
            match macros
                .clone()
                .resolve_with_skip_recursive(&ctx.intern(&config), |&token| {
                    // GMS2 doesn't expand macro tokens that match a defined macro if the token name
                    // is also a part of the stdlib. This allows re-defining builtins with macros
                    // (at the cost of making the macro system even more complicated and special
                    // case).
                    //
                    // We do something similar here, except we skip expansion for *all* exports
                    // defined in a previous compilation unit.
                    magic.find(token).is_none() && !global_vars.contains(&token)
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
                    let chunk = &chunks[chunk_index];
                    let line_number = chunk.line_numbers.line(macro_.span.start());
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
        // conflict with any pre-existing magic variable name.

        // List of starting enum indexes per chunk, to identify which enums come from which chunk.
        let mut enum_chunk_indexes = Vec::new();

        for &mut (chunk, ref mut block, _) in &mut parsed_chunks {
            let prev_enum_len = enums.len();
            enum_chunk_indexes.push(prev_enum_len);

            if let Err(err) = enums.extract(block) {
                let line_number = chunk.line_number(err.span.start());
                return Err(CompileError {
                    kind: CompileErrorKind::Enum(err),
                    chunk_name: chunk.name().clone(),
                    line_number,
                });
            }

            for i in prev_enum_len..enums.len() {
                let enum_ = enums.get(i).unwrap();
                // Enums are not allowed to shadow names of existing global variables.
                if global_vars.contains(&enum_.name.inner) {
                    let line_number = chunk.line_number(enum_.span.start());
                    return Err(CompileError {
                        kind: CompileErrorKind::ShadowsSpecial {
                            name: enum_.name.as_str().to_owned(),
                            span: enum_.span,
                        },
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
            let chunk = &parsed_chunks[chunk_index].0;
            let line_number = chunk.line_number(enum_.span.start());
            CompileError {
                kind: CompileErrorKind::EnumResolution(err),
                chunk_name: chunk.name().clone(),
                line_number,
            }
        })?;

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
        // pre-existing magic variable or enum.

        let mut exports = ExportSet::new();

        // List of starting export indexes per chunk, to identify which exports come from which
        // chunk.
        let mut export_chunk_indexes = Vec::new();

        // The magic index for each exported item, if one exists.
        let mut export_magic_indexes = IndexMap::new();

        let stub_magic = vm::MagicConstant::new_ptr(&ctx, vm::Value::Undefined);

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
                let export_name = export.name();
                let export_span = export.span();

                // New exports are not allowed to shadow names for existing enums or global
                // variables.
                if enums.find(export_name).is_some() || global_vars.contains(export_name) {
                    let line_number = chunk.line_number(export_span.start());
                    return Err(CompileError {
                        kind: CompileErrorKind::ShadowsSpecial {
                            name: export_name.as_str().to_owned(),
                            span: export_span,
                        },
                        chunk_name: chunk.name().clone(),
                        line_number,
                    });
                }

                match export {
                    Export::Function(_) => {
                        let index = magic.insert(export_name.clone(), stub_magic).0;
                        export_magic_indexes.insert(i, index);
                    }
                    Export::GlobalVar(ident) => {
                        global_vars.insert(ident.inner);
                    }
                }
            }
        }

        let magic = Gc::new(&ctx, magic);
        let magic_write = Gc::write(&ctx, magic);

        // Place every compiled function into a list, for dumping bytecode.
        let mut debug_output = DebugOutput::new();

        // For each exported item, produce a real magic value for it.

        for (i, export) in exports.iter().enumerate() {
            let chunk_index = match export_chunk_indexes.binary_search_by(|j| j.cmp(&i)) {
                Ok(i) => i,
                Err(i) => i.checked_sub(1).unwrap(),
            };
            let (chunk, _, compile_settings) = parsed_chunks[chunk_index];

            match export {
                Export::Function(func_stmt) => {
                    let magic_index = export_magic_indexes[i];

                    let mut ir = compile_settings
                        .ir_gen
                        .gen_func_stmt_ir(
                            &mut VmInterner::new(ctx),
                            func_stmt,
                            &CompilerVarDict {
                                enums: &enums,
                                global_vars: &global_vars,
                                magic: &magic,
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

                    let proto = optimize_and_generate_proto(compile_settings, &mut ir, &magic);
                    let vm_proto = proto.into_vm(&ctx, chunk, magic);
                    let closure = vm::Closure::new(&ctx, vm_proto, vm::Value::Undefined).unwrap();

                    debug_output.push((vm_proto, ir));

                    vm::MagicSet::replace(
                        magic_write,
                        magic_index,
                        vm::MagicConstant::new_ptr(&ctx, closure),
                    )
                    .unwrap();
                }
                Export::GlobalVar(_) => {}
            }
        }

        let mut outputs = Vec::new();

        for (chunk, block, compile_settings) in parsed_chunks {
            let mut ir = compile_settings
                .ir_gen
                .gen_chunk_ir(
                    &mut VmInterner::new(self.ctx),
                    &block,
                    &CompilerVarDict {
                        enums: &enums,
                        global_vars: &global_vars,
                        magic: &magic,
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

            let proto = optimize_and_generate_proto(compile_settings, &mut ir, &magic);
            let vm_proto = proto.into_vm(&ctx, chunk, magic);
            debug_output.push((vm_proto, ir));
            outputs.push(vm_proto);
        }

        let imports = ImportItems {
            macros: if macros.is_empty() {
                None
            } else {
                Some(Gc::new(&ctx, macros))
            },
            enums: if enums.is_empty() {
                None
            } else {
                Some(Gc::new(&ctx, enums))
            },
            global_vars: if global_vars.is_empty() {
                None
            } else {
                Some(Gc::new(&ctx, global_vars))
            },
            magic,
        };

        Ok((outputs, imports, debug_output))
    }
}

struct Chunk<'gc> {
    name: vm::SharedStr,
    line_numbers: LineNumbers,
    tokens: Vec<Token<vm::String<'gc>>>,
    compile_settings: CompileSettings,
}

struct CompilerVarDict<'gc, 'a> {
    enums: &'a EnumSet<vm::String<'gc>>,
    global_vars: &'a HashSet<vm::String<'gc>>,
    magic: &'a vm::MagicSet<'gc>,
}

impl<'gc, 'a> VarDict<vm::String<'gc>> for CompilerVarDict<'gc, 'a> {
    fn permit_declaration(&self, name: &vm::String<'gc>) -> bool {
        !self.enums.find(name).is_some()
    }

    fn free_var_mode(&self, ident: &vm::String<'gc>) -> FreeVarMode {
        if let Some(index) = self.magic.find(*ident) {
            FreeVarMode::Magic {
                is_read_only: self.magic.get(index).unwrap().read_only(),
            }
        } else if self.global_vars.contains(ident) {
            FreeVarMode::GlobalVar
        } else {
            FreeVarMode::This
        }
    }
}

fn optimize_and_generate_proto<'gc>(
    compile_settings: CompileSettings,
    ir: &mut ir::Function<vm::String<'gc>>,
    magic: &vm::MagicSet<'gc>,
) -> Prototype<vm::String<'gc>> {
    if let Err(err) = verify_ir(ir) {
        panic!("Internal IR Generation Error: {err}\nIR: {ir:?}");
    }
    for _ in 0..compile_settings.optimization_passes {
        optimize_ir(ir);
    }
    if let Err(err) = verify_ir(ir) {
        panic!("Internal IR Optimization Error: {err}\nIR: {ir:?}");
    }
    match gen_prototype(&ir, |n| magic.find(*n)) {
        Ok(proto) => proto,
        Err(err) => {
            panic!("Internal Codegen Error: {err}\nIR: {ir:?}");
        }
    }
}
