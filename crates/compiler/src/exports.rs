use std::{borrow::Borrow, collections::HashMap, hash::Hash};

use fabricator_vm::Span;
use thiserror::Error;

use crate::ast;

#[derive(Debug, Error)]
#[error("export name is a duplicate of item #{index}")]
pub struct DuplicateExportError {
    pub index: usize,
    pub span: Span,
}

#[derive(Debug, Copy, Clone)]
pub struct ExportSettings {
    /// If `true`, then all top-level functions are interpreted as magic exports and NOT normal
    /// function statements. Such functions must be completely independent from their surrounding
    /// script and are compiled as their own chunk.
    pub export_top_level_functions: bool,
}

impl Default for ExportSettings {
    fn default() -> Self {
        Self {
            export_top_level_functions: true,
        }
    }
}

pub enum Export<S> {
    Function(ast::FunctionStmt<S>),
    GlobalVar(ast::Ident<S>),
}

impl<S> Export<S> {
    pub fn name(&self) -> &S {
        match self {
            Export::Function(function_stmt) => &function_stmt.name,
            Export::GlobalVar(ident) => ident,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Export::Function(function_stmt) => function_stmt.span,
            Export::GlobalVar(ident) => ident.span,
        }
    }
}

/// Extract and collect exported items from multiple sources.
///
/// "Exported items" are a way for source code to declare its own special free variables available
/// anywhere in the source.
pub struct ExportSet<S> {
    exports: Vec<Export<S>>,
    dict: HashMap<S, usize>,
}

impl<S> Default for ExportSet<S> {
    fn default() -> Self {
        Self {
            exports: Vec::new(),
            dict: HashMap::new(),
        }
    }
}

impl<S> ExportSet<S> {
    pub fn new() -> Self {
        Self::default()
    }

    /// The current count of extracted exports.
    ///
    /// Each export is assigned a sequential index for identification starting from zero. Checking
    /// the current export count can be used to determine which exports are extracted from which
    /// calls to [`ExportSet::extract`].
    pub fn len(&self) -> usize {
        self.exports.len()
    }

    pub fn is_empty(&self) -> bool {
        self.exports.is_empty()
    }

    /// Get an extracted enum.
    pub fn get(&self, index: usize) -> Option<&Export<S>> {
        self.exports.get(index)
    }

    pub fn iter(&self) -> impl Iterator<Item = &Export<S>> {
        self.exports.iter()
    }
}

impl<S: Eq + Hash> ExportSet<S> {
    /// Find an export index by its name.
    pub fn find<Q>(&self, name: &Q) -> Option<usize>
    where
        S: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.dict.get(name).copied()
    }
}

impl<S: Clone + Eq + Hash> ExportSet<S> {
    /// Extract all top-level exports from a block.
    pub fn extract(
        &mut self,
        block: &mut ast::Block<S>,
        settings: ExportSettings,
    ) -> Result<(), DuplicateExportError> {
        let exports = block.statements.extract_if(.., |s| match s {
            ast::Statement::Function(_) if settings.export_top_level_functions => true,
            ast::Statement::GlobalVar(_) => true,
            _ => false,
        });

        for stmt in exports {
            let export = match stmt {
                ast::Statement::Function(func_stmt) => Export::Function(func_stmt),
                ast::Statement::GlobalVar(ident) => Export::GlobalVar(ident),
                _ => unreachable!(),
            };

            if let Some(&index) = self.dict.get(export.name()) {
                return Err(DuplicateExportError {
                    index,
                    span: export.span(),
                });
            }

            self.dict.insert(export.name().clone(), self.exports.len());
            self.exports.push(export);
        }

        Ok(())
    }
}
