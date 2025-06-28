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

pub enum ExportKind<S> {
    Function(ast::FunctionStatement<S>),
}

pub struct Export<S> {
    pub name: S,
    pub span: Span,
    pub kind: ExportKind<S>,
}

/// Extract and collect exported items from multiple sources.
///
/// "Exported items" are a way for source code to declare its own magic variables available anywhere
/// in the source.
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
    pub fn find<Q: ?Sized>(&self, name: &Q) -> Option<usize>
    where
        S: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.dict.get(name).copied()
    }
}

impl<S: Clone + Eq + Hash> ExportSet<S> {
    /// Extract all top-level exports from a block.
    pub fn extract(&mut self, block: &mut ast::Block<S>) -> Result<(), DuplicateExportError> {
        let exports = block
            .statements
            .extract_if(.., |s| matches!(*s.kind, ast::StatementKind::Function(_)));

        for stmt in exports {
            let export = match *stmt.kind {
                ast::StatementKind::Function(func_stmt) => Export {
                    name: func_stmt.name.clone(),
                    span: stmt.span,
                    kind: ExportKind::Function(func_stmt),
                },
                _ => unreachable!(),
            };

            self.dict.insert(export.name.clone(), self.exports.len());
            self.exports.push(export);
        }

        Ok(())
    }
}
