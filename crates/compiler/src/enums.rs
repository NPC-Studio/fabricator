use std::{borrow::Borrow, collections::HashMap, hash::Hash, ops::ControlFlow};

use fabricator_vm::Span;
use gc_arena::Collect;
use thiserror::Error;

use crate::{ast, constant::Constant};

#[derive(Debug, Error)]
pub enum EnumErrorKind {
    #[error("enum name is a duplicate of enum #{0}")]
    Duplicate(usize),
    #[error("enum variant value is not a constant expression")]
    ValueNotConstant,
}

#[derive(Debug, Error)]
#[error("{kind}")]
pub struct EnumError {
    pub kind: EnumErrorKind,
    pub span: Span,
}

#[derive(Debug, Error)]
pub enum EnumEvaluationErrorKind {
    #[error("reference to enum #{0} with an invalid variant")]
    BadVariant(usize),
    #[error("reference to enum {0} with no variant")]
    NoVariant(usize),
}

#[derive(Debug, Error)]
#[error("{kind}")]
pub struct EnumEvaluationError {
    pub kind: EnumEvaluationErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub struct Enum<S> {
    pub name: S,
    pub span: Span,
    pub variants: HashMap<S, Constant<S>>,
}

/// Gather enum definitions from multiple sources and then expand them all anywhere they occur in
/// an AST.
#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub struct EnumSet<S> {
    enums: Vec<Enum<S>>,
    dict: HashMap<S, usize>,
}

impl<S> Default for EnumSet<S> {
    fn default() -> Self {
        Self {
            enums: Vec::new(),
            dict: HashMap::new(),
        }
    }
}

impl<S> EnumSet<S> {
    pub fn new() -> Self {
        Self::default()
    }

    /// The current count of extracted enums.
    ///
    /// Each enum is assigned a sequential index for identification starting from zero. Checking the
    /// current enum count can be used to determine which enums are extracted from which calls to
    /// [`EnumSet::extract`].
    pub fn len(&self) -> usize {
        self.enums.len()
    }

    pub fn is_empty(&self) -> bool {
        self.enums.is_empty()
    }

    /// Get an extracted enum.
    pub fn get(&self, index: usize) -> Option<&Enum<S>> {
        self.enums.get(index)
    }

    pub fn iter(&self) -> impl Iterator<Item = &Enum<S>> {
        self.enums.iter()
    }
}

impl<S: Eq + Hash> EnumSet<S> {
    /// Find a enum index by its name.
    pub fn find<Q: ?Sized>(&self, name: &Q) -> Option<usize>
    where
        S: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.dict.get(name).copied()
    }
}

impl<S: Clone + Eq + Hash> EnumSet<S> {
    /// Extract all top-level enum statements from a block.
    pub fn extract(&mut self, block: &mut ast::Block<S>) -> Result<(), EnumError> {
        for stmt in &block.statements {
            if let ast::Statement::Enum(enum_stmt) = stmt {
                if let Some(&index) = self.dict.get(&enum_stmt.name.inner) {
                    return Err(EnumError {
                        kind: EnumErrorKind::Duplicate(index),
                        span: enum_stmt.span,
                    });
                }
            }
        }

        let enums = block
            .statements
            .extract_if(.., |s| matches!(s, ast::Statement::Enum(_)));

        for stmt in enums {
            let enum_stmt = match stmt {
                ast::Statement::Enum(enum_stmt) => enum_stmt,
                _ => unreachable!(),
            };

            let mut enum_ = Enum {
                name: enum_stmt.name.inner,
                span: enum_stmt.span,
                variants: HashMap::new(),
            };

            let mut implicit_index = 0;
            for (name, value) in enum_stmt.variants {
                if let Some(expr) = value {
                    let span = expr.span();
                    let value = expr.fold_constant().ok_or(EnumError {
                        kind: EnumErrorKind::ValueNotConstant,
                        span,
                    })?;
                    enum_.variants.insert(name.inner, value);
                } else {
                    enum_
                        .variants
                        .insert(name.inner, Constant::Integer(implicit_index));
                    implicit_index += 1;
                };
            }

            assert!(
                self.dict
                    .insert(enum_.name.clone(), self.enums.len())
                    .is_none()
            );
            self.enums.push(enum_);
        }

        Ok(())
    }

    /// Expand all enum values referenced anywhere in the given block.
    pub fn expand(&mut self, block: &mut ast::Block<S>) -> Result<(), EnumEvaluationError> {
        struct EnumExpander<'a, S>(&'a EnumSet<S>);

        impl<'a, S: Clone + Eq + Hash> ast::VisitorMut<S> for EnumExpander<'a, S> {
            type Break = EnumEvaluationError;

            fn visit_expr_mut(
                &mut self,
                expr: &mut ast::Expression<S>,
            ) -> ControlFlow<Self::Break> {
                match expr {
                    ast::Expression::Ident(ident) => {
                        if let Some(&index) = self.0.dict.get(ident) {
                            return ControlFlow::Break(EnumEvaluationError {
                                kind: EnumEvaluationErrorKind::NoVariant(index),
                                span: expr.span(),
                            });
                        }
                    }
                    ast::Expression::Field(field_expr) => {
                        if let ast::Expression::Ident(ident) = &*field_expr.base {
                            if let Some(&index) = self.0.dict.get(ident) {
                                if let Some(var) =
                                    self.0.enums[index].variants.get(&field_expr.field)
                                {
                                    *expr = ast::Expression::Constant(var.clone(), field_expr.span);
                                } else {
                                    return ControlFlow::Break(EnumEvaluationError {
                                        kind: EnumEvaluationErrorKind::BadVariant(index),
                                        span: expr.span(),
                                    });
                                }
                            }
                        }
                    }
                    _ => {}
                }

                expr.walk_mut(self)
            }
        }

        if let ControlFlow::Break(err) = block.walk_mut(&mut EnumExpander(self)) {
            Err(err)
        } else {
            Ok(())
        }
    }
}
