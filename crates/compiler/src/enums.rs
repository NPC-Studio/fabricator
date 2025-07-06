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
    #[error("declaration would shadow enum #{0}")]
    ShadowsEnum(usize),
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
        let enums = block
            .statements
            .extract_if(.., |s| matches!(*s.kind, ast::StatementKind::Enum(_)));

        for stmt in enums {
            let enum_stmt = match *stmt.kind {
                ast::StatementKind::Enum(enum_stmt) => enum_stmt,
                _ => unreachable!(),
            };

            let mut enom = Enum {
                name: enum_stmt.name,
                span: stmt.span,
                variants: HashMap::new(),
            };

            let mut implicit_index = 0;
            for (name, value) in enum_stmt.variants {
                if let Some(expr) = value {
                    let span = expr.span;
                    let value = expr.fold_constant().ok_or_else(|| EnumError {
                        kind: EnumErrorKind::ValueNotConstant,
                        span,
                    })?;
                    enom.variants.insert(name, value);
                } else {
                    enom.variants
                        .insert(name, Constant::Integer(implicit_index));
                    implicit_index += 1;
                };
            }

            self.dict.insert(enom.name.clone(), self.enums.len());
            self.enums.push(enom);
        }

        Ok(())
    }

    /// Expand all enum values referenced anywhere in the given block.
    pub fn expand(&mut self, block: &mut ast::Block<S>) -> Result<(), EnumEvaluationError> {
        struct EnumExpander<'a, S>(&'a EnumSet<S>);

        impl<'a, S: Clone + Eq + Hash> ast::VisitorMut<S> for EnumExpander<'a, S> {
            type Break = EnumEvaluationError;

            fn visit_stmt_mut(&mut self, stmt: &mut ast::Statement<S>) -> ControlFlow<Self::Break> {
                let shadows = match stmt.kind.as_ref() {
                    ast::StatementKind::Enum(enum_stmt) => self.0.dict.get(&enum_stmt.name),
                    ast::StatementKind::Function(func_stmt) => self.0.dict.get(&func_stmt.name),
                    ast::StatementKind::Var(var_stmt) => self.0.dict.get(&var_stmt.name),
                    _ => None,
                }
                .copied();

                if let Some(index) = shadows {
                    ControlFlow::Break(EnumEvaluationError {
                        kind: EnumEvaluationErrorKind::ShadowsEnum(index),
                        span: stmt.span,
                    })
                } else {
                    stmt.walk_mut(self)
                }
            }

            fn visit_expr_mut(
                &mut self,
                expr: &mut ast::Expression<S>,
            ) -> ControlFlow<Self::Break> {
                match &mut *expr.kind {
                    ast::ExpressionKind::Name(ident) => {
                        if let Some(&index) = self.0.dict.get(ident) {
                            return ControlFlow::Break(EnumEvaluationError {
                                kind: EnumEvaluationErrorKind::BadVariant(index),
                                span: expr.span,
                            });
                        }
                    }
                    ast::ExpressionKind::Field(field_expr) => {
                        if let ast::ExpressionKind::Name(ident) = &*field_expr.base.kind {
                            if let Some(&index) = self.0.dict.get(ident) {
                                if let Some(var) =
                                    self.0.enums[index].variants.get(&field_expr.field)
                                {
                                    *expr.kind = ast::ExpressionKind::Constant(var.clone());
                                } else {
                                    return ControlFlow::Break(EnumEvaluationError {
                                        kind: EnumEvaluationErrorKind::BadVariant(index),
                                        span: expr.span,
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
