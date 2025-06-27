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
    pub variants: HashMap<S, Constant<S>>,
}

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub struct EnumSet<S> {
    enums: Vec<Enum<S>>,
    enum_dict: HashMap<S, usize>,
}

impl<S> Default for EnumSet<S> {
    fn default() -> Self {
        Self {
            enums: Vec::new(),
            enum_dict: HashMap::new(),
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
    /// [`EnumSet::extract_enums`].
    pub fn len(&self) -> usize {
        self.enums.len()
    }

    /// Get an extracted enum.
    pub fn get(&self, index: usize) -> Option<&Enum<S>> {
        self.enums.get(index)
    }
}

impl<S: Eq + Hash> EnumSet<S> {
    /// Find a enum index by enum name.
    pub fn find<Q: ?Sized>(&self, name: &Q) -> Option<usize>
    where
        S: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.enum_dict.get(name).copied()
    }
}

impl<S: Clone + Eq + Hash> EnumSet<S> {
    /// Extract all top-level enum statements from a block.
    pub fn extract(&mut self, block: &mut ast::Block<S>) -> Result<(), EnumError> {
        let enums = block
            .statements
            .extract_if(.., |s| matches!(*s.kind, ast::StatementKind::Enum(_)))
            .map(|s| match *s.kind {
                ast::StatementKind::Enum(es) => es,
                _ => unreachable!(),
            });

        for stmt in enums {
            let mut enom = Enum {
                name: stmt.name,
                variants: HashMap::new(),
            };

            let mut implicit_index = 0;
            for (name, value) in stmt.variants {
                if let Some(expr) = value {
                    let span = expr.span;
                    let value = fold_constant_expr(expr).ok_or_else(|| EnumError {
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

            self.enum_dict.insert(enom.name.clone(), self.enums.len());
            self.enums.push(enom);
        }

        Ok(())
    }

    /// Expand all enum values referenced anywhere in the given block.
    pub fn expand_block(&mut self, block: &mut ast::Block<S>) -> Result<(), EnumEvaluationError> {
        struct EnumExpander<'a, S>(&'a EnumSet<S>);

        impl<'a, S: Clone + Eq + Hash> ast::VisitorMut<S> for EnumExpander<'a, S> {
            type Break = EnumEvaluationError;

            fn visit_stmt_mut(&mut self, stmt: &mut ast::Statement<S>) -> ControlFlow<Self::Break> {
                let shadows = match stmt.kind.as_ref() {
                    ast::StatementKind::Enum(enum_stmt) => self.0.enum_dict.get(&enum_stmt.name),
                    ast::StatementKind::Function(func_stmt) => {
                        self.0.enum_dict.get(&func_stmt.name)
                    }
                    ast::StatementKind::Var(var_stmt) => self.0.enum_dict.get(&var_stmt.name),
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
                        if let Some(&index) = self.0.enum_dict.get(ident) {
                            return ControlFlow::Break(EnumEvaluationError {
                                kind: EnumEvaluationErrorKind::BadVariant(index),
                                span: expr.span,
                            });
                        }
                    }
                    ast::ExpressionKind::Field(field_expr) => {
                        if let ast::ExpressionKind::Name(ident) = &*field_expr.base.kind {
                            if let Some(&index) = self.0.enum_dict.get(ident) {
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

fn fold_constant_expr<S>(expr: ast::Expression<S>) -> Option<Constant<S>> {
    match *expr.kind {
        ast::ExpressionKind::Constant(c) => Some(c),
        ast::ExpressionKind::Group(expr) => fold_constant_expr(expr),
        ast::ExpressionKind::Unary(unary_op, expr) => match unary_op {
            ast::UnaryOp::Not => Some(Constant::Boolean(!fold_constant_expr(expr)?.to_bool())),
            ast::UnaryOp::Minus => fold_constant_expr(expr)?.negate(),
        },
        ast::ExpressionKind::Binary(l, op, r) => {
            let l = fold_constant_expr(l)?;
            let r = fold_constant_expr(r)?;

            match op {
                ast::BinaryOp::Add => l.add(r),
                ast::BinaryOp::Sub => l.sub(r),
                ast::BinaryOp::Mult => l.mult(r),
                ast::BinaryOp::Div => l.div(r),
                ast::BinaryOp::Equal => l.equal(r).map(Constant::Boolean),
                ast::BinaryOp::NotEqual => l.equal(r).map(|b| Constant::Boolean(!b)),
                ast::BinaryOp::LessThan => l.less_than(r).map(Constant::Boolean),
                ast::BinaryOp::LessEqual => l.less_equal(r).map(Constant::Boolean),
                ast::BinaryOp::GreaterThan => r.less_than(l).map(Constant::Boolean),
                ast::BinaryOp::GreaterEqual => r.less_equal(l).map(Constant::Boolean),
                ast::BinaryOp::And => Some(Constant::Boolean(l.to_bool() && r.to_bool())),
                ast::BinaryOp::Or => Some(Constant::Boolean(l.to_bool() || r.to_bool())),
            }
        }
        _ => None,
    }
}
