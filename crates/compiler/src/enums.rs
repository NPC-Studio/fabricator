use std::{borrow::Borrow, collections::HashMap, hash::Hash, ops::ControlFlow};

use fabricator_vm::Span;
use gc_arena::Collect;
use thiserror::Error;

use crate::{constant::Constant, parser};

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
#[error("reference to enum #{index} with an invalid variant")]
pub struct BadEnumVariant {
    pub index: usize,
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
    pub fn extract(&mut self, block: &mut parser::Block<S>) -> Result<(), EnumError> {
        let enums = block
            .statements
            .extract_if(.., |s| matches!(*s.kind, parser::StatementKind::Enum(_)))
            .map(|s| match *s.kind {
                parser::StatementKind::Enum(es) => es,
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
    pub fn expand_block(&mut self, block: &mut parser::Block<S>) -> Result<(), BadEnumVariant> {
        if let ControlFlow::Break(err) = block.for_each_expr(|expr| {
            match &mut *expr.kind {
                parser::ExpressionKind::Name(ident) => {
                    if let Some(&index) = self.enum_dict.get(ident) {
                        return ControlFlow::Break(BadEnumVariant {
                            index,
                            span: expr.span,
                        });
                    }
                }
                parser::ExpressionKind::Field(field_expr) => {
                    if let parser::ExpressionKind::Name(ident) = &*field_expr.base.kind {
                        if let Some(&index) = self.enum_dict.get(ident) {
                            if let Some(var) = self.enums[index].variants.get(&field_expr.field) {
                                *expr.kind = parser::ExpressionKind::Constant(var.clone());
                            } else {
                                return ControlFlow::Break(BadEnumVariant {
                                    index,
                                    span: expr.span,
                                });
                            }
                        }
                    }
                }
                _ => {}
            }
            ControlFlow::Continue(())
        }) {
            Err(err)
        } else {
            Ok(())
        }
    }
}

fn fold_constant_expr<S>(expr: parser::Expression<S>) -> Option<Constant<S>> {
    match *expr.kind {
        parser::ExpressionKind::Constant(c) => Some(c),
        parser::ExpressionKind::Group(expr) => fold_constant_expr(expr),
        parser::ExpressionKind::Unary(unary_op, expr) => match unary_op {
            parser::UnaryOp::Not => Some(Constant::Boolean(!fold_constant_expr(expr)?.to_bool())),
            parser::UnaryOp::Minus => fold_constant_expr(expr)?.negate(),
        },
        parser::ExpressionKind::Binary(l, op, r) => {
            let l = fold_constant_expr(l)?;
            let r = fold_constant_expr(r)?;

            match op {
                parser::BinaryOp::Add => l.add(r),
                parser::BinaryOp::Sub => l.sub(r),
                parser::BinaryOp::Mult => l.mult(r),
                parser::BinaryOp::Div => l.div(r),
                parser::BinaryOp::Equal => l.equal(r).map(Constant::Boolean),
                parser::BinaryOp::NotEqual => l.equal(r).map(|b| Constant::Boolean(!b)),
                parser::BinaryOp::LessThan => l.less_than(r).map(Constant::Boolean),
                parser::BinaryOp::LessEqual => l.less_equal(r).map(Constant::Boolean),
                parser::BinaryOp::GreaterThan => r.less_than(l).map(Constant::Boolean),
                parser::BinaryOp::GreaterEqual => r.less_equal(l).map(Constant::Boolean),
                parser::BinaryOp::And => Some(Constant::Boolean(l.to_bool() && r.to_bool())),
                parser::BinaryOp::Or => Some(Constant::Boolean(l.to_bool() || r.to_bool())),
            }
        }
        _ => None,
    }
}
