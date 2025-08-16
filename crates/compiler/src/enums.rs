use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
    hash::Hash,
    ops::ControlFlow,
};

use fabricator_vm::Span;
use gc_arena::Collect;
use thiserror::Error;

use crate::{ast, constant::Constant};

#[derive(Debug, Error)]
pub enum EnumErrorKind {
    #[error("enum name is a duplicate of enum #{0}")]
    DuplicateEnum(usize),
    #[error("enum variant name is a duplicate of a previous one")]
    DuplicateVariantName,
}

#[derive(Debug, Error)]
#[error("{kind}")]
pub struct EnumError {
    pub kind: EnumErrorKind,
    pub span: Span,
}

#[derive(Debug, Error)]
pub enum EnumResolutionErrorKind {
    #[error("variant depends on itself recursively")]
    RecursiveEnumVariant,
    #[error("variant value is not a constant integer")]
    EnumVariantNotConstantInteger,
}

#[derive(Debug, Error)]
#[error("enum #{enum_index} variant #{variant_index} {kind}")]
pub struct EnumResolutionError {
    pub kind: EnumResolutionErrorKind,
    pub enum_index: usize,
    pub variant_index: usize,
}

#[derive(Debug, Error)]
pub enum EnumEvaluationErrorKind {
    #[error("reference to enum with an invalid variant")]
    BadVariant,
    #[error("reference to enum with no variant")]
    NoVariant,
}

#[derive(Debug, Error)]
#[error("{kind}")]
pub struct EnumEvaluationError {
    pub kind: EnumEvaluationErrorKind,
    pub enum_index: usize,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ResolvingEnumVariantKind<S> {
    Evaluated(i64),
    Implicit,
    Expression(ast::Expression<S>),
}

#[derive(Debug, Clone)]
pub struct ResolvingEnumVariant<S> {
    pub name: ast::Ident<S>,
    pub kind: ResolvingEnumVariantKind<S>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ResolvingEnum<S> {
    pub name: ast::Ident<S>,
    pub variants: Vec<ResolvingEnumVariant<S>>,
    pub dict: HashMap<S, usize>,
    pub span: Span,
}

/// Gather enum definitions from multiple sources and then later resolve them as a recursively
/// dependent set.
#[derive(Debug, Clone)]
pub struct EnumSetBuilder<S> {
    enums: Vec<ResolvingEnum<S>>,
    dict: HashMap<S, usize>,
}

impl<S> Default for EnumSetBuilder<S> {
    fn default() -> Self {
        Self {
            enums: Vec::new(),
            dict: HashMap::new(),
        }
    }
}

impl<S> EnumSetBuilder<S> {
    pub fn new() -> Self {
        Self::default()
    }

    /// The current count of extracted enums.
    ///
    /// Each enum is assigned a sequential index for identification starting from zero. Checking the
    /// current enum count can be used to determine which enums are extracted from which calls to
    /// [`EnumSetBuilder::extract`].
    pub fn len(&self) -> usize {
        self.enums.len()
    }

    pub fn is_empty(&self) -> bool {
        self.enums.is_empty()
    }

    /// Get an extracted enum.
    pub fn get(&self, index: usize) -> Option<&ResolvingEnum<S>> {
        self.enums.get(index)
    }

    pub fn iter(&self) -> impl Iterator<Item = &ResolvingEnum<S>> {
        self.enums.iter()
    }
}

impl<S: Eq + Hash> EnumSetBuilder<S> {
    /// Find a enum index by its name.
    pub fn find<Q: ?Sized>(&self, name: &Q) -> Option<usize>
    where
        S: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.dict.get(name).copied()
    }
}

impl<S: Clone + Eq + Hash> EnumSetBuilder<S> {
    /// Extract all top-level enum statements from a block.
    pub fn extract(&mut self, block: &mut ast::Block<S>) -> Result<(), EnumError> {
        for stmt in &block.statements {
            if let ast::Statement::Enum(enum_stmt) = stmt {
                if let Some(&index) = self.dict.get(&enum_stmt.name.inner) {
                    return Err(EnumError {
                        kind: EnumErrorKind::DuplicateEnum(index),
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

            let mut enum_ = ResolvingEnum {
                name: enum_stmt.name,
                variants: Vec::new(),
                dict: HashMap::new(),
                span: enum_stmt.span,
            };

            for (name, value) in enum_stmt.variants {
                if let Some(expr) = value {
                    let span = name.span.combine(expr.span());
                    enum_.dict.insert(name.inner.clone(), enum_.variants.len());
                    enum_.variants.push(ResolvingEnumVariant {
                        name,
                        kind: ResolvingEnumVariantKind::Expression(expr),
                        span,
                    });
                } else {
                    let span = name.span;
                    enum_.dict.insert(name.inner.clone(), enum_.variants.len());
                    enum_.variants.push(ResolvingEnumVariant {
                        name,
                        kind: ResolvingEnumVariantKind::Implicit,
                        span,
                    });
                };
            }

            assert!(
                self.dict
                    .insert(enum_.name.inner.clone(), self.enums.len())
                    .is_none()
            );
            self.enums.push(enum_);
        }

        Ok(())
    }

    /// Resolve all inter-variant dependencies and return an `EnumSet`.
    ///
    /// Enums and variants will retain the same indexes they had in the `EnumSetBuilder`.
    pub fn resolve(mut self) -> Result<EnumSet<S>, EnumResolutionError> {
        // Determine a proper enum variant evaluation order.
        //
        // Add all of the enum variants we need to evaluate to a stack.
        //
        // Take the top entry off of the stack and check to see if it has any un-evaluated
        // dependencies. If it does, then push the popped variant back onto the stack, followed
        // by all of its un-evaluated dependencies. Otherwise if the variant has no un-evaluated
        // dependencies, then it can be evaluated next.
        //
        // We also keep track of in-progress *evaluating* variants (un-evaluated variants that we
        // have encountered before which had an un-evaluated dependency). If we encounter one of
        // these variants more than once without it becoming evaluated in-between, then we know we
        // have a recursive enum variant.

        let mut eval_stack = Vec::new();
        for (enum_index, enum_) in self.enums.iter().enumerate() {
            for var_index in 0..enum_.variants.len() {
                eval_stack.push((enum_index, var_index));
            }
        }

        let mut evaluating_variants = HashSet::<(usize, usize)>::new();
        let mut evaluated_variants = HashSet::<(usize, usize)>::new();
        let mut eval_order = Vec::<(usize, usize)>::new();
        let mut dependencies = Vec::<(usize, usize)>::new();

        loop {
            let Some((enum_index, variant_index)) = eval_stack.pop() else {
                break;
            };

            if evaluated_variants.contains(&(enum_index, variant_index)) {
                // Variants can be in the evaluation stack more than once since we push every
                // unevaluated variant to the stack first thing to make sure that every one is
                // evaluated. If we encounter an evaluated variant again we can just skip it.
                continue;
            }

            let enum_ = &self.enums[enum_index];
            let variant = &enum_.variants[variant_index];

            dependencies.clear();
            match &variant.kind {
                ResolvingEnumVariantKind::Evaluated(_) => {}
                ResolvingEnumVariantKind::Implicit => {
                    // Implicit variants depend on the previous enum value, unless they are the
                    // first enum variant, in which case their value is always 0.
                    if variant_index > 0 {
                        dependencies.push((enum_index, variant_index - 1));
                    }
                }
                ResolvingEnumVariantKind::Expression(expression) => {
                    struct FindEnumVariants<'a, S> {
                        parent: &'a EnumSetBuilder<S>,
                        dependencies: &'a mut Vec<(usize, usize)>,
                    }

                    impl<'a, S> ast::Visitor<S> for FindEnumVariants<'a, S>
                    where
                        S: Eq + Hash,
                    {
                        type Break = ();

                        fn visit_expr(
                            &mut self,
                            expr: &ast::Expression<S>,
                        ) -> ControlFlow<Self::Break> {
                            match expr {
                                ast::Expression::Field(field_expr) => {
                                    if let ast::Expression::Ident(ident) = &*field_expr.base {
                                        if let Some(&ref_enum_index) =
                                            self.parent.dict.get(&ident.inner)
                                        {
                                            if let Some(&ref_variant_index) = self.parent.enums
                                                [ref_enum_index]
                                                .dict
                                                .get(&field_expr.field.inner)
                                            {
                                                self.dependencies
                                                    .push((ref_enum_index, ref_variant_index));
                                            }
                                        }
                                    }
                                }
                                _ => {}
                            }

                            expr.walk(self)
                        }
                    }

                    let _ = ast::Visitor::visit_expr(
                        &mut FindEnumVariants {
                            parent: &self,
                            dependencies: &mut dependencies,
                        },
                        expression,
                    );
                }
            }

            let mut has_unevaluated_dependency = false;
            for (dep_enum_index, dep_variant_index) in dependencies.drain(..) {
                if !evaluated_variants.contains(&(dep_enum_index, dep_variant_index)) {
                    if !has_unevaluated_dependency {
                        has_unevaluated_dependency = true;

                        // We need to evaluate dependencies before this variant, so mark the variant
                        // as in-progress. If we get here a *second* time without the variant
                        // becoming evaluated, then we must have a recursive dependency.
                        if !evaluating_variants.insert((enum_index, variant_index)) {
                            return Err(EnumResolutionError {
                                kind: EnumResolutionErrorKind::RecursiveEnumVariant,
                                enum_index,
                                variant_index,
                            });
                        }

                        // We must push the evaluating variant *before* all of its dependencies.
                        eval_stack.push((enum_index, variant_index));
                    }

                    eval_stack.push((dep_enum_index, dep_variant_index))
                }
            }

            if !has_unevaluated_dependency {
                evaluated_variants.insert((enum_index, variant_index));
                eval_order.push((enum_index, variant_index));
            }
        }

        // Once we have a known-good evaluation order, we can evaluate our interdependent variants
        // in this order and all references should be present.

        for (enum_index, variant_index) in eval_order {
            let get_evaluated = |enum_index: usize, var_index: usize| -> i64 {
                match self.enums[enum_index].variants[var_index].kind {
                    ResolvingEnumVariantKind::Evaluated(v) => v,
                    _ => panic!("enum #{enum_index} variant #{var_index} has not been evaluated"),
                }
            };

            match self.enums[enum_index].variants[variant_index].kind.clone() {
                ResolvingEnumVariantKind::Evaluated(_) => {}
                ResolvingEnumVariantKind::Implicit => {
                    let value = if variant_index == 0 {
                        // The first enum value, if not specified, is always 0.
                        0
                    } else {
                        get_evaluated(enum_index, variant_index - 1).wrapping_add(1)
                    };
                    self.enums[enum_index].variants[variant_index].kind =
                        ResolvingEnumVariantKind::Evaluated(value);
                }
                ResolvingEnumVariantKind::Expression(mut expression) => {
                    struct EnumExpander<'a, S>(&'a dyn Fn(&S, &S) -> Option<i64>);

                    impl<'a, S> ast::VisitorMut<S> for EnumExpander<'a, S> {
                        type Break = ();

                        fn visit_expr_mut(
                            &mut self,
                            expr: &mut ast::Expression<S>,
                        ) -> ControlFlow<Self::Break> {
                            match expr {
                                ast::Expression::Field(field_expr) => {
                                    if let ast::Expression::Ident(ident) = &*field_expr.base {
                                        if let Some(val) =
                                            (self.0)(&ident.inner, &field_expr.field.inner)
                                        {
                                            *expr = ast::Expression::Constant(
                                                Constant::Integer(val),
                                                field_expr.span,
                                            );
                                        }
                                    }
                                }
                                _ => {}
                            }

                            expr.walk_mut(self)
                        }
                    }

                    let _ = ast::VisitorMut::visit_expr_mut(
                        &mut EnumExpander(&|enum_name, variant_name| {
                            let &enum_index = self.dict.get(enum_name)?;
                            let &variant_index = self.enums[enum_index].dict.get(variant_name)?;
                            Some(get_evaluated(enum_index, variant_index))
                        }),
                        &mut expression,
                    );
                    let value = expression
                        .fold_constant()
                        .and_then(|c| c.to_integer())
                        .ok_or(EnumResolutionError {
                            kind: EnumResolutionErrorKind::EnumVariantNotConstantInteger,
                            enum_index,
                            variant_index,
                        })?;
                    self.enums[enum_index].variants[variant_index].kind =
                        ResolvingEnumVariantKind::Evaluated(value);
                }
            }
        }

        Ok(EnumSet::<S> {
            enums: self
                .enums
                .into_iter()
                .enumerate()
                .map(|(enum_index, enum_)| Enum {
                    name: enum_.name.inner,
                    name_span: enum_.name.span,
                    variants: enum_
                        .variants
                        .into_iter()
                        .enumerate()
                        .map(|(var_index, variant)| EnumVariant {
                            name: variant.name.inner,
                            name_span: variant.name.span,
                            value: match variant.kind {
                                ResolvingEnumVariantKind::Evaluated(val) => val,
                                _ => panic!(
                                    "enum #{enum_index} variant #{var_index} has not been evaluated"
                                ),
                            },
                            span: variant.span,
                        })
                        .collect(),
                    dict: enum_.dict,
                    span: enum_.span,
                })
                .collect(),
            dict: self.dict,
        })
    }
}

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub struct EnumVariant<S> {
    pub name: S,
    pub name_span: Span,
    pub value: i64,
    pub span: Span,
}

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub struct Enum<S> {
    pub name: S,
    pub name_span: Span,
    pub variants: Vec<EnumVariant<S>>,
    pub dict: HashMap<S, usize>,
    pub span: Span,
}

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
    /// The current count of extracted enums.
    ///
    /// Each enum is assigned a sequential index for identification starting from zero. Checking the
    /// current enum count can be used to determine which enums are extracted from which calls to
    /// [`EnumSetBuilder::extract`].
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
    /// Expand all enum values referenced anywhere in the given block.
    pub fn expand(&self, block: &mut ast::Block<S>) -> Result<(), EnumEvaluationError> {
        struct EnumExpander<'a, S>(&'a EnumSet<S>);

        impl<'a, S: Clone + Eq + Hash> ast::VisitorMut<S> for EnumExpander<'a, S> {
            type Break = EnumEvaluationError;

            fn visit_expr_mut(
                &mut self,
                expr: &mut ast::Expression<S>,
            ) -> ControlFlow<Self::Break> {
                match expr {
                    ast::Expression::Ident(ident) => {
                        if let Some(&enum_index) = self.0.dict.get(ident) {
                            return ControlFlow::Break(EnumEvaluationError {
                                kind: EnumEvaluationErrorKind::NoVariant,
                                enum_index,
                                span: expr.span(),
                            });
                        }
                    }
                    ast::Expression::Field(field_expr) => {
                        if let ast::Expression::Ident(ident) = &*field_expr.base {
                            if let Some(&enum_index) = self.0.dict.get(ident) {
                                if let Some(&var_index) =
                                    self.0.enums[enum_index].dict.get(&field_expr.field)
                                {
                                    let val = self.0.enums[enum_index].variants[var_index].value;
                                    *expr = ast::Expression::Constant(
                                        Constant::Integer(val),
                                        field_expr.span,
                                    );
                                } else {
                                    return ControlFlow::Break(EnumEvaluationError {
                                        kind: EnumEvaluationErrorKind::BadVariant,
                                        enum_index,
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

    /// Turn an `EnumSet` back into an `EnumSetBuilder` to extend the set of enums.
    ///
    /// Existing enums and variants will retain the same indexes that they have in the `EnumSet`.
    pub fn into_builder(self) -> EnumSetBuilder<S> {
        EnumSetBuilder {
            enums: self
                .enums
                .into_iter()
                .map(|e| ResolvingEnum {
                    name: ast::Ident::new(e.name, e.name_span),
                    variants: e
                        .variants
                        .into_iter()
                        .map(|v| ResolvingEnumVariant {
                            name: ast::Ident::new(v.name, v.name_span),
                            kind: ResolvingEnumVariantKind::Evaluated(v.value),
                            span: v.span,
                        })
                        .collect(),
                    dict: e.dict,
                    span: e.span,
                })
                .collect(),
            dict: self.dict,
        }
    }
}
