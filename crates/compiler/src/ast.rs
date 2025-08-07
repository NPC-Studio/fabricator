use std::{
    borrow,
    fmt::Debug,
    hash,
    ops::{self, ControlFlow},
};

use fabricator_vm::Span;

use crate::constant::Constant;

#[derive(Debug, Clone)]
pub struct Block<S> {
    pub statements: Vec<Statement<S>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Statement<S> {
    pub kind: Box<StatementKind<S>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StatementKind<S> {
    Block(Block<S>),
    Enum(EnumStatement<S>),
    Function(FunctionStatement<S>),
    Var(Vec<Declaration<S>>),
    Static(Vec<Declaration<S>>),
    Assignment(AssignmentStatement<S>),
    Return(ReturnStatement<S>),
    If(IfStatement<S>),
    For(ForStatement<S>),
    While(LoopStatement<S>),
    Repeat(LoopStatement<S>),
    Switch(SwitchStatement<S>),
    With(LoopStatement<S>),
    Call(Call<S>),
    Prefix(MutationOp, MutableExpr<S>),
    Postfix(MutableExpr<S>, MutationOp),
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub struct EnumStatement<S> {
    pub name: Ident<S>,
    pub variants: Vec<(Ident<S>, Option<Expression<S>>)>,
}

#[derive(Debug, Clone)]
pub struct FunctionStatement<S> {
    pub name: Ident<S>,
    pub is_constructor: bool,
    pub inherit: Option<Call<S>>,
    pub parameters: Vec<Parameter<S>>,
    pub body: Block<S>,
}

#[derive(Debug, Clone)]
pub struct Declaration<S> {
    pub name: Ident<S>,
    pub value: Option<Expression<S>>,
}

#[derive(Debug, Clone)]
pub struct AssignmentStatement<S> {
    pub target: MutableExpr<S>,
    pub op: AssignmentOp,
    pub value: Expression<S>,
}

#[derive(Debug, Clone)]
pub enum MutableExpr<S> {
    Ident(Ident<S>),
    Field(FieldExpr<S>),
    Index(IndexExpr<S>),
}

#[derive(Debug, Clone)]
pub struct ReturnStatement<S> {
    pub value: Option<Expression<S>>,
}

#[derive(Debug, Clone)]
pub struct IfStatement<S> {
    pub condition: Expression<S>,
    pub then_stmt: Statement<S>,
    pub else_stmt: Option<Statement<S>>,
}

#[derive(Debug, Clone)]
pub struct ForStatement<S> {
    pub initializer: Statement<S>,
    pub condition: Expression<S>,
    pub iterator: Statement<S>,
    pub body: Statement<S>,
}

#[derive(Debug, Clone)]
pub struct LoopStatement<S> {
    pub target: Expression<S>,
    pub body: Statement<S>,
}

#[derive(Debug, Clone)]
pub struct SwitchStatement<S> {
    pub target: Expression<S>,
    pub cases: Vec<SwitchCase<S>>,
    pub default: Option<Block<S>>,
}

#[derive(Debug, Clone)]
pub struct SwitchCase<S> {
    pub compare: Expression<S>,
    pub body: Block<S>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind<S> {
    Global,
    This,
    Other,
    Constant(Constant<S>),
    Ident(Ident<S>),
    Group(Expression<S>),
    Object(Vec<Field<S>>),
    Array(Vec<Expression<S>>),
    Unary(UnaryOp, Expression<S>),
    Prefix(MutationOp, MutableExpr<S>),
    Postfix(MutableExpr<S>, MutationOp),
    Binary(Expression<S>, BinaryOp, Expression<S>),
    Ternary(TernaryExpr<S>),
    Function(FunctionExpr<S>),
    Call(Call<S>),
    Field(FieldExpr<S>),
    Index(IndexExpr<S>),
}

#[derive(Debug, Clone)]
pub struct Expression<S> {
    pub kind: Box<ExpressionKind<S>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TernaryExpr<S> {
    pub cond: Expression<S>,
    pub if_true: Expression<S>,
    pub if_false: Expression<S>,
}

#[derive(Debug, Clone)]
pub struct FunctionExpr<S> {
    pub parameters: Vec<Parameter<S>>,
    pub body: Block<S>,
}

#[derive(Debug, Clone)]
pub struct FieldExpr<S> {
    pub base: Expression<S>,
    pub field: Ident<S>,
}

#[derive(Debug, Clone)]
pub struct IndexExpr<S> {
    pub base: Expression<S>,
    pub accessor_type: Option<AccessorType>,
    pub indexes: Vec<Expression<S>>,
}

#[derive(Debug, Clone)]
pub struct Call<S> {
    pub base: Expression<S>,
    pub arguments: Vec<Expression<S>>,
    pub has_new: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Parameter<S> {
    pub name: Ident<S>,
    pub default: Option<Expression<S>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Field<S> {
    Value(Ident<S>, Expression<S>),
    Init(Ident<S>),
}

#[derive(Debug, Copy, Clone)]
pub struct Ident<S> {
    pub inner: S,
    pub span: Span,
}

impl<S> Ident<S> {
    pub fn new(inner: S, span: Span) -> Self {
        Self { inner, span }
    }
}

impl<S: PartialEq> PartialEq for Ident<S> {
    fn eq(&self, other: &Self) -> bool {
        self.inner.eq(&other.inner)
    }
}

impl<S: Eq> Eq for Ident<S> {}

impl<S: hash::Hash> hash::Hash for Ident<S> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state);
    }
}

impl<S> ops::Deref for Ident<S> {
    type Target = S;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<S> borrow::Borrow<S> for Ident<S> {
    fn borrow(&self) -> &S {
        &self.inner
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum UnaryOp {
    Not,
    Minus,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum MutationOp {
    Increment,
    Decrement,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mult,
    Div,
    Rem,
    IDiv,
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    And,
    Or,
    NullCoalesce,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum AssignmentOp {
    Equal,
    PlusEqual,
    MinusEqual,
    MultEqual,
    DivEqual,
    NullCoalesce,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum AccessorType {
    List,
    Map,
    Grid,
    Array,
    Struct,
}

pub trait Visitor<S>: Sized {
    type Break;

    fn visit_block(&mut self, block: &Block<S>) -> ControlFlow<Self::Break> {
        block.walk(self)
    }

    fn visit_stmt(&mut self, stmt: &Statement<S>) -> ControlFlow<Self::Break> {
        stmt.walk(self)
    }

    fn visit_expr(&mut self, expr: &Expression<S>) -> ControlFlow<Self::Break> {
        expr.walk(self)
    }
}

pub trait VisitorMut<S>: Sized {
    type Break;

    fn visit_block_mut(&mut self, block: &mut Block<S>) -> ControlFlow<Self::Break> {
        block.walk_mut(self)
    }

    fn visit_stmt_mut(&mut self, stmt: &mut Statement<S>) -> ControlFlow<Self::Break> {
        stmt.walk_mut(self)
    }

    fn visit_expr_mut(&mut self, expr: &mut Expression<S>) -> ControlFlow<Self::Break> {
        expr.walk_mut(self)
    }
}

impl<S> Block<S> {
    pub fn walk<V: Visitor<S>>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        for stmt in &self.statements {
            visitor.visit_stmt(stmt)?;
        }
        ControlFlow::Continue(())
    }

    pub fn walk_mut<V: VisitorMut<S>>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        for stmt in &mut self.statements {
            visitor.visit_stmt_mut(stmt)?;
        }
        ControlFlow::Continue(())
    }
}

impl<S> Statement<S> {
    pub fn walk<V: Visitor<S>>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        match self.kind.as_ref() {
            StatementKind::Block(block) => visitor.visit_block(block)?,
            StatementKind::Enum(enum_) => {
                enum_.walk(visitor)?;
            }
            StatementKind::Function(func_stmt) => {
                func_stmt.walk(visitor)?;
            }
            StatementKind::Var(decl_list) => {
                for decl in decl_list {
                    decl.walk(visitor)?;
                }
            }
            StatementKind::Static(decl_list) => {
                for decl in decl_list {
                    decl.walk(visitor)?;
                }
            }
            StatementKind::Assignment(assignment_stmt) => {
                assignment_stmt.walk(visitor)?;
            }
            StatementKind::Return(ret_stmt) => {
                ret_stmt.walk(visitor)?;
            }
            StatementKind::If(if_stmt) => {
                if_stmt.walk(visitor)?;
            }
            StatementKind::For(for_stmt) => {
                for_stmt.walk(visitor)?;
            }
            StatementKind::While(while_stmt) => {
                while_stmt.walk(visitor)?;
            }
            StatementKind::Repeat(repeat_stmt) => {
                repeat_stmt.walk(visitor)?;
            }
            StatementKind::Switch(switch_stmt) => {
                switch_stmt.walk(visitor)?;
            }
            StatementKind::With(with_stmt) => {
                with_stmt.walk(visitor)?;
            }
            StatementKind::Call(call_expr) => {
                call_expr.walk(visitor)?;
            }
            StatementKind::Prefix(_, target) => {
                target.walk(visitor)?;
            }
            StatementKind::Postfix(target, _) => {
                target.walk(visitor)?;
            }
            StatementKind::Break | StatementKind::Continue => {}
        }
        ControlFlow::Continue(())
    }

    pub fn walk_mut<V: VisitorMut<S>>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        match self.kind.as_mut() {
            StatementKind::Block(block) => visitor.visit_block_mut(block)?,
            StatementKind::Enum(enum_) => {
                enum_.walk_mut(visitor)?;
            }
            StatementKind::Function(func_stmt) => {
                func_stmt.walk_mut(visitor)?;
            }
            StatementKind::Var(decl_list) => {
                for decl in decl_list {
                    decl.walk_mut(visitor)?;
                }
            }
            StatementKind::Static(decl_list) => {
                for decl in decl_list {
                    decl.walk_mut(visitor)?;
                }
            }
            StatementKind::Assignment(assignment_stmt) => {
                assignment_stmt.walk_mut(visitor)?;
            }
            StatementKind::Return(ret_stmt) => {
                ret_stmt.walk_mut(visitor)?;
            }
            StatementKind::If(if_stmt) => {
                if_stmt.walk_mut(visitor)?;
            }
            StatementKind::For(for_stmt) => {
                for_stmt.walk_mut(visitor)?;
            }
            StatementKind::While(while_stmt) => {
                while_stmt.walk_mut(visitor)?;
            }
            StatementKind::Repeat(repeat_stmt) => {
                repeat_stmt.walk_mut(visitor)?;
            }
            StatementKind::Switch(switch_stmt) => {
                switch_stmt.walk_mut(visitor)?;
            }
            StatementKind::With(with_stmt) => {
                with_stmt.walk_mut(visitor)?;
            }
            StatementKind::Call(call_expr) => {
                call_expr.walk_mut(visitor)?;
            }
            StatementKind::Prefix(_, target) => {
                target.walk_mut(visitor)?;
            }
            StatementKind::Postfix(target, _) => {
                target.walk_mut(visitor)?;
            }
            StatementKind::Break | StatementKind::Continue => {}
        }
        ControlFlow::Continue(())
    }
}

impl<S> Expression<S> {
    pub fn walk<V: Visitor<S>>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        match self.kind.as_ref() {
            ExpressionKind::Group(expr) => visitor.visit_expr(expr)?,
            ExpressionKind::Object(fields) => {
                for field in fields {
                    field.walk(visitor)?;
                }
            }
            ExpressionKind::Array(items) => {
                for item in items {
                    visitor.visit_expr(item)?;
                }
            }
            ExpressionKind::Unary(_, expr) => visitor.visit_expr(expr)?,
            ExpressionKind::Prefix(_, target) => target.walk(visitor)?,
            ExpressionKind::Postfix(target, _) => target.walk(visitor)?,
            ExpressionKind::Binary(left, _, right) => {
                visitor.visit_expr(left)?;
                visitor.visit_expr(right)?;
            }
            ExpressionKind::Ternary(ternary) => {
                ternary.walk(visitor)?;
            }
            ExpressionKind::Function(func_expr) => {
                func_expr.walk(visitor)?;
            }
            ExpressionKind::Call(call_expr) => {
                call_expr.walk(visitor)?;
            }
            ExpressionKind::Field(field_expr) => {
                field_expr.walk(visitor)?;
            }
            ExpressionKind::Index(index_expr) => {
                index_expr.walk(visitor)?;
            }
            ExpressionKind::Ident(_)
            | ExpressionKind::Global
            | ExpressionKind::This
            | ExpressionKind::Other
            | ExpressionKind::Constant(_) => {}
        }
        ControlFlow::Continue(())
    }

    pub fn walk_mut<V: VisitorMut<S>>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        match self.kind.as_mut() {
            ExpressionKind::Group(expr) => visitor.visit_expr_mut(expr)?,
            ExpressionKind::Object(fields) => {
                for field in fields {
                    field.walk_mut(visitor)?;
                }
            }
            ExpressionKind::Array(items) => {
                for item in items {
                    visitor.visit_expr_mut(item)?;
                }
            }
            ExpressionKind::Unary(_, expr) => visitor.visit_expr_mut(expr)?,
            ExpressionKind::Prefix(_, target) => target.walk_mut(visitor)?,
            ExpressionKind::Postfix(target, _) => target.walk_mut(visitor)?,
            ExpressionKind::Binary(left, _, right) => {
                visitor.visit_expr_mut(left)?;
                visitor.visit_expr_mut(right)?;
            }
            ExpressionKind::Ternary(ternary) => {
                ternary.walk_mut(visitor)?;
            }
            ExpressionKind::Function(func_expr) => {
                func_expr.walk_mut(visitor)?;
            }
            ExpressionKind::Call(call_expr) => {
                call_expr.walk_mut(visitor)?;
            }
            ExpressionKind::Field(field_expr) => {
                field_expr.walk_mut(visitor)?;
            }
            ExpressionKind::Index(index_expr) => {
                index_expr.walk_mut(visitor)?;
            }
            ExpressionKind::Ident(_)
            | ExpressionKind::Global
            | ExpressionKind::This
            | ExpressionKind::Other
            | ExpressionKind::Constant(_) => {}
        }
        ControlFlow::Continue(())
    }
}

impl<S: Eq + Clone> Expression<S> {
    pub fn fold_constant(&self) -> Option<Constant<S>> {
        match &*self.kind {
            ExpressionKind::Constant(c) => Some(c.clone()),
            ExpressionKind::Group(expr) => expr.fold_constant(),
            ExpressionKind::Unary(unary_op, expr) => match unary_op {
                UnaryOp::Not => Some(Constant::Boolean(!expr.fold_constant()?.to_bool())),
                UnaryOp::Minus => expr.fold_constant()?.negate(),
            },
            ExpressionKind::Binary(l, op, r) => {
                let l = {
                    match &*l.kind {
                        ExpressionKind::Constant(c) => c,
                        _ => &l.fold_constant()?,
                    }
                };

                let r = {
                    match &*r.kind {
                        ExpressionKind::Constant(c) => c,
                        _ => &r.fold_constant()?,
                    }
                };

                match op {
                    BinaryOp::Add => l.add(r),
                    BinaryOp::Sub => l.sub(r),
                    BinaryOp::Mult => l.mult(r),
                    BinaryOp::Div => l.div(r),
                    BinaryOp::Rem => l.rem(r),
                    BinaryOp::IDiv => l.idiv(r).map(Constant::Integer),
                    BinaryOp::Equal => Some(Constant::Boolean(l.equal(r))),
                    BinaryOp::NotEqual => Some(Constant::Boolean(!l.equal(r))),
                    BinaryOp::LessThan => l.less_than(r).map(Constant::Boolean),
                    BinaryOp::LessEqual => l.less_equal(r).map(Constant::Boolean),
                    BinaryOp::GreaterThan => r.less_than(l).map(Constant::Boolean),
                    BinaryOp::GreaterEqual => r.less_equal(l).map(Constant::Boolean),
                    BinaryOp::And => Some(Constant::Boolean(l.to_bool() && r.to_bool())),
                    BinaryOp::Or => Some(Constant::Boolean(l.to_bool() || r.to_bool())),
                    BinaryOp::NullCoalesce => Some(if l.is_undefined() {
                        r.clone()
                    } else {
                        l.clone()
                    }),
                }
            }
            _ => None,
        }
    }
}

impl<S> EnumStatement<S> {
    pub fn walk<V: Visitor<S>>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        for (_, expr) in &self.variants {
            if let Some(expr) = expr {
                visitor.visit_expr(expr)?;
            }
        }
        ControlFlow::Continue(())
    }

    pub fn walk_mut<V: VisitorMut<S>>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        for (_, expr) in &mut self.variants {
            if let Some(expr) = expr {
                visitor.visit_expr_mut(expr)?;
            }
        }
        ControlFlow::Continue(())
    }
}

impl<S> FunctionStatement<S> {
    pub fn walk<V: Visitor<S>>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        if let Some(call) = &self.inherit {
            call.walk(visitor)?;
        }
        for parameter in &self.parameters {
            parameter.walk(visitor)?;
        }
        visitor.visit_block(&self.body)?;
        ControlFlow::Continue(())
    }

    pub fn walk_mut<V: VisitorMut<S>>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        if let Some(call) = &mut self.inherit {
            call.walk_mut(visitor)?;
        }
        for parameter in &mut self.parameters {
            parameter.walk_mut(visitor)?;
        }
        visitor.visit_block_mut(&mut self.body)?;
        ControlFlow::Continue(())
    }
}

impl<S> Declaration<S> {
    pub fn walk<V: Visitor<S>>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        if let Some(value) = &self.value {
            visitor.visit_expr(value)?;
        }
        ControlFlow::Continue(())
    }

    pub fn walk_mut<V: VisitorMut<S>>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        if let Some(value) = &mut self.value {
            visitor.visit_expr_mut(value)?;
        }
        ControlFlow::Continue(())
    }
}

impl<S> AssignmentStatement<S> {
    pub fn walk<V: Visitor<S>>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        self.target.walk(visitor)?;
        visitor.visit_expr(&self.value)?;
        ControlFlow::Continue(())
    }

    pub fn walk_mut<V: VisitorMut<S>>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        self.target.walk_mut(visitor)?;
        visitor.visit_expr_mut(&mut self.value)?;
        ControlFlow::Continue(())
    }
}

impl<S> MutableExpr<S> {
    pub fn walk<V: Visitor<S>>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        match self {
            MutableExpr::Ident(_) => {}
            MutableExpr::Field(field_expr) => {
                visitor.visit_expr(&field_expr.base)?;
            }
            MutableExpr::Index(index_expr) => {
                index_expr.walk(visitor)?;
            }
        }
        ControlFlow::Continue(())
    }

    pub fn walk_mut<V: VisitorMut<S>>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        match self {
            MutableExpr::Ident(_) => {}
            MutableExpr::Field(field_expr) => {
                visitor.visit_expr_mut(&mut field_expr.base)?;
            }
            MutableExpr::Index(index_expr) => {
                index_expr.walk_mut(visitor)?;
            }
        }
        ControlFlow::Continue(())
    }
}

impl<S> ReturnStatement<S> {
    pub fn walk<V: Visitor<S>>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        if let Some(val) = &self.value {
            visitor.visit_expr(val)?;
        }
        ControlFlow::Continue(())
    }

    pub fn walk_mut<V: VisitorMut<S>>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        if let Some(val) = &mut self.value {
            visitor.visit_expr_mut(val)?;
        }
        ControlFlow::Continue(())
    }
}

impl<S> IfStatement<S> {
    pub fn walk<V: Visitor<S>>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        visitor.visit_expr(&self.condition)?;
        visitor.visit_stmt(&self.then_stmt)?;
        if let Some(else_stmt) = &self.else_stmt {
            visitor.visit_stmt(else_stmt)?;
        }
        ControlFlow::Continue(())
    }

    pub fn walk_mut<V: VisitorMut<S>>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        visitor.visit_expr_mut(&mut self.condition)?;
        visitor.visit_stmt_mut(&mut self.then_stmt)?;
        if let Some(else_stmt) = &mut self.else_stmt {
            visitor.visit_stmt_mut(else_stmt)?;
        }
        ControlFlow::Continue(())
    }
}

impl<S> ForStatement<S> {
    pub fn walk<V: Visitor<S>>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        visitor.visit_stmt(&self.initializer)?;
        visitor.visit_expr(&self.condition)?;
        visitor.visit_stmt(&self.iterator)?;
        visitor.visit_stmt(&self.body)?;
        ControlFlow::Continue(())
    }

    pub fn walk_mut<V: VisitorMut<S>>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        visitor.visit_stmt_mut(&mut self.initializer)?;
        visitor.visit_expr_mut(&mut self.condition)?;
        visitor.visit_stmt_mut(&mut self.iterator)?;
        visitor.visit_stmt_mut(&mut self.body)?;
        ControlFlow::Continue(())
    }
}

impl<S> LoopStatement<S> {
    pub fn walk<V: Visitor<S>>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        visitor.visit_expr(&self.target)?;
        visitor.visit_stmt(&self.body)?;
        ControlFlow::Continue(())
    }

    pub fn walk_mut<V: VisitorMut<S>>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        visitor.visit_expr_mut(&mut self.target)?;
        visitor.visit_stmt_mut(&mut self.body)?;
        ControlFlow::Continue(())
    }
}

impl<S> SwitchStatement<S> {
    pub fn walk<V: Visitor<S>>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        visitor.visit_expr(&self.target)?;
        for case in &self.cases {
            case.walk(visitor)?;
        }
        if let Some(default_block) = &self.default {
            visitor.visit_block(default_block)?;
        }
        ControlFlow::Continue(())
    }

    pub fn walk_mut<V: VisitorMut<S>>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        visitor.visit_expr_mut(&mut self.target)?;
        for case in &mut self.cases {
            case.walk_mut(visitor)?;
        }
        if let Some(default_block) = &mut self.default {
            visitor.visit_block_mut(default_block)?;
        }
        ControlFlow::Continue(())
    }
}

impl<S> SwitchCase<S> {
    pub fn walk<V: Visitor<S>>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        visitor.visit_expr(&self.compare)?;
        visitor.visit_block(&self.body)?;
        ControlFlow::Continue(())
    }

    pub fn walk_mut<V: VisitorMut<S>>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        visitor.visit_expr_mut(&mut self.compare)?;
        visitor.visit_block_mut(&mut self.body)?;
        ControlFlow::Continue(())
    }
}

impl<S> TernaryExpr<S> {
    pub fn walk<V: Visitor<S>>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        visitor.visit_expr(&self.cond)?;
        visitor.visit_expr(&self.if_true)?;
        visitor.visit_expr(&self.if_false)?;
        ControlFlow::Continue(())
    }

    pub fn walk_mut<V: VisitorMut<S>>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        visitor.visit_expr_mut(&mut self.cond)?;
        visitor.visit_expr_mut(&mut self.if_true)?;
        visitor.visit_expr_mut(&mut self.if_false)?;
        ControlFlow::Continue(())
    }
}

impl<S> FunctionExpr<S> {
    pub fn walk<V: Visitor<S>>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        for parameter in &self.parameters {
            parameter.walk(visitor)?;
        }
        visitor.visit_block(&self.body)?;
        ControlFlow::Continue(())
    }

    pub fn walk_mut<V: VisitorMut<S>>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        for parameter in &mut self.parameters {
            parameter.walk_mut(visitor)?;
        }
        visitor.visit_block_mut(&mut self.body)?;
        ControlFlow::Continue(())
    }
}

impl<S> Call<S> {
    pub fn walk<V: Visitor<S>>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        visitor.visit_expr(&self.base)?;
        for arg in &self.arguments {
            visitor.visit_expr(arg)?;
        }
        ControlFlow::Continue(())
    }

    pub fn walk_mut<V: VisitorMut<S>>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        visitor.visit_expr_mut(&mut self.base)?;
        for arg in &mut self.arguments {
            visitor.visit_expr_mut(arg)?;
        }
        ControlFlow::Continue(())
    }
}

impl<S> FieldExpr<S> {
    pub fn walk<V: Visitor<S>>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        visitor.visit_expr(&self.base)?;
        ControlFlow::Continue(())
    }

    pub fn walk_mut<V: VisitorMut<S>>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        visitor.visit_expr_mut(&mut self.base)?;
        ControlFlow::Continue(())
    }
}

impl<S> IndexExpr<S> {
    pub fn walk<V: Visitor<S>>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        visitor.visit_expr(&self.base)?;
        for expr in &self.indexes {
            visitor.visit_expr(expr)?;
        }
        ControlFlow::Continue(())
    }

    pub fn walk_mut<V: VisitorMut<S>>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        visitor.visit_expr_mut(&mut self.base)?;
        for expr in &mut self.indexes {
            visitor.visit_expr_mut(expr)?;
        }
        ControlFlow::Continue(())
    }
}

impl<S> Parameter<S> {
    pub fn walk<V: Visitor<S>>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        if let Some(default) = &self.default {
            visitor.visit_expr(default)?;
        }
        ControlFlow::Continue(())
    }

    pub fn walk_mut<V: VisitorMut<S>>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        if let Some(default) = &mut self.default {
            visitor.visit_expr_mut(default)?;
        }
        ControlFlow::Continue(())
    }
}

impl<S> Field<S> {
    pub fn walk<V: Visitor<S>>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        match self {
            Field::Value(_, expr) => visitor.visit_expr(expr)?,
            Field::Init(_) => {}
        }
        ControlFlow::Continue(())
    }

    pub fn walk_mut<V: VisitorMut<S>>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        match self {
            Field::Value(_, expr) => visitor.visit_expr_mut(expr)?,
            Field::Init(_) => {}
        }
        ControlFlow::Continue(())
    }
}
