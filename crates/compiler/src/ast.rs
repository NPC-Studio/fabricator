use std::{fmt::Debug, ops::ControlFlow};

use fabricator_vm::Span;

use crate::constant::Constant;

#[derive(Debug, Clone)]
pub struct Block<S> {
    pub statements: Vec<Statement<S>>,
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
    Var(VarStatement<S>),
    Assignment(AssignmentStatement<S>),
    Return(ReturnStatement<S>),
    If(IfStatement<S>),
    For(ForStatement<S>),
    Call(CallExpr<S>),
}

#[derive(Debug, Clone)]
pub struct EnumStatement<S> {
    pub name: S,
    pub variants: Vec<(S, Option<Expression<S>>)>,
}

#[derive(Debug, Clone)]
pub struct FunctionStatement<S> {
    pub name: S,
    pub parameters: Vec<Parameter<S>>,
    pub body: Block<S>,
}

#[derive(Debug, Clone)]
pub struct VarStatement<S> {
    pub name: S,
    pub value: Expression<S>,
}

#[derive(Debug, Clone)]
pub struct AssignmentStatement<S> {
    pub target: AssignmentTarget<S>,
    pub op: AssignmentOp,
    pub value: Expression<S>,
}

#[derive(Debug, Clone)]
pub enum AssignmentTarget<S> {
    Name(S),
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
pub enum ExpressionKind<S> {
    This,
    Constant(Constant<S>),
    Name(S),
    Group(Expression<S>),
    Object(Vec<(S, Expression<S>)>),
    Array(Vec<Expression<S>>),
    Unary(UnaryOp, Expression<S>),
    Binary(Expression<S>, BinaryOp, Expression<S>),
    Function(FunctionExpr<S>),
    Call(CallExpr<S>),
    Field(FieldExpr<S>),
    Index(IndexExpr<S>),
}

#[derive(Debug, Clone)]
pub struct Expression<S> {
    pub kind: Box<ExpressionKind<S>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FunctionExpr<S> {
    pub parameters: Vec<Parameter<S>>,
    pub body: Block<S>,
}

#[derive(Debug, Clone)]
pub struct CallExpr<S> {
    pub base: Expression<S>,
    pub arguments: Vec<Expression<S>>,
}

#[derive(Debug, Clone)]
pub struct FieldExpr<S> {
    pub base: Expression<S>,
    pub field: S,
}

#[derive(Debug, Clone)]
pub struct IndexExpr<S> {
    pub base: Expression<S>,
    pub index: Expression<S>,
}

#[derive(Debug, Clone)]
pub struct Parameter<S> {
    pub name: S,
    pub default: Option<Expression<S>>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum UnaryOp {
    Not,
    Minus,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum BinaryOp {
    Mult,
    Div,
    Add,
    Sub,
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    And,
    Or,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum AssignmentOp {
    Equal,
    PlusEqual,
    MinusEqual,
    MultEqual,
    DivEqual,
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
            StatementKind::Enum(enom) => {
                for (_, expr) in &enom.variants {
                    if let Some(expr) = expr {
                        visitor.visit_expr(expr)?;
                    }
                }
            }
            StatementKind::Function(func_stmt) => {
                visitor.visit_block(&func_stmt.body)?;
            }
            StatementKind::Var(var_stmt) => {
                visitor.visit_expr(&var_stmt.value)?;
            }
            StatementKind::Assignment(assignment_stmt) => {
                match &assignment_stmt.target {
                    AssignmentTarget::Name(_) => {}
                    AssignmentTarget::Field(field_expr) => {
                        visitor.visit_expr(&field_expr.base)?;
                    }
                    AssignmentTarget::Index(index_expr) => {
                        visitor.visit_expr(&index_expr.base)?;
                        visitor.visit_expr(&index_expr.index)?;
                    }
                }
                visitor.visit_expr(&assignment_stmt.value)?;
            }
            StatementKind::Return(ret_stmt) => {
                if let Some(val) = &ret_stmt.value {
                    visitor.visit_expr(val)?;
                }
            }
            StatementKind::If(if_stmt) => {
                visitor.visit_expr(&if_stmt.condition)?;
                visitor.visit_stmt(&if_stmt.then_stmt)?;
                if let Some(else_stmt) = &if_stmt.else_stmt {
                    visitor.visit_stmt(else_stmt)?;
                }
            }
            StatementKind::For(for_stmt) => {
                visitor.visit_stmt(&for_stmt.initializer)?;
                visitor.visit_expr(&for_stmt.condition)?;
                visitor.visit_stmt(&for_stmt.iterator)?;
                visitor.visit_stmt(&for_stmt.body)?;
            }
            StatementKind::Call(call_expr) => {
                visitor.visit_expr(&call_expr.base)?;
                for arg in &call_expr.arguments {
                    visitor.visit_expr(arg)?;
                }
            }
        }
        ControlFlow::Continue(())
    }

    pub fn walk_mut<V: VisitorMut<S>>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        match self.kind.as_mut() {
            StatementKind::Block(block) => visitor.visit_block_mut(block)?,
            StatementKind::Enum(enom) => {
                for (_, expr) in &mut enom.variants {
                    if let Some(expr) = expr {
                        visitor.visit_expr_mut(expr)?;
                    }
                }
            }
            StatementKind::Function(func_stmt) => {
                visitor.visit_block_mut(&mut func_stmt.body)?;
            }
            StatementKind::Var(var_stmt) => {
                visitor.visit_expr_mut(&mut var_stmt.value)?;
            }
            StatementKind::Assignment(assignment_stmt) => {
                match &mut assignment_stmt.target {
                    AssignmentTarget::Name(_) => {}
                    AssignmentTarget::Field(field_expr) => {
                        visitor.visit_expr_mut(&mut field_expr.base)?;
                    }
                    AssignmentTarget::Index(index_expr) => {
                        visitor.visit_expr_mut(&mut index_expr.base)?;
                        visitor.visit_expr_mut(&mut index_expr.index)?;
                    }
                }
                visitor.visit_expr_mut(&mut assignment_stmt.value)?;
            }
            StatementKind::Return(ret_stmt) => {
                if let Some(val) = &mut ret_stmt.value {
                    visitor.visit_expr_mut(val)?;
                }
            }
            StatementKind::If(if_stmt) => {
                visitor.visit_expr_mut(&mut if_stmt.condition)?;
                visitor.visit_stmt_mut(&mut if_stmt.then_stmt)?;
                if let Some(else_stmt) = &mut if_stmt.else_stmt {
                    visitor.visit_stmt_mut(else_stmt)?;
                }
            }
            StatementKind::For(for_stmt) => {
                visitor.visit_stmt_mut(&mut for_stmt.initializer)?;
                visitor.visit_expr_mut(&mut for_stmt.condition)?;
                visitor.visit_stmt_mut(&mut for_stmt.iterator)?;
                visitor.visit_stmt_mut(&mut for_stmt.body)?;
            }
            StatementKind::Call(call_expr) => {
                visitor.visit_expr_mut(&mut call_expr.base)?;
                for arg in &mut call_expr.arguments {
                    visitor.visit_expr_mut(arg)?;
                }
            }
        }
        ControlFlow::Continue(())
    }
}

impl<S> Expression<S> {
    pub fn walk<V: Visitor<S>>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        match self.kind.as_ref() {
            ExpressionKind::Name(_) => {}
            ExpressionKind::Group(expr) => visitor.visit_expr(expr)?,
            ExpressionKind::Object(items) => {
                for (_, item) in items {
                    visitor.visit_expr(item)?;
                }
            }
            ExpressionKind::Array(items) => {
                for item in items {
                    visitor.visit_expr(item)?;
                }
            }
            ExpressionKind::Unary(_, expr) => visitor.visit_expr(expr)?,
            ExpressionKind::Binary(left, _, right) => {
                visitor.visit_expr(left)?;
                visitor.visit_expr(right)?;
            }
            ExpressionKind::Function(func_expr) => {
                visitor.visit_block(&func_expr.body)?;
            }
            ExpressionKind::Call(call_expr) => {
                visitor.visit_expr(&call_expr.base)?;
                for arg in &call_expr.arguments {
                    visitor.visit_expr(arg)?;
                }
            }
            ExpressionKind::Field(field_expr) => {
                visitor.visit_expr(&field_expr.base)?;
            }
            ExpressionKind::Index(index_expr) => {
                visitor.visit_expr(&index_expr.base)?;
                visitor.visit_expr(&index_expr.index)?;
            }
            ExpressionKind::This => {}
            ExpressionKind::Constant(_) => {}
        }
        ControlFlow::Continue(())
    }

    pub fn walk_mut<V: VisitorMut<S>>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        match self.kind.as_mut() {
            ExpressionKind::Name(_) => {}
            ExpressionKind::Group(expr) => visitor.visit_expr_mut(expr)?,
            ExpressionKind::Object(items) => {
                for (_, item) in items {
                    visitor.visit_expr_mut(item)?;
                }
            }
            ExpressionKind::Array(items) => {
                for item in items {
                    visitor.visit_expr_mut(item)?;
                }
            }
            ExpressionKind::Unary(_, expr) => visitor.visit_expr_mut(expr)?,
            ExpressionKind::Binary(left, _, right) => {
                visitor.visit_expr_mut(left)?;
                visitor.visit_expr_mut(right)?;
            }
            ExpressionKind::Function(func_expr) => {
                visitor.visit_block_mut(&mut func_expr.body)?;
            }
            ExpressionKind::Call(call_expr) => {
                visitor.visit_expr_mut(&mut call_expr.base)?;
                for arg in &mut call_expr.arguments {
                    visitor.visit_expr_mut(arg)?;
                }
            }
            ExpressionKind::Field(field_expr) => {
                visitor.visit_expr_mut(&mut field_expr.base)?;
            }
            ExpressionKind::Index(index_expr) => {
                visitor.visit_expr_mut(&mut index_expr.base)?;
                visitor.visit_expr_mut(&mut index_expr.index)?;
            }
            ExpressionKind::This => {}
            ExpressionKind::Constant(_) => {}
        }
        ControlFlow::Continue(())
    }

    pub fn fold_constant(self) -> Option<Constant<S>> {
        match *self.kind {
            ExpressionKind::Constant(c) => Some(c),
            ExpressionKind::Group(expr) => expr.fold_constant(),
            ExpressionKind::Unary(unary_op, expr) => match unary_op {
                UnaryOp::Not => Some(Constant::Boolean(!expr.fold_constant()?.to_bool())),
                UnaryOp::Minus => expr.fold_constant()?.negate(),
            },
            ExpressionKind::Binary(l, op, r) => {
                let l = l.fold_constant()?;
                let r = r.fold_constant()?;

                match op {
                    BinaryOp::Add => l.add(r),
                    BinaryOp::Sub => l.sub(r),
                    BinaryOp::Mult => l.mult(r),
                    BinaryOp::Div => l.div(r),
                    BinaryOp::Equal => l.equal(r).map(Constant::Boolean),
                    BinaryOp::NotEqual => l.equal(r).map(|b| Constant::Boolean(!b)),
                    BinaryOp::LessThan => l.less_than(r).map(Constant::Boolean),
                    BinaryOp::LessEqual => l.less_equal(r).map(Constant::Boolean),
                    BinaryOp::GreaterThan => r.less_than(l).map(Constant::Boolean),
                    BinaryOp::GreaterEqual => r.less_equal(l).map(Constant::Boolean),
                    BinaryOp::And => Some(Constant::Boolean(l.to_bool() && r.to_bool())),
                    BinaryOp::Or => Some(Constant::Boolean(l.to_bool() || r.to_bool())),
                }
            }
            _ => None,
        }
    }
}
