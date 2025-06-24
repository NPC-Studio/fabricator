use std::{fmt::Debug, ops::ControlFlow};

use arrayvec::ArrayVec;
use fabricator_vm::Span;
use thiserror::Error;

use crate::{
    constant::Constant,
    lexer::{Token, TokenKind},
};

#[derive(Debug, Clone)]
pub struct Block<S> {
    pub statements: Vec<Statement<S>>,
}

impl<S> Block<S> {
    /// Visits each expression in this block.
    ///
    /// Expressions are guaranteed to be visited in a depth-first, first to last order (in order of
    /// appearance in the original source file).
    ///
    /// If an expression is modified by the given callback, then the callback will be called on any
    /// sub-expressions present *after* modification.
    pub fn for_each_expr<B>(
        &mut self,
        mut f: impl Fn(&mut Expression<S>) -> ControlFlow<B>,
    ) -> ControlFlow<B> {
        fn block_each_expr<S, B>(
            f: &mut dyn Fn(&mut Expression<S>) -> ControlFlow<B>,
            block: &mut Block<S>,
        ) -> ControlFlow<B> {
            for stmt in &mut block.statements {
                stmt_each_expr(f, stmt)?;
            }
            ControlFlow::Continue(())
        }

        fn stmt_each_expr<S, B>(
            f: &mut dyn Fn(&mut Expression<S>) -> ControlFlow<B>,
            stmt: &mut Statement<S>,
        ) -> ControlFlow<B> {
            match stmt.kind.as_mut() {
                StatementKind::Enum(_) => {}
                StatementKind::Var(var_statement) => {
                    expr_each_expr(f, &mut var_statement.value)?;
                }
                StatementKind::Assignment(assignment_statement) => {
                    match &mut assignment_statement.target {
                        AssignmentTarget::Name(_) => {}
                        AssignmentTarget::Field(field_expr) => {
                            expr_each_expr(f, &mut field_expr.base)?;
                        }
                        AssignmentTarget::Index(index_expr) => {
                            expr_each_expr(f, &mut index_expr.base)?;
                            expr_each_expr(f, &mut index_expr.index)?;
                        }
                    }
                    expr_each_expr(f, &mut assignment_statement.value)?;
                }
                StatementKind::Return(ret_statement) => {
                    if let Some(val) = &mut ret_statement.value {
                        expr_each_expr(f, val)?;
                    }
                }
                StatementKind::If(if_statement) => {
                    expr_each_expr(f, &mut if_statement.condition)?;
                    stmt_each_expr(f, &mut if_statement.then_stmt)?;
                    if let Some(else_stmt) = &mut if_statement.else_stmt {
                        stmt_each_expr(f, else_stmt)?;
                    }
                }
                StatementKind::For(for_statement) => {
                    stmt_each_expr(f, &mut for_statement.initializer)?;
                    expr_each_expr(f, &mut for_statement.condition)?;
                    stmt_each_expr(f, &mut for_statement.iterator)?;
                    stmt_each_expr(f, &mut for_statement.body)?;
                }
                StatementKind::Block(block) => block_each_expr(f, block)?,
                StatementKind::Call(call_expr) => {
                    expr_each_expr(f, &mut call_expr.base)?;
                    for arg in &mut call_expr.arguments {
                        expr_each_expr(f, arg)?;
                    }
                }
            }
            ControlFlow::Continue(())
        }

        fn expr_each_expr<S, B>(
            f: &mut dyn Fn(&mut Expression<S>) -> ControlFlow<B>,
            expr: &mut Expression<S>,
        ) -> ControlFlow<B> {
            f(expr)?;

            match expr.kind.as_mut() {
                ExpressionKind::Name(_) => {}
                ExpressionKind::Group(expr) => expr_each_expr(f, expr)?,
                ExpressionKind::Object(items) => {
                    for (_, item) in items {
                        expr_each_expr(f, item)?;
                    }
                }
                ExpressionKind::Array(items) => {
                    for item in items {
                        expr_each_expr(f, item)?;
                    }
                }
                ExpressionKind::Unary(_, expr) => {
                    expr_each_expr(f, expr)?;
                }
                ExpressionKind::Binary(left, _, right) => {
                    expr_each_expr(f, left)?;
                    expr_each_expr(f, right)?;
                }
                ExpressionKind::Function(func_expr) => {
                    block_each_expr(f, &mut func_expr.body)?;
                }
                ExpressionKind::Call(call_expr) => {
                    expr_each_expr(f, &mut call_expr.base)?;
                    for arg in &mut call_expr.arguments {
                        expr_each_expr(f, arg)?;
                    }
                }
                ExpressionKind::Field(field_expr) => {
                    expr_each_expr(f, &mut field_expr.base)?;
                }
                ExpressionKind::Index(index_expr) => {
                    expr_each_expr(f, &mut index_expr.base)?;
                    expr_each_expr(f, &mut index_expr.index)?;
                }
                _ => {}
            }
            ControlFlow::Continue(())
        }

        block_each_expr(&mut f, self)
    }
}

#[derive(Debug, Clone)]
pub struct Statement<S> {
    pub kind: Box<StatementKind<S>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StatementKind<S> {
    Enum(EnumStatement<S>),
    Var(VarStatement<S>),
    Assignment(AssignmentStatement<S>),
    Return(ReturnStatement<S>),
    If(IfStatement<S>),
    For(ForStatement<S>),
    Block(Block<S>),
    Call(CallExpr<S>),
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
pub struct EnumStatement<S> {
    pub name: S,
    pub variants: Vec<(S, Option<Expression<S>>)>,
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
    pub parameters: Vec<S>,
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

#[derive(Debug, Error)]
pub enum ParseErrorKind {
    #[error("found {unexpected:?}, expected {expected:?}")]
    Unexpected {
        unexpected: &'static str,
        expected: &'static str,
    },
    #[error("invalid numeric literal")]
    BadNumber,
}

#[derive(Debug, Error)]
#[error("{kind}")]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

#[derive(Debug, Copy, Clone)]
pub struct ParseSettings {
    /// Require semicolons at the end of all statements other than block-like statements like `if`
    /// and `for`.
    pub strict: bool,
}

impl ParseSettings {
    pub fn strict() -> Self {
        ParseSettings { strict: true }
    }

    pub fn compat() -> Self {
        ParseSettings { strict: false }
    }

    pub fn parse<I, S>(self, token_iter: I) -> Result<Block<S>, ParseError>
    where
        I: IntoIterator<Item = Token<S>>,
        S: AsRef<str>,
    {
        Parser::new(self, token_iter.into_iter()).parse()
    }
}

struct Parser<I, S> {
    settings: ParseSettings,
    token_iter: I,
    look_ahead_buffer: ArrayVec<Token<S>, 1>,
    end_of_stream_span: Span,
}

impl<I, S> Parser<I, S>
where
    I: Iterator<Item = Token<S>>,
    S: AsRef<str>,
{
    fn new(settings: ParseSettings, token_iter: I) -> Self {
        Parser {
            settings,
            token_iter,
            look_ahead_buffer: ArrayVec::new(),
            end_of_stream_span: Span::null(),
        }
    }

    fn parse(&mut self) -> Result<Block<S>, ParseError> {
        let block = self.parse_block()?;

        self.look_ahead(1);
        let last = self.peek(0);
        match &last.kind {
            TokenKind::EndOfStream => Ok(block),
            _ => Err(ParseError {
                kind: ParseErrorKind::Unexpected {
                    unexpected: token_indicator(&last.kind),
                    expected: "<statement>",
                },
                span: last.span,
            }),
        }
    }

    fn parse_block(&mut self) -> Result<Block<S>, ParseError> {
        let mut statements = Vec::new();
        loop {
            self.look_ahead(1);
            if matches!(
                self.peek(0).kind,
                TokenKind::RightBrace | TokenKind::EndOfStream
            ) {
                break;
            }

            let stmt = self.parse_statement()?;

            if self.settings.strict {
                if !matches!(
                    &*stmt.kind,
                    StatementKind::Enum(_)
                        | StatementKind::If(_)
                        | StatementKind::For(_)
                        | StatementKind::Block(_)
                ) {
                    self.parse_token(TokenKind::SemiColon)?;
                }
            } else {
                self.look_ahead(1);
                if matches!(self.peek(0).kind, TokenKind::SemiColon) {
                    self.advance(1);
                }
            }

            statements.push(stmt);
        }

        Ok(Block { statements })
    }

    fn parse_statement(&mut self) -> Result<Statement<S>, ParseError> {
        self.look_ahead(1);
        let &Token { ref kind, mut span } = self.peek(0);
        match kind {
            TokenKind::Enum => {
                let (stmt, span) = self.parse_enum_statement()?;
                Ok(Statement {
                    kind: Box::new(StatementKind::Enum(stmt)),
                    span,
                })
            }
            TokenKind::Var => {
                self.advance(1);
                let name = self.parse_identifier()?.0;
                self.parse_token(TokenKind::Equal)?;
                let value = self.parse_expression()?;
                span = span.combine(value.span);
                Ok(Statement {
                    kind: Box::new(StatementKind::Var(VarStatement { name, value })),
                    span,
                })
            }
            TokenKind::Return => {
                self.advance(1);
                self.look_ahead(1);
                let value = if matches!(self.peek(0).kind, TokenKind::SemiColon) {
                    None
                } else {
                    let expr = self.parse_expression()?;
                    span = span.combine(expr.span);
                    Some(expr)
                };
                Ok(Statement {
                    kind: Box::new(StatementKind::Return(ReturnStatement { value })),
                    span,
                })
            }
            TokenKind::Exit => {
                self.advance(1);
                Ok(Statement {
                    kind: Box::new(StatementKind::Return(ReturnStatement { value: None })),
                    span,
                })
            }
            TokenKind::If => {
                self.advance(1);
                let condition = self.parse_expression()?;
                let then_stmt = self.parse_statement()?;
                span = span.combine(then_stmt.span);

                let mut else_stmt = None;

                self.look_ahead(1);
                let next = self.peek(0);
                if matches!(next.kind, TokenKind::Else) {
                    span = span.combine(next.span);
                    self.advance(1);
                    let stmt = self.parse_statement()?;
                    span = span.combine(stmt.span);
                    else_stmt = Some(stmt);
                }

                Ok(Statement {
                    kind: Box::new(StatementKind::If(IfStatement {
                        condition,
                        then_stmt,
                        else_stmt,
                    })),
                    span,
                })
            }
            TokenKind::For => {
                self.advance(1);
                self.parse_token(TokenKind::LeftParen)?;
                let initializer = self.parse_statement()?;
                self.parse_token(TokenKind::SemiColon)?;
                let condition = self.parse_expression()?;
                self.parse_token(TokenKind::SemiColon)?;
                let iterator = self.parse_statement()?;
                self.parse_token(TokenKind::RightParen)?;
                let body = self.parse_statement()?;

                span = span.combine(body.span);
                Ok(Statement {
                    kind: Box::new(StatementKind::For(ForStatement {
                        initializer,
                        condition,
                        iterator,
                        body,
                    })),
                    span,
                })
            }
            TokenKind::LeftBrace => {
                self.advance(1);
                let block = self.parse_block()?;
                span = span.combine(self.parse_token(TokenKind::RightBrace)?);
                Ok(Statement {
                    kind: Box::new(StatementKind::Block(block)),
                    span,
                })
            }
            _ => {
                let expr = match self.parse_suffixed_expression() {
                    Ok(expr) => expr,
                    Err(ParseError {
                        kind: ParseErrorKind::Unexpected { unexpected, .. },
                        span,
                    }) => {
                        return Err(ParseError {
                            kind: ParseErrorKind::Unexpected {
                                unexpected,
                                expected: "<statement>",
                            },
                            span,
                        });
                    }
                    Err(err) => return Err(err),
                };

                span = expr.span;

                let kind = match *expr.kind {
                    ExpressionKind::Call(call) => StatementKind::Call(call),
                    expr @ (ExpressionKind::Name(_)
                    | ExpressionKind::Field(_)
                    | ExpressionKind::Index(_)) => {
                        let target = match expr {
                            ExpressionKind::Name(name) => AssignmentTarget::Name(name),
                            ExpressionKind::Field(field) => AssignmentTarget::Field(field),
                            ExpressionKind::Index(index) => AssignmentTarget::Index(index),
                            _ => unreachable!(),
                        };

                        self.look_ahead(1);
                        let Some(assignment_op) = get_assignment_operator(&self.peek(0).kind)
                        else {
                            return Err(ParseError {
                                kind: ParseErrorKind::Unexpected {
                                    unexpected: "<non-statement expression>",
                                    expected: "<statement>",
                                },
                                span,
                            });
                        };
                        self.advance(1);

                        let value = self.parse_expression()?;
                        span = span.combine(value.span);

                        StatementKind::Assignment(AssignmentStatement {
                            target,
                            op: assignment_op,
                            value,
                        })
                    }
                    _ => {
                        return Err(ParseError {
                            kind: ParseErrorKind::Unexpected {
                                unexpected: "<non-statement expression>",
                                expected: "<statement>",
                            },
                            span,
                        });
                    }
                };

                Ok(Statement {
                    kind: Box::new(kind),
                    span,
                })
            }
        }
    }

    fn parse_enum_statement(&mut self) -> Result<(EnumStatement<S>, Span), ParseError> {
        let mut span = self.parse_token(TokenKind::Enum)?;
        let name = self.parse_identifier()?.0;
        span = span.combine(self.parse_token(TokenKind::LeftBrace)?);

        let mut variants = Vec::new();

        loop {
            self.look_ahead(1);
            if matches!(self.peek(0).kind, TokenKind::RightBrace) {
                break;
            }

            let key = self.parse_identifier()?.0;

            self.look_ahead(1);
            let value = if matches!(self.peek(0).kind, TokenKind::Equal) {
                self.advance(1);
                Some(self.parse_expression()?)
            } else {
                None
            };

            variants.push((key, value));

            self.look_ahead(1);
            let next = self.peek(0);
            match &next.kind {
                TokenKind::Comma => {
                    self.advance(1);
                }
                TokenKind::RightBrace => {
                    break;
                }
                _ => {
                    return Err(ParseError {
                        kind: ParseErrorKind::Unexpected {
                            unexpected: token_indicator(&next.kind),
                            expected: "',' or '}'",
                        },
                        span: next.span,
                    });
                }
            }
        }

        span = span.combine(self.parse_token(TokenKind::RightBrace)?);

        Ok((EnumStatement { name, variants }, span))
    }

    fn parse_expression(&mut self) -> Result<Expression<S>, ParseError> {
        self.parse_sub_expression(MIN_PRIORITY)
    }

    fn parse_sub_expression(
        &mut self,
        priority_limit: OperatorPriority,
    ) -> Result<Expression<S>, ParseError> {
        self.look_ahead(1);
        let next = self.peek(0);
        let mut expr = if let Some(unary_op) = get_unary_operator(&next.kind) {
            let mut span = next.span;
            self.advance(1);
            let target = self.parse_sub_expression(UNARY_PRIORITY)?;
            span = span.combine(target.span);
            Expression {
                kind: Box::new(ExpressionKind::Unary(unary_op, target)),
                span,
            }
        } else {
            self.parse_simple_expression()?
        };

        loop {
            self.look_ahead(1);
            let Some(binary_op) = get_binary_operator(&self.peek(0).kind) else {
                break;
            };

            let (left_priority, right_priority) = binary_priority(binary_op);
            if left_priority <= priority_limit {
                break;
            }

            self.advance(1);

            let right_expression = self.parse_sub_expression(right_priority)?;
            let span = expr.span.combine(right_expression.span);
            expr = Expression {
                kind: Box::new(ExpressionKind::Binary(expr, binary_op, right_expression)),
                span,
            };
        }

        Ok(expr)
    }

    fn parse_suffixed_expression(&mut self) -> Result<Expression<S>, ParseError> {
        let mut expr = self.parse_primary_expression()?;
        loop {
            self.look_ahead(1);
            match &self.peek(0).kind {
                TokenKind::LeftParen => {
                    self.advance(1);

                    let mut arguments = Vec::new();
                    loop {
                        self.look_ahead(1);
                        if matches!(self.peek(0).kind, TokenKind::RightParen) {
                            break;
                        }

                        arguments.push(self.parse_expression()?);

                        self.look_ahead(1);
                        let next = self.peek(0);
                        match &next.kind {
                            TokenKind::Comma => {
                                self.advance(1);
                            }
                            TokenKind::RightParen => {
                                break;
                            }
                            _ => {
                                return Err(ParseError {
                                    kind: ParseErrorKind::Unexpected {
                                        unexpected: token_indicator(&next.kind),
                                        expected: "',' or ')'",
                                    },
                                    span: next.span,
                                });
                            }
                        }
                    }

                    let span = expr.span.combine(self.parse_token(TokenKind::RightParen)?);
                    expr = Expression {
                        kind: Box::new(ExpressionKind::Call(CallExpr {
                            base: expr,
                            arguments,
                        })),
                        span,
                    };
                }
                TokenKind::Dot => {
                    self.advance(1);
                    let (field, fspan) = self.parse_identifier()?;
                    let span = expr.span.combine(fspan);
                    expr = Expression {
                        kind: Box::new(ExpressionKind::Field(FieldExpr { base: expr, field })),
                        span,
                    };
                }
                TokenKind::LeftBracket => {
                    self.advance(1);
                    let index = self.parse_expression()?;
                    let span = expr
                        .span
                        .combine(self.parse_token(TokenKind::RightBracket)?);
                    expr = Expression {
                        kind: Box::new(ExpressionKind::Index(IndexExpr { base: expr, index })),
                        span,
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_primary_expression(&mut self) -> Result<Expression<S>, ParseError> {
        let Token { kind, mut span } = self.next();
        match kind {
            TokenKind::LeftParen => {
                let expr = self.parse_expression()?;
                span = span.combine(self.parse_token(TokenKind::RightParen)?);
                Ok(Expression {
                    kind: Box::new(ExpressionKind::Group(expr)),
                    span,
                })
            }
            TokenKind::Identifier(n) => Ok(Expression {
                kind: Box::new(ExpressionKind::Name(n)),
                span,
            }),
            token => Err(ParseError {
                kind: ParseErrorKind::Unexpected {
                    unexpected: token_indicator(&token),
                    expected: "<grouped expression or name>",
                },
                span,
            }),
        }
    }

    fn parse_simple_expression(&mut self) -> Result<Expression<S>, ParseError> {
        self.look_ahead(1);
        let &Token { ref kind, mut span } = self.peek(0);
        match kind {
            TokenKind::Undefined => {
                self.advance(1);
                Ok(Expression {
                    kind: Box::new(ExpressionKind::Constant(Constant::Undefined)),
                    span,
                })
            }
            TokenKind::This => {
                self.advance(1);
                Ok(Expression {
                    kind: Box::new(ExpressionKind::This),
                    span,
                })
            }
            TokenKind::True => {
                self.advance(1);
                Ok(Expression {
                    kind: Box::new(ExpressionKind::Constant(Constant::Boolean(true))),
                    span,
                })
            }
            TokenKind::False => {
                self.advance(1);
                Ok(Expression {
                    kind: Box::new(ExpressionKind::Constant(Constant::Boolean(false))),
                    span,
                })
            }
            TokenKind::Integer(_) => {
                let Token {
                    kind: TokenKind::Integer(i),
                    ..
                } = self.next()
                else {
                    unreachable!()
                };
                match read_dec_integer(i.as_ref()) {
                    Some(i) => Ok(Expression {
                        kind: Box::new(ExpressionKind::Constant(Constant::Integer(i))),
                        span,
                    }),
                    None => Err(ParseError {
                        kind: ParseErrorKind::BadNumber,
                        span,
                    }),
                }
            }
            TokenKind::HexInteger(_) => {
                let Token {
                    kind: TokenKind::HexInteger(i),
                    ..
                } = self.next()
                else {
                    unreachable!()
                };
                match read_hex_integer(i.as_ref()) {
                    Some(i) => Ok(Expression {
                        kind: Box::new(ExpressionKind::Constant(Constant::Integer(i))),
                        span,
                    }),
                    None => Err(ParseError {
                        kind: ParseErrorKind::BadNumber,
                        span,
                    }),
                }
            }
            TokenKind::Float(_) => {
                let Token {
                    kind: TokenKind::Float(f),
                    ..
                } = self.next()
                else {
                    unreachable!()
                };
                match read_dec_float(f.as_ref()) {
                    Some(f) => Ok(Expression {
                        kind: Box::new(ExpressionKind::Constant(Constant::Float(f))),
                        span,
                    }),
                    None => Err(ParseError {
                        kind: ParseErrorKind::BadNumber,
                        span,
                    }),
                }
            }
            TokenKind::String(_) => {
                let Token {
                    kind: TokenKind::String(s),
                    ..
                } = self.next()
                else {
                    unreachable!()
                };
                Ok(Expression {
                    kind: Box::new(ExpressionKind::Constant(Constant::String(s))),
                    span,
                })
            }
            TokenKind::Function => {
                self.advance(1);
                self.parse_token(TokenKind::LeftParen)?;

                let mut parameters = Vec::new();
                self.look_ahead(1);
                loop {
                    self.look_ahead(1);
                    if matches!(self.peek(0).kind, TokenKind::RightParen) {
                        break;
                    }

                    parameters.push(self.parse_identifier()?.0);

                    self.look_ahead(1);
                    let next = self.peek(0);
                    match &next.kind {
                        TokenKind::Comma => {
                            self.advance(1);
                        }
                        TokenKind::RightParen => {
                            break;
                        }
                        _ => {
                            return Err(ParseError {
                                kind: ParseErrorKind::Unexpected {
                                    unexpected: token_indicator(&next.kind),
                                    expected: "',' or ')'",
                                },
                                span: next.span,
                            });
                        }
                    }
                }

                self.parse_token(TokenKind::RightParen)?;
                self.parse_token(TokenKind::LeftBrace)?;
                let body = self.parse_block()?;
                span = span.combine(self.parse_token(TokenKind::RightBrace)?);

                Ok(Expression {
                    kind: Box::new(ExpressionKind::Function(FunctionExpr { parameters, body })),
                    span,
                })
            }
            TokenKind::LeftBrace => {
                let (pairs, span) = self.parse_object()?;
                Ok(Expression {
                    kind: Box::new(ExpressionKind::Object(pairs)),
                    span,
                })
            }
            TokenKind::LeftBracket => {
                let (items, span) = self.parse_array()?;
                Ok(Expression {
                    kind: Box::new(ExpressionKind::Array(items)),
                    span,
                })
            }
            _ => self.parse_suffixed_expression(),
        }
    }

    fn parse_object(&mut self) -> Result<(Vec<(S, Expression<S>)>, Span), ParseError> {
        let mut span = self.parse_token(TokenKind::LeftBrace)?;
        let mut entries = Vec::new();

        loop {
            self.look_ahead(1);
            if matches!(self.peek(0).kind, TokenKind::RightBrace) {
                break;
            }

            let key = self.parse_identifier()?.0;
            self.parse_token(TokenKind::Colon)?;
            let value = self.parse_expression()?;

            entries.push((key, value));

            self.look_ahead(1);
            let next = self.peek(0);
            match &next.kind {
                TokenKind::Comma => {
                    self.advance(1);
                }
                TokenKind::RightBrace => {
                    break;
                }
                _ => {
                    return Err(ParseError {
                        kind: ParseErrorKind::Unexpected {
                            unexpected: token_indicator(&next.kind),
                            expected: "',' or '}'",
                        },
                        span: next.span,
                    });
                }
            }
        }

        span = span.combine(self.parse_token(TokenKind::RightBrace)?);

        Ok((entries, span))
    }

    fn parse_array(&mut self) -> Result<(Vec<Expression<S>>, Span), ParseError> {
        let mut span = self.parse_token(TokenKind::LeftBracket)?;
        let mut entries = Vec::new();
        loop {
            self.look_ahead(1);
            if matches!(self.peek(0).kind, TokenKind::RightBracket) {
                break;
            }

            entries.push(self.parse_expression()?);

            self.look_ahead(1);
            let next = self.peek(0);
            match &next.kind {
                TokenKind::Comma => {
                    self.advance(1);
                }
                TokenKind::RightBracket => {
                    break;
                }
                _ => {
                    return Err(ParseError {
                        kind: ParseErrorKind::Unexpected {
                            unexpected: token_indicator(&next.kind),
                            expected: "',' or ']'",
                        },
                        span,
                    });
                }
            }
        }

        span = span.combine(self.parse_token(TokenKind::RightBracket)?);

        Ok((entries, span))
    }

    fn parse_identifier(&mut self) -> Result<(S, Span), ParseError> {
        let Token { kind, span } = self.next();
        match kind {
            TokenKind::Identifier(ident) => Ok((ident, span)),
            t => Err(ParseError {
                kind: ParseErrorKind::Unexpected {
                    unexpected: token_indicator(&t),
                    expected: "<identifier>",
                },
                span,
            }),
        }
    }

    fn parse_token(&mut self, expected: TokenKind<()>) -> Result<Span, ParseError> {
        let Token { kind, span } = self.next();

        if kind.as_string_ref().map_string(|_| ()) == expected {
            Ok(span)
        } else {
            Err(ParseError {
                kind: ParseErrorKind::Unexpected {
                    unexpected: token_indicator(&kind),
                    expected: token_indicator(&expected),
                },
                span,
            })
        }
    }

    // Look ahead `n` tokens in the lexer, making them available to peek methods.
    fn look_ahead(&mut self, n: usize) {
        while self.look_ahead_buffer.len() < n {
            match self.token_iter.next() {
                Some(token) => {
                    if matches!(token.kind, TokenKind::EndOfStream) {
                        // If our token stream has a real `EndOfStream` token, record its span so
                        // that all generated `EndOfStream` tokens will have the correct span.
                        self.end_of_stream_span = token.span;
                    }

                    // Newlines are not observed at all in the parser at the moment.
                    if !matches!(token.kind, TokenKind::Newline) {
                        self.look_ahead_buffer.push(token);
                    }
                }
                None => {
                    // If the token stream does not generate an `EndOfStream` token, the span here
                    // will be null.
                    self.look_ahead_buffer.push(Token {
                        kind: TokenKind::EndOfStream,
                        span: self.end_of_stream_span,
                    });
                }
            }
        }
    }

    // Advance the token stream `n` tokens.
    //
    // # Panics
    //
    // Panics if the given `n` is less than the number of tokens we have previously buffered with
    // `Parser::look_ahead`.
    fn advance(&mut self, n: usize) {
        self.look_ahead_buffer.drain(0..n);
    }

    // Returns a reference to the `n`th token ahead in the look-ahead token buffer if it exists,
    // along with the line number on which it is found.
    //
    // # Panics
    //
    // Panics if the given `n` is less than the number of tokens we have previously buffered with
    // `Parser::look_ahead`.
    fn peek(&self, n: usize) -> &Token<S> {
        &self.look_ahead_buffer[n]
    }

    // Return the next token in the token stream if it exists and advance the stream.
    fn next(&mut self) -> Token<S> {
        self.look_ahead(1);
        self.look_ahead_buffer.remove(0)
    }
}

fn get_unary_operator<S>(token: &TokenKind<S>) -> Option<UnaryOp> {
    match *token {
        TokenKind::Minus => Some(UnaryOp::Minus),
        TokenKind::Bang => Some(UnaryOp::Not),
        _ => None,
    }
}

fn get_binary_operator<S>(token: &TokenKind<S>) -> Option<BinaryOp> {
    match *token {
        TokenKind::Star => Some(BinaryOp::Mult),
        TokenKind::Slash => Some(BinaryOp::Div),
        TokenKind::Plus => Some(BinaryOp::Add),
        TokenKind::Minus => Some(BinaryOp::Sub),
        TokenKind::DoubleEqual => Some(BinaryOp::Equal),
        TokenKind::BangEqual => Some(BinaryOp::NotEqual),
        TokenKind::Less => Some(BinaryOp::LessThan),
        TokenKind::LessEqual => Some(BinaryOp::LessEqual),
        TokenKind::Greater => Some(BinaryOp::GreaterThan),
        TokenKind::GreaterEqual => Some(BinaryOp::GreaterEqual),
        TokenKind::DoubleAmpersand => Some(BinaryOp::And),
        TokenKind::DoublePipe => Some(BinaryOp::Or),
        _ => None,
    }
}

fn get_assignment_operator<S>(token: &TokenKind<S>) -> Option<AssignmentOp> {
    match *token {
        TokenKind::Equal => Some(AssignmentOp::Equal),
        TokenKind::PlusEqual => Some(AssignmentOp::PlusEqual),
        TokenKind::MinusEqual => Some(AssignmentOp::MinusEqual),
        TokenKind::StarEqual => Some(AssignmentOp::MultEqual),
        TokenKind::SlashEqual => Some(AssignmentOp::DivEqual),
        _ => None,
    }
}

fn token_indicator<S>(t: &TokenKind<S>) -> &'static str {
    match *t {
        TokenKind::EndOfStream => "<eof>",
        TokenKind::Newline => "\n",
        TokenKind::Macro => "#macro",
        TokenKind::LeftParen => "(",
        TokenKind::RightParen => ")",
        TokenKind::LeftBracket => "[",
        TokenKind::RightBracket => "]",
        TokenKind::LeftBrace => "{",
        TokenKind::RightBrace => "}",
        TokenKind::Colon => ":",
        TokenKind::SemiColon => ";",
        TokenKind::Comma => ",",
        TokenKind::Dot => ".",
        TokenKind::Plus => "+",
        TokenKind::Minus => "-",
        TokenKind::Bang => "!",
        TokenKind::Slash => "/",
        TokenKind::Star => "*",
        TokenKind::Mod => "mod",
        TokenKind::Div => "div",
        TokenKind::Percent => "%",
        TokenKind::Ampersand => "&",
        TokenKind::Pipe => "|",
        TokenKind::Equal => "=",
        TokenKind::PlusEqual => "+=",
        TokenKind::MinusEqual => "-=",
        TokenKind::StarEqual => "*=",
        TokenKind::SlashEqual => "/=",
        TokenKind::PercentEqual => "%=",
        TokenKind::DoubleEqual => "==",
        TokenKind::BangEqual => "!=",
        TokenKind::Less => "<",
        TokenKind::LessEqual => "<=",
        TokenKind::Greater => ">",
        TokenKind::GreaterEqual => ">=",
        TokenKind::DoublePlus => "++",
        TokenKind::DoubleMinus => "--",
        TokenKind::DoubleAmpersand => "&&",
        TokenKind::DoublePipe => "--",
        TokenKind::Enum => "enum",
        TokenKind::Function => "function",
        TokenKind::Var => "var",
        TokenKind::Switch => "switch",
        TokenKind::Case => "case",
        TokenKind::Break => "break",
        TokenKind::If => "if",
        TokenKind::Else => "else",
        TokenKind::For => "for",
        TokenKind::Repeat => "repeat",
        TokenKind::Return => "return",
        TokenKind::Exit => "exit",
        TokenKind::Undefined => "undefined",
        TokenKind::True => "true",
        TokenKind::False => "false",
        TokenKind::This => "self",
        TokenKind::Integer(_) => "<integer>",
        TokenKind::HexInteger(_) => "<hex_integer>",
        TokenKind::Float(_) => "<float>",
        TokenKind::Identifier(_) => "<identifier>",
        TokenKind::String(_) => "<string>",
    }
}

type OperatorPriority = u8;

// Priority lower than any unary or binary operator.
const MIN_PRIORITY: OperatorPriority = 0;

// Priority of all unary operators.
const UNARY_PRIORITY: OperatorPriority = 6;

// Returns the left and right priority of the given binary operator.
//
// Different left and right priorities can be used to make an operation associate leftwards
// or rightwards, if the two priorities are the same the operation will default to associating
// leftwards.
fn binary_priority(operator: BinaryOp) -> (OperatorPriority, OperatorPriority) {
    match operator {
        BinaryOp::Mult => (5, 5),
        BinaryOp::Div => (5, 5),
        BinaryOp::Add => (4, 4),
        BinaryOp::Sub => (4, 4),
        BinaryOp::NotEqual => (3, 3),
        BinaryOp::Equal => (3, 3),
        BinaryOp::LessThan => (3, 3),
        BinaryOp::LessEqual => (3, 3),
        BinaryOp::GreaterThan => (3, 3),
        BinaryOp::GreaterEqual => (3, 3),
        BinaryOp::And => (2, 2),
        BinaryOp::Or => (1, 1),
    }
}

fn read_dec_integer(s: &str) -> Option<i64> {
    let s = s.replace('_', "");
    i64::from_str_radix(&s, 10).ok()
}

fn read_hex_integer(s: &str) -> Option<i64> {
    let s = s.replace('_', "");

    let mut chars = s.chars();
    let c0 = chars.next()?;
    let c1 = chars.next()?;

    if c0 != '0' || (c1 != 'x' && c1 != 'X') || chars.as_str().is_empty() {
        return None;
    }

    i64::from_str_radix(chars.as_str(), 16).ok()
}

pub fn read_dec_float(s: &str) -> Option<f64> {
    let s = s.replace('_', "");
    str::parse(&s).ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{lexer::Lexer, string_interner::StringInterner};

    fn parse(source: &str) -> Result<Block<String>, ParseError> {
        struct SimpleInterner;

        impl StringInterner for SimpleInterner {
            type String = String;

            fn intern(&mut self, s: &str) -> Self::String {
                s.to_owned()
            }
        }

        let mut tokens = Vec::new();
        Lexer::tokenize(SimpleInterner, source, &mut tokens).unwrap();

        ParseSettings::strict().parse(tokens)
    }

    #[test]
    fn test_parser() {
        const SOURCE: &str = r#"
            // Line comment
            var sum = 0;
            for (var i = 0; i < 1000000; i += 1) {
                /*
                    Multiline comment
                */
                sum += i;
            }

            if sum > 100 {
                show_debug_message("yes");
            }

            test.foo = 1;
            test.bar = {
                a: 1,
                b: 2,
            };

            var i = 1_234;
        "#;

        parse(SOURCE).unwrap();
    }
}
