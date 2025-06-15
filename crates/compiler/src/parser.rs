use std::fmt::Debug;

use fabricator_vm::Span;
use thiserror::Error;

use crate::{
    lexer::{LexError, Lexer, Token},
    string_interner::StringInterner,
};

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
pub enum ExpressionKind<S> {
    Undefined,
    Boolean(bool),
    Float(f64),
    Integer(u64),
    String(S),
    Name(S),
    This,
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
    #[error("unexpected end of token stream, expected {expected:?}")]
    EndOfStream { expected: &'static str },
    #[error("lexer error")]
    LexError(#[from] LexError),
}

#[derive(Debug, Error)]
#[error("parse error at ({0}..{1}): {kind}", span.start(), span.end())]
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

impl Default for ParseSettings {
    fn default() -> Self {
        Self { strict: true }
    }
}

impl ParseSettings {
    pub fn parse<S>(self, source: &str, interner: S) -> Result<Block<S::String>, ParseError>
    where
        S: StringInterner,
    {
        Parser::new(self, source, interner).parse()
    }
}

struct Parser<'a, S: StringInterner> {
    settings: ParseSettings,
    lexer: Lexer<'a, S>,
    look_ahead_buffer: Vec<Option<(Token<S::String>, Span)>>,
}

impl<'a, S: StringInterner> Parser<'a, S> {
    fn new(settings: ParseSettings, source: &'a str, interner: S) -> Self {
        Parser {
            settings,
            lexer: Lexer::new(source, interner),
            look_ahead_buffer: Vec::new(),
        }
    }

    fn parse(&mut self) -> Result<Block<S::String>, ParseError> {
        let block = self.parse_block()?;

        self.look_ahead(1)?;
        if let Some((token, span)) = self.peek(0) {
            Err(ParseError {
                kind: ParseErrorKind::Unexpected {
                    unexpected: token_indicator(token),
                    expected: "<statement>",
                },
                span,
            })
        } else {
            Ok(block)
        }
    }

    fn parse_block(&mut self) -> Result<Block<S::String>, ParseError> {
        let mut statements = Vec::new();
        loop {
            self.look_ahead(1)?;
            if matches!(self.peek(0), None | Some((Token::RightBrace, _))) {
                break;
            }

            let stmt = self.parse_statement()?;

            if self.settings.strict {
                if !matches!(
                    &*stmt.kind,
                    StatementKind::If(_) | StatementKind::For(_) | StatementKind::Block(_)
                ) {
                    self.parse_token(Token::SemiColon)?;
                }
            } else {
                self.look_ahead(1)?;
                if matches!(self.peek(0), Some((Token::SemiColon, _))) {
                    self.advance(1);
                }
            }

            statements.push(stmt);
        }

        Ok(Block { statements })
    }

    fn parse_statement(&mut self) -> Result<Statement<S::String>, ParseError> {
        self.look_ahead(1)?;
        match self.peek_expected(0, "<statement>")? {
            (Token::Var, mut span) => {
                self.advance(1);
                let name = self.parse_identifier()?.0;
                self.parse_token(Token::Equal)?;
                let value = self.parse_expression()?;
                span = span.combine(value.span);
                Ok(Statement {
                    kind: Box::new(StatementKind::Var(VarStatement { name, value })),
                    span,
                })
            }
            (Token::Return, mut span) => {
                self.advance(1);
                self.look_ahead(1)?;
                let value = match self.peek(0) {
                    Some((Token::SemiColon, _)) => None,
                    None => None,
                    _ => {
                        let expr = self.parse_expression()?;
                        span = span.combine(expr.span);
                        Some(expr)
                    }
                };
                Ok(Statement {
                    kind: Box::new(StatementKind::Return(ReturnStatement { value })),
                    span,
                })
            }
            (Token::Exit, span) => {
                self.advance(1);
                Ok(Statement {
                    kind: Box::new(StatementKind::Return(ReturnStatement { value: None })),
                    span,
                })
            }
            (Token::If, mut span) => {
                self.advance(1);
                let condition = self.parse_expression()?;
                let then_stmt = self.parse_statement()?;
                span = span.combine(then_stmt.span);

                let mut else_stmt = None;

                self.look_ahead(1)?;
                if matches!(self.peek(0), Some((Token::Else, _))) {
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
            (Token::For, mut span) => {
                self.advance(1);
                self.parse_token(Token::LeftParen)?;
                let initializer = self.parse_statement()?;
                self.parse_token(Token::SemiColon)?;
                let condition = self.parse_expression()?;
                self.parse_token(Token::SemiColon)?;
                let iterator = self.parse_statement()?;
                self.parse_token(Token::RightParen)?;
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
            (Token::LeftBrace, mut span) => {
                self.advance(1);
                let block = self.parse_block()?;
                span = span.combine(self.parse_token(Token::RightBrace)?);
                Ok(Statement {
                    kind: Box::new(StatementKind::Block(block)),
                    span,
                })
            }
            (_, mut span) => {
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

                        self.look_ahead(1)?;
                        let Some(assignment_op) =
                            self.peek(0).and_then(|(t, _)| get_assignment_operator(t))
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

    fn parse_expression(&mut self) -> Result<Expression<S::String>, ParseError> {
        self.parse_sub_expression(MIN_PRIORITY)
    }

    fn parse_sub_expression(
        &mut self,
        priority_limit: OperatorPriority,
    ) -> Result<Expression<S::String>, ParseError> {
        self.look_ahead(1)?;
        let mut expr = if let Some((unary_op, mut span)) = self
            .peek(0)
            .and_then(|(t, s)| Some((get_unary_operator(t)?, s)))
        {
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

        self.look_ahead(1)?;
        while let Some(binary_op) = self.peek(0).and_then(|(t, _)| get_binary_operator(t)) {
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

    fn parse_suffixed_expression(&mut self) -> Result<Expression<S::String>, ParseError> {
        let mut expr = self.parse_primary_expression()?;
        loop {
            self.look_ahead(1)?;
            match self.peek(0) {
                Some((Token::LeftParen, _)) => {
                    self.advance(1);
                    self.look_ahead(1)?;
                    let arguments = if !matches!(self.peek(0), Some((Token::RightParen, _))) {
                        let mut expressions = Vec::new();
                        expressions.push(self.parse_expression()?);
                        self.look_ahead(1)?;
                        while matches!(self.peek(0), Some((Token::Comma, _))) {
                            self.advance(1);
                            expressions.push(self.parse_expression()?);
                            self.look_ahead(1)?;
                        }
                        expressions
                    } else {
                        Vec::new()
                    };
                    let span = expr.span.combine(self.parse_token(Token::RightParen)?);
                    expr = Expression {
                        kind: Box::new(ExpressionKind::Call(CallExpr {
                            base: expr,
                            arguments,
                        })),
                        span,
                    };
                }
                Some((Token::Dot, _)) => {
                    self.advance(1);
                    let (field, fspan) = self.parse_identifier()?;
                    let span = expr.span.combine(fspan);
                    expr = Expression {
                        kind: Box::new(ExpressionKind::Field(FieldExpr { base: expr, field })),
                        span,
                    };
                }
                Some((Token::LeftBracket, _)) => {
                    self.advance(1);
                    let index = self.parse_expression()?;
                    let span = expr.span.combine(self.parse_token(Token::RightBracket)?);
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

    fn parse_primary_expression(&mut self) -> Result<Expression<S::String>, ParseError> {
        match self.expect_next("<expression>")? {
            (Token::LeftParen, mut span) => {
                let expr = self.parse_expression()?;
                span = span.combine(self.parse_token(Token::RightParen)?);
                Ok(Expression {
                    kind: Box::new(ExpressionKind::Group(expr)),
                    span,
                })
            }
            (Token::Identifier(n), span) => Ok(Expression {
                kind: Box::new(ExpressionKind::Name(n)),
                span,
            }),
            (token, span) => Err(ParseError {
                kind: ParseErrorKind::Unexpected {
                    unexpected: token_indicator(&token),
                    expected: "<grouped expression or name>",
                },
                span,
            }),
        }
    }

    fn parse_simple_expression(&mut self) -> Result<Expression<S::String>, ParseError> {
        self.look_ahead(1)?;
        match self.peek_expected(0, "<expression>")? {
            (Token::Undefined, span) => {
                self.advance(1);
                Ok(Expression {
                    kind: Box::new(ExpressionKind::Undefined),
                    span,
                })
            }
            (Token::True, span) => {
                self.advance(1);
                Ok(Expression {
                    kind: Box::new(ExpressionKind::Boolean(true)),
                    span,
                })
            }
            (Token::False, span) => {
                self.advance(1);
                Ok(Expression {
                    kind: Box::new(ExpressionKind::Boolean(false)),
                    span,
                })
            }
            (&Token::Float(f), span) => {
                self.advance(1);
                Ok(Expression {
                    kind: Box::new(ExpressionKind::Float(f)),
                    span,
                })
            }
            (&Token::Integer(i), span) => {
                self.advance(1);
                Ok(Expression {
                    kind: Box::new(ExpressionKind::Integer(i)),
                    span,
                })
            }
            (Token::String(_), _) => {
                let Some((Token::String(s), span)) = self.next().unwrap() else {
                    unreachable!()
                };
                Ok(Expression {
                    kind: Box::new(ExpressionKind::String(s)),
                    span,
                })
            }
            (Token::This, span) => {
                self.advance(1);
                Ok(Expression {
                    kind: Box::new(ExpressionKind::This),
                    span,
                })
            }
            (Token::Function, mut span) => {
                self.advance(1);
                self.parse_token(Token::LeftParen)?;
                let mut parameters = Vec::new();
                self.look_ahead(1)?;
                if !matches!(self.peek(0), Some((Token::RightParen, _))) {
                    loop {
                        parameters.push(self.parse_identifier()?.0);
                        self.look_ahead(1)?;
                        if matches!(self.peek(0), Some((Token::Comma, _))) {
                            self.advance(1);
                        } else {
                            break;
                        }
                    }
                }
                self.parse_token(Token::RightParen)?;
                self.parse_token(Token::LeftBrace)?;
                let body = self.parse_block()?;
                span = span.combine(self.parse_token(Token::RightBrace)?);

                Ok(Expression {
                    kind: Box::new(ExpressionKind::Function(FunctionExpr { parameters, body })),
                    span,
                })
            }
            (Token::LeftBrace, _) => {
                let (pairs, span) = self.parse_object()?;
                Ok(Expression {
                    kind: Box::new(ExpressionKind::Object(pairs)),
                    span,
                })
            }
            (Token::LeftBracket, _) => {
                let (items, span) = self.parse_array()?;
                Ok(Expression {
                    kind: Box::new(ExpressionKind::Array(items)),
                    span,
                })
            }
            _ => self.parse_suffixed_expression(),
        }
    }

    fn parse_object(
        &mut self,
    ) -> Result<(Vec<(S::String, Expression<S::String>)>, Span), ParseError> {
        let mut span = self.parse_token(Token::LeftBrace)?;
        let mut entries = Vec::new();

        loop {
            self.look_ahead(1)?;
            if let Some((Token::RightBrace, espan)) = self.peek(0) {
                span = span.combine(espan);
                self.advance(1);
                break;
            }

            let key = self.parse_identifier()?.0;
            self.parse_token(Token::Colon)?;
            let value = self.parse_expression()?;

            entries.push((key, value));

            self.look_ahead(1)?;
            match self.peek(0) {
                Some((Token::Comma, _)) => {
                    self.advance(1);
                }
                Some((Token::RightBrace, espan)) => {
                    span = span.combine(espan);
                    self.advance(1);
                    break;
                }
                Some((t, span)) => {
                    return Err(ParseError {
                        kind: ParseErrorKind::Unexpected {
                            unexpected: token_indicator(t),
                            expected: "',' or '}'",
                        },
                        span,
                    });
                }
                None => {
                    return Err(ParseError {
                        kind: ParseErrorKind::EndOfStream {
                            expected: "',' or '}'",
                        },
                        span: Span::empty(self.lexer.position()),
                    });
                }
            }
        }

        Ok((entries, span))
    }

    fn parse_array(&mut self) -> Result<(Vec<Expression<S::String>>, Span), ParseError> {
        let mut span = self.parse_token(Token::LeftBracket)?;
        let mut entries = Vec::new();
        loop {
            self.look_ahead(1)?;
            if let Some((Token::RightBracket, espan)) = self.peek(0) {
                span = span.combine(espan);
                self.advance(1);
                break;
            }

            let value = self.parse_expression()?;
            entries.push(value);

            self.look_ahead(1)?;
            match self.peek(0) {
                Some((Token::Comma, _)) => {
                    self.advance(1);
                }
                Some((Token::RightBracket, espan)) => {
                    span = span.combine(espan);
                    self.advance(1);
                    break;
                }
                Some((t, span)) => {
                    return Err(ParseError {
                        kind: ParseErrorKind::Unexpected {
                            unexpected: token_indicator(t),
                            expected: "',' or ']'",
                        },
                        span,
                    });
                }
                None => {
                    return Err(ParseError {
                        kind: ParseErrorKind::EndOfStream {
                            expected: "',' or ']'",
                        },
                        span: Span::empty(self.lexer.position()),
                    });
                }
            }
        }

        Ok((entries, span))
    }

    fn parse_identifier(&mut self) -> Result<(S::String, Span), ParseError> {
        match self.next()? {
            Some((Token::Identifier(ident), span)) => Ok((ident, span)),
            Some((t, span)) => Err(ParseError {
                kind: ParseErrorKind::Unexpected {
                    unexpected: token_indicator(&t),
                    expected: "<identifier>",
                },
                span,
            }),
            None => Err(ParseError {
                kind: ParseErrorKind::EndOfStream {
                    expected: "<identifier>",
                },
                span: Span::empty(self.lexer.position()),
            }),
        }
    }

    fn parse_token(&mut self, expected: Token<&'static str>) -> Result<Span, ParseError> {
        if let Some((next, span)) = self.next()? {
            if next.as_str() == expected {
                Ok(span)
            } else {
                Err(ParseError {
                    kind: ParseErrorKind::Unexpected {
                        unexpected: token_indicator(&next),
                        expected: token_indicator(&expected),
                    },
                    span,
                })
            }
        } else {
            Err(ParseError {
                kind: ParseErrorKind::EndOfStream {
                    expected: token_indicator(&expected),
                },
                span: Span::empty(self.lexer.position()),
            })
        }
    }

    // Look ahead `n` tokens in the lexer, making them available to peek methods.
    fn look_ahead(&mut self, n: usize) -> Result<(), ParseError> {
        while self.look_ahead_buffer.len() < n {
            self.lexer.skip_whitespace();
            if let Some((token, span)) = self.lexer.read_token().map_err(|e| ParseError {
                kind: ParseErrorKind::LexError(e),
                span: Span::empty(self.lexer.position()),
            })? {
                self.look_ahead_buffer.push(Some((token, span)));
            } else {
                self.look_ahead_buffer.push(None);
            }
        }
        Ok(())
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
    fn peek(&self, n: usize) -> Option<(&Token<S::String>, Span)> {
        self.look_ahead_buffer[n].as_ref().map(|(t, ln)| (t, *ln))
    }

    // Equivalent to `Parser::peek`, except if the the `n`th token ahead in the token stream doesn't
    // exist produces a `ParseErrorKind::EndOfStream` error.
    //
    // # Panics
    //
    // Panics if the given `n` is less than the number of tokens we have previously buffered with
    // `Parser::look_ahead`.
    fn peek_expected(
        &self,
        n: usize,
        expected: &'static str,
    ) -> Result<(&Token<S::String>, Span), ParseError> {
        self.peek(n).ok_or_else(|| ParseError {
            kind: ParseErrorKind::EndOfStream { expected },
            span: Span::empty(self.lexer.position()),
        })
    }

    // Return the next token in the token stream if it exists and advance the stream.
    fn next(&mut self) -> Result<Option<(Token<S::String>, Span)>, ParseError> {
        self.look_ahead(1)?;
        Ok(self.look_ahead_buffer.remove(0))
    }

    // Return the next token in the token stream, producing a `ParseErrorKind::EndOfStream` error if
    // it doesn't exist.
    fn expect_next(
        &mut self,
        expected: &'static str,
    ) -> Result<(Token<S::String>, Span), ParseError> {
        if let Some((token, span)) = self.next()? {
            Ok((token, span))
        } else {
            Err(ParseError {
                kind: ParseErrorKind::EndOfStream { expected },
                span: Span::empty(self.lexer.position()),
            })
        }
    }
}

fn get_unary_operator<S>(token: &Token<S>) -> Option<UnaryOp> {
    match *token {
        Token::Minus => Some(UnaryOp::Minus),
        Token::Bang => Some(UnaryOp::Not),
        _ => None,
    }
}

fn get_binary_operator<S>(token: &Token<S>) -> Option<BinaryOp> {
    match *token {
        Token::Star => Some(BinaryOp::Mult),
        Token::Slash => Some(BinaryOp::Div),
        Token::Plus => Some(BinaryOp::Add),
        Token::Minus => Some(BinaryOp::Sub),
        Token::DoubleEqual => Some(BinaryOp::Equal),
        Token::BangEqual => Some(BinaryOp::NotEqual),
        Token::Less => Some(BinaryOp::LessThan),
        Token::LessEqual => Some(BinaryOp::LessEqual),
        Token::Greater => Some(BinaryOp::GreaterThan),
        Token::GreaterEqual => Some(BinaryOp::GreaterEqual),
        Token::DoubleAmpersand => Some(BinaryOp::And),
        Token::DoublePipe => Some(BinaryOp::Or),
        _ => None,
    }
}

fn get_assignment_operator<S>(token: &Token<S>) -> Option<AssignmentOp> {
    match *token {
        Token::Equal => Some(AssignmentOp::Equal),
        Token::PlusEqual => Some(AssignmentOp::PlusEqual),
        Token::MinusEqual => Some(AssignmentOp::MinusEqual),
        Token::StarEqual => Some(AssignmentOp::MultEqual),
        Token::SlashEqual => Some(AssignmentOp::DivEqual),
        _ => None,
    }
}

fn token_indicator<S>(t: &Token<S>) -> &'static str {
    match *t {
        Token::LeftParen => "(",
        Token::RightParen => ")",
        Token::LeftBracket => "[",
        Token::RightBracket => "]",
        Token::LeftBrace => "{",
        Token::RightBrace => "}",
        Token::Colon => ":",
        Token::SemiColon => ";",
        Token::Comma => ",",
        Token::Dot => ".",
        Token::Plus => "+",
        Token::Minus => "-",
        Token::Bang => "!",
        Token::Slash => "/",
        Token::Star => "*",
        Token::Mod => "mod",
        Token::Div => "div",
        Token::Percent => "%",
        Token::Ampersand => "&",
        Token::Pipe => "|",
        Token::Equal => "=",
        Token::PlusEqual => "+=",
        Token::MinusEqual => "-=",
        Token::StarEqual => "*=",
        Token::SlashEqual => "/=",
        Token::PercentEqual => "%=",
        Token::DoubleEqual => "==",
        Token::BangEqual => "!=",
        Token::Less => "<",
        Token::LessEqual => "<=",
        Token::Greater => ">",
        Token::GreaterEqual => ">=",
        Token::DoublePlus => "++",
        Token::DoubleMinus => "--",
        Token::DoubleAmpersand => "&&",
        Token::DoublePipe => "--",
        Token::Var => "var",
        Token::Function => "function",
        Token::Switch => "switch",
        Token::Case => "case",
        Token::Break => "break",
        Token::If => "if",
        Token::Else => "else",
        Token::For => "for",
        Token::Repeat => "repeat",
        Token::Return => "return",
        Token::Exit => "exit",
        Token::Undefined => "undefined",
        Token::True => "true",
        Token::False => "false",
        Token::This => "self",
        Token::Integer(_) => "<integer>",
        Token::Float(_) => "<float>",
        Token::Identifier(_) => "<identifier>",
        Token::String(_) => "<string>",
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

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(source: &str) -> Result<Block<String>, ParseError> {
        struct SimpleInterner;

        impl StringInterner for SimpleInterner {
            type String = String;

            fn intern(&mut self, s: &str) -> Self::String {
                s.to_owned()
            }
        }

        ParseSettings::default().parse(source, SimpleInterner)
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
        "#;

        parse(SOURCE).unwrap();
    }
}
