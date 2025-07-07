use std::fmt::Debug;

use arrayvec::ArrayVec;
use fabricator_vm::Span;
use thiserror::Error;

use crate::{
    ast,
    constant::Constant,
    tokens::{Token, TokenKind},
};

#[derive(Debug, Error)]
pub enum ParseErrorKind {
    #[error("found {unexpected:?}, expected {expected:?}")]
    Unexpected {
        unexpected: &'static str,
        expected: &'static str,
    },
    #[error("invalid numeric literal")]
    BadNumber,
    #[error("function declarations with inheritance must be annotated with `constructor`")]
    InheritWithoutConstructor,
    #[error("parser settings forbid `constructor` annotations and `new` on call expressions")]
    ConstructorsDisallowed,
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
    pub strict_semicolons: bool,
    /// Allow `constructor` annotated functions and `new` before function call expressions.
    pub allow_constructors: bool,
}

impl ParseSettings {
    pub fn strict() -> Self {
        ParseSettings {
            strict_semicolons: true,
            allow_constructors: false,
        }
    }

    pub fn compat() -> Self {
        ParseSettings {
            strict_semicolons: false,
            allow_constructors: true,
        }
    }

    pub fn parse<I, S>(self, token_iter: I) -> Result<ast::Block<S>, ParseError>
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

    fn parse(&mut self) -> Result<ast::Block<S>, ParseError> {
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

    fn parse_block(&mut self) -> Result<ast::Block<S>, ParseError> {
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

            if self.settings.strict_semicolons {
                if !matches!(
                    &*stmt.kind,
                    ast::StatementKind::Block(_)
                        | ast::StatementKind::Enum(_)
                        | ast::StatementKind::Function(_)
                        | ast::StatementKind::If(_)
                        | ast::StatementKind::For(_)
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

        Ok(ast::Block { statements })
    }

    fn parse_statement(&mut self) -> Result<ast::Statement<S>, ParseError> {
        self.look_ahead(1);
        let &Token { ref kind, mut span } = self.peek(0);
        match kind {
            TokenKind::Enum => {
                let (stmt, span) = self.parse_enum_statement()?;
                Ok(ast::Statement {
                    kind: Box::new(ast::StatementKind::Enum(stmt)),
                    span,
                })
            }
            TokenKind::Function => {
                self.advance(1);
                let name = self.parse_identifier()?.0;
                let parameters = self.parse_parameter_list()?.0;

                self.look_ahead(1);
                let inherit;
                if matches!(self.peek(0).kind, TokenKind::Colon) {
                    self.advance(1);
                    let expr = self.parse_expression()?;

                    let ast::ExpressionKind::Call(expr) = *expr.kind else {
                        return Err(ParseError {
                            kind: ParseErrorKind::Unexpected {
                                unexpected: "<expression>",
                                expected: "<call expression>",
                            },
                            span: expr.span,
                        });
                    };

                    inherit = Some(expr);
                } else {
                    inherit = None;
                }

                self.look_ahead(1);
                let is_constructor = if matches!(self.peek(0).kind, TokenKind::Constructor) {
                    self.advance(1);
                    true
                } else {
                    false
                };

                self.parse_token(TokenKind::LeftBrace)?;
                let body = self.parse_block()?;
                span = span.combine(self.parse_token(TokenKind::RightBrace)?);

                if !is_constructor && inherit.is_some() {
                    return Err(ParseError {
                        kind: ParseErrorKind::InheritWithoutConstructor,
                        span,
                    });
                }

                if is_constructor && !self.settings.allow_constructors {
                    return Err(ParseError {
                        kind: ParseErrorKind::ConstructorsDisallowed,
                        span,
                    });
                }

                Ok(ast::Statement {
                    kind: Box::new(ast::StatementKind::Function(ast::FunctionStatement {
                        name,
                        is_constructor,
                        inherit,
                        parameters,
                        body,
                    })),
                    span,
                })
            }
            TokenKind::Var => {
                self.advance(1);
                let name = self.parse_identifier()?.0;
                self.parse_token(TokenKind::Equal)?;
                let value = self.parse_expression()?;
                span = span.combine(value.span);
                Ok(ast::Statement {
                    kind: Box::new(ast::StatementKind::Var(ast::Declaration { name, value })),
                    span,
                })
            }
            TokenKind::Static => {
                self.advance(1);
                let name = self.parse_identifier()?.0;
                self.parse_token(TokenKind::Equal)?;
                let value = self.parse_expression()?;
                span = span.combine(value.span);
                Ok(ast::Statement {
                    kind: Box::new(ast::StatementKind::Static(ast::Declaration { name, value })),
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
                Ok(ast::Statement {
                    kind: Box::new(ast::StatementKind::Return(ast::ReturnStatement { value })),
                    span,
                })
            }
            TokenKind::Exit => {
                self.advance(1);
                Ok(ast::Statement {
                    kind: Box::new(ast::StatementKind::Return(ast::ReturnStatement {
                        value: None,
                    })),
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

                Ok(ast::Statement {
                    kind: Box::new(ast::StatementKind::If(ast::IfStatement {
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
                Ok(ast::Statement {
                    kind: Box::new(ast::StatementKind::For(ast::ForStatement {
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
                Ok(ast::Statement {
                    kind: Box::new(ast::StatementKind::Block(block)),
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
                    ast::ExpressionKind::Call(call) => ast::StatementKind::Call(call),
                    expr @ (ast::ExpressionKind::Name(_)
                    | ast::ExpressionKind::Field(_)
                    | ast::ExpressionKind::Index(_)) => {
                        let target = match expr {
                            ast::ExpressionKind::Name(name) => ast::AssignmentTarget::Name(name),
                            ast::ExpressionKind::Field(field) => {
                                ast::AssignmentTarget::Field(field)
                            }
                            ast::ExpressionKind::Index(index) => {
                                ast::AssignmentTarget::Index(index)
                            }
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

                        ast::StatementKind::Assignment(ast::AssignmentStatement {
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

                Ok(ast::Statement {
                    kind: Box::new(kind),
                    span,
                })
            }
        }
    }

    fn parse_enum_statement(&mut self) -> Result<(ast::EnumStatement<S>, Span), ParseError> {
        let mut span = self.parse_token(TokenKind::Enum)?;
        let name = self.parse_identifier()?.0;

        let mut variants = Vec::new();

        span = span.combine(self.parse_comma_separated_list(
            TokenKind::LeftBrace,
            TokenKind::RightBrace,
            |this| {
                let key = this.parse_identifier()?.0;

                this.look_ahead(1);
                let value = if matches!(this.peek(0).kind, TokenKind::Equal) {
                    this.advance(1);
                    Some(this.parse_expression()?)
                } else {
                    None
                };

                variants.push((key, value));

                Ok(())
            },
        )?);

        Ok((ast::EnumStatement { name, variants }, span))
    }

    fn parse_expression(&mut self) -> Result<ast::Expression<S>, ParseError> {
        self.parse_sub_expression(MIN_PRIORITY)
    }

    fn parse_sub_expression(
        &mut self,
        priority_limit: OperatorPriority,
    ) -> Result<ast::Expression<S>, ParseError> {
        self.look_ahead(1);
        let next = self.peek(0);
        let mut expr = if let Some(unary_op) = get_unary_operator(&next.kind) {
            let mut span = next.span;
            self.advance(1);
            let target = self.parse_sub_expression(UNARY_PRIORITY)?;
            span = span.combine(target.span);
            ast::Expression {
                kind: Box::new(ast::ExpressionKind::Unary(unary_op, target)),
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
            expr = ast::Expression {
                kind: Box::new(ast::ExpressionKind::Binary(
                    expr,
                    binary_op,
                    right_expression,
                )),
                span,
            };
        }

        Ok(expr)
    }

    fn parse_suffixed_expression(&mut self) -> Result<ast::Expression<S>, ParseError> {
        let mut expr = self.parse_primary_expression()?;
        loop {
            self.look_ahead(1);
            match &self.peek(0).kind {
                TokenKind::LeftParen => {
                    let mut arguments = Vec::new();

                    let span = expr.span.combine(self.parse_comma_separated_list(
                        TokenKind::LeftParen,
                        TokenKind::RightParen,
                        |this| {
                            arguments.push(this.parse_expression()?);
                            Ok(())
                        },
                    )?);

                    expr = ast::Expression {
                        kind: Box::new(ast::ExpressionKind::Call(ast::CallExpr {
                            base: expr,
                            arguments,
                            has_new: false,
                        })),
                        span,
                    };
                }
                TokenKind::Dot => {
                    self.advance(1);
                    let (field, fspan) = self.parse_identifier()?;
                    let span = expr.span.combine(fspan);
                    expr = ast::Expression {
                        kind: Box::new(ast::ExpressionKind::Field(ast::FieldExpr {
                            base: expr,
                            field,
                        })),
                        span,
                    };
                }
                TokenKind::LeftBracket => {
                    self.advance(1);
                    let index = self.parse_expression()?;
                    let span = expr
                        .span
                        .combine(self.parse_token(TokenKind::RightBracket)?);
                    expr = ast::Expression {
                        kind: Box::new(ast::ExpressionKind::Index(ast::IndexExpr {
                            base: expr,
                            index,
                        })),
                        span,
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_primary_expression(&mut self) -> Result<ast::Expression<S>, ParseError> {
        let Token { kind, mut span } = self.next();
        match kind {
            TokenKind::LeftParen => {
                let expr = self.parse_expression()?;
                span = span.combine(self.parse_token(TokenKind::RightParen)?);
                Ok(ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Group(expr)),
                    span,
                })
            }
            TokenKind::Identifier(n) => Ok(ast::Expression {
                kind: Box::new(ast::ExpressionKind::Name(n)),
                span,
            }),
            TokenKind::Global => Ok(ast::Expression {
                kind: Box::new(ast::ExpressionKind::Global),
                span,
            }),
            TokenKind::This => Ok(ast::Expression {
                kind: Box::new(ast::ExpressionKind::This),
                span,
            }),
            TokenKind::Other => Ok(ast::Expression {
                kind: Box::new(ast::ExpressionKind::Other),
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

    fn parse_simple_expression(&mut self) -> Result<ast::Expression<S>, ParseError> {
        self.look_ahead(1);
        let &Token { ref kind, mut span } = self.peek(0);
        match kind {
            TokenKind::Undefined => {
                self.advance(1);
                Ok(ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Constant(Constant::Undefined)),
                    span,
                })
            }
            TokenKind::True => {
                self.advance(1);
                Ok(ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Constant(Constant::Boolean(true))),
                    span,
                })
            }
            TokenKind::False => {
                self.advance(1);
                Ok(ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Constant(Constant::Boolean(false))),
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
                    Some(i) => Ok(ast::Expression {
                        kind: Box::new(ast::ExpressionKind::Constant(Constant::Integer(i))),
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
                    Some(i) => Ok(ast::Expression {
                        kind: Box::new(ast::ExpressionKind::Constant(Constant::Integer(i))),
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
                    Some(f) => Ok(ast::Expression {
                        kind: Box::new(ast::ExpressionKind::Constant(Constant::Float(f))),
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
                Ok(ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Constant(Constant::String(s))),
                    span,
                })
            }
            TokenKind::Function => {
                self.advance(1);

                let parameters = self.parse_parameter_list()?.0;

                self.parse_token(TokenKind::LeftBrace)?;
                let body = self.parse_block()?;
                span = span.combine(self.parse_token(TokenKind::RightBrace)?);

                Ok(ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Function(ast::FunctionExpr {
                        parameters,
                        body,
                    })),
                    span,
                })
            }
            TokenKind::New => {
                self.advance(1);

                let mut expr = self.parse_suffixed_expression()?;
                expr.span = span.combine(expr.span);

                match &mut *expr.kind {
                    ast::ExpressionKind::Call(call_expr) => {
                        if !self.settings.allow_constructors {
                            return Err(ParseError {
                                kind: ParseErrorKind::ConstructorsDisallowed,
                                span: expr.span,
                            });
                        }

                        call_expr.has_new = true;
                        Ok(expr)
                    }
                    _ => {
                        return Err(ParseError {
                            kind: ParseErrorKind::Unexpected {
                                unexpected: "<suffixed expression>",
                                expected: "<call expression>",
                            },
                            span: expr.span,
                        });
                    }
                }
            }
            TokenKind::LeftBrace => {
                let (pairs, span) = self.parse_object()?;
                Ok(ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Object(pairs)),
                    span,
                })
            }
            TokenKind::LeftBracket => {
                let (items, span) = self.parse_array()?;
                Ok(ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Array(items)),
                    span,
                })
            }
            _ => self.parse_suffixed_expression(),
        }
    }

    fn parse_object(&mut self) -> Result<(Vec<(S, ast::Expression<S>)>, Span), ParseError> {
        let mut entries = Vec::new();

        let span =
            self.parse_comma_separated_list(TokenKind::LeftBrace, TokenKind::RightBrace, |this| {
                let key = this.parse_identifier()?.0;
                this.parse_token(TokenKind::Colon)?;
                let value = this.parse_expression()?;

                entries.push((key, value));

                Ok(())
            })?;

        Ok((entries, span))
    }

    fn parse_array(&mut self) -> Result<(Vec<ast::Expression<S>>, Span), ParseError> {
        let mut entries = Vec::new();

        let span = self.parse_comma_separated_list(
            TokenKind::LeftBracket,
            TokenKind::RightBracket,
            |this| {
                entries.push(this.parse_expression()?);
                Ok(())
            },
        )?;

        Ok((entries, span))
    }

    fn parse_parameter_list(&mut self) -> Result<(Vec<ast::Parameter<S>>, Span), ParseError> {
        let mut parameters = Vec::new();
        let span =
            self.parse_comma_separated_list(TokenKind::LeftParen, TokenKind::RightParen, |this| {
                let name = this.parse_identifier()?.0;

                this.look_ahead(1);
                let default = if matches!(this.peek(0).kind, TokenKind::Equal) {
                    this.advance(1);
                    Some(this.parse_expression()?)
                } else {
                    None
                };

                parameters.push(ast::Parameter { name, default });
                Ok(())
            })?;

        Ok((parameters, span))
    }

    /// Parse a comma separated list of items surrounded by paired left / right delimiters.
    ///
    /// Takes a callback to parse whatever the *item* is.
    fn parse_comma_separated_list(
        &mut self,
        left_delimiter: TokenKind<()>,
        right_delimiter: TokenKind<()>,
        mut read_item: impl FnMut(&mut Self) -> Result<(), ParseError>,
    ) -> Result<Span, ParseError> {
        let mut span = self.parse_token(left_delimiter)?;

        let is_right_delimiter =
            |kind: &TokenKind<S>| kind.as_string_ref().map_string(|_| ()) == right_delimiter;

        loop {
            self.look_ahead(1);
            if is_right_delimiter(&self.peek(0).kind) {
                break;
            }

            read_item(self)?;

            self.look_ahead(1);
            let next = self.peek(0);
            match &next.kind {
                TokenKind::Comma => {
                    self.advance(1);
                }
                kind if is_right_delimiter(kind) => {
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

        span = span.combine(self.parse_token(right_delimiter)?);

        Ok(span)
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

fn get_unary_operator<S>(token: &TokenKind<S>) -> Option<ast::UnaryOp> {
    match *token {
        TokenKind::Minus => Some(ast::UnaryOp::Minus),
        TokenKind::Bang => Some(ast::UnaryOp::Not),
        _ => None,
    }
}

fn get_binary_operator<S>(token: &TokenKind<S>) -> Option<ast::BinaryOp> {
    match *token {
        TokenKind::Star => Some(ast::BinaryOp::Mult),
        TokenKind::Slash => Some(ast::BinaryOp::Div),
        TokenKind::Plus => Some(ast::BinaryOp::Add),
        TokenKind::Minus => Some(ast::BinaryOp::Sub),
        TokenKind::DoubleEqual => Some(ast::BinaryOp::Equal),
        TokenKind::BangEqual => Some(ast::BinaryOp::NotEqual),
        TokenKind::Less => Some(ast::BinaryOp::LessThan),
        TokenKind::LessEqual => Some(ast::BinaryOp::LessEqual),
        TokenKind::Greater => Some(ast::BinaryOp::GreaterThan),
        TokenKind::GreaterEqual => Some(ast::BinaryOp::GreaterEqual),
        TokenKind::DoubleAmpersand => Some(ast::BinaryOp::And),
        TokenKind::DoublePipe => Some(ast::BinaryOp::Or),
        _ => None,
    }
}

fn get_assignment_operator<S>(token: &TokenKind<S>) -> Option<ast::AssignmentOp> {
    match *token {
        TokenKind::Equal => Some(ast::AssignmentOp::Equal),
        TokenKind::PlusEqual => Some(ast::AssignmentOp::PlusEqual),
        TokenKind::MinusEqual => Some(ast::AssignmentOp::MinusEqual),
        TokenKind::StarEqual => Some(ast::AssignmentOp::MultEqual),
        TokenKind::SlashEqual => Some(ast::AssignmentOp::DivEqual),
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
        TokenKind::Tilde => "~",
        TokenKind::QuestionMark => "?",
        TokenKind::Octothorpe => "#",
        TokenKind::AtSign => "@",
        TokenKind::Dollar => "$",
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
        TokenKind::Constructor => "constructor",
        TokenKind::Var => "var",
        TokenKind::Static => "static",
        TokenKind::Switch => "switch",
        TokenKind::Case => "case",
        TokenKind::Break => "break",
        TokenKind::If => "if",
        TokenKind::Else => "else",
        TokenKind::For => "for",
        TokenKind::Repeat => "repeat",
        TokenKind::While => "While",
        TokenKind::With => "With",
        TokenKind::Return => "return",
        TokenKind::Exit => "exit",
        TokenKind::Undefined => "undefined",
        TokenKind::True => "true",
        TokenKind::False => "false",
        TokenKind::Global => "global",
        TokenKind::This => "self",
        TokenKind::Other => "other",
        TokenKind::New => "new",
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
fn binary_priority(operator: ast::BinaryOp) -> (OperatorPriority, OperatorPriority) {
    match operator {
        ast::BinaryOp::Mult => (5, 5),
        ast::BinaryOp::Div => (5, 5),
        ast::BinaryOp::Add => (4, 4),
        ast::BinaryOp::Sub => (4, 4),
        ast::BinaryOp::NotEqual => (3, 3),
        ast::BinaryOp::Equal => (3, 3),
        ast::BinaryOp::LessThan => (3, 3),
        ast::BinaryOp::LessEqual => (3, 3),
        ast::BinaryOp::GreaterThan => (3, 3),
        ast::BinaryOp::GreaterEqual => (3, 3),
        ast::BinaryOp::And => (2, 2),
        ast::BinaryOp::Or => (1, 1),
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

    fn parse(settings: ParseSettings, source: &str) -> Result<ast::Block<String>, ParseError> {
        struct SimpleInterner;

        impl StringInterner for SimpleInterner {
            type String = String;

            fn intern(&mut self, s: &str) -> Self::String {
                s.to_owned()
            }
        }

        let mut tokens = Vec::new();
        Lexer::tokenize(SimpleInterner, source, &mut tokens).unwrap();

        settings.parse(tokens)
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
            var j = new Foo();
        "#;

        parse(ParseSettings::compat(), SOURCE).unwrap();
    }
}
