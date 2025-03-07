use thiserror::Error;

use crate::{
    lexer::{LexError, Lexer, LineNumber, Token},
    string_interner::StringInterner,
};

#[derive(Debug, Clone)]
pub struct Block<S> {
    pub statements: Vec<Statement<S>>,
}

#[derive(Debug, Clone)]
pub enum Statement<S> {
    For(ForStatement<S>),
    Var(VarStatement<S>),
    Assignment(AssignmentStatement<S>),
    Block(Block<S>),
}

#[derive(Debug, Clone)]
pub struct ForStatement<S> {
    pub initializer: Box<Statement<S>>,
    pub condition: Expression<S>,
    pub iterator: Box<Statement<S>>,
    pub body: Block<S>,
}

#[derive(Debug, Clone)]
pub struct VarStatement<S> {
    pub name: S,
    pub value: Expression<S>,
}

#[derive(Debug, Clone)]
pub struct AssignmentStatement<S> {
    pub name: S,
    pub op: AssignmentOperator,
    pub value: Expression<S>,
}

#[derive(Debug, Clone)]
pub enum Expression<S> {
    Undefined,
    True,
    False,
    Float(f64),
    Integer(u64),
    String(S),
    Name(S),
    Group(Box<Expression<S>>),
    Call {
        base: Box<Expression<S>>,
        arguments: Vec<Expression<S>>,
    },
    Unary(UnaryOperator, Box<Expression<S>>),
    Binary(Box<Expression<S>>, BinaryOperator, Box<Expression<S>>),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum UnaryOperator {
    Not,
    Minus,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mult,
    Div,
    And,
    Or,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum AssignmentOperator {
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
    #[error("recursion limit reached")]
    RecursionLimit,
    #[error("lexer error")]
    LexError(#[from] LexError),
}

#[derive(Debug, Error)]
#[error("parse error at line {line_number}: {kind}")]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub line_number: LineNumber,
}

pub fn parse_block<S>(source: &str, interner: S) -> Result<Block<S::String>, ParseError>
where
    S: StringInterner,
{
    Parser {
        lexer: Lexer::new(source, interner),
        peek_buffer: Vec::new(),
    }
    .parse_block()
}

struct Parser<'a, S: StringInterner> {
    lexer: Lexer<'a, S>,
    peek_buffer: Vec<(LineNumber, Token<S::String>)>,
}

impl<'a, S: StringInterner> Parser<'a, S> {
    fn parse_block(&mut self) -> Result<Block<S::String>, ParseError> {
        let mut statements = Vec::new();
        while self.peek(0)?.is_some() {
            statements.push(self.parse_statement()?);
            if matches!(self.peek(0)?, Some(Token::SemiColon)) {
                self.advance(1);
            }
        }
        Ok(Block { statements })
    }

    fn parse_statement(&mut self) -> Result<Statement<S::String>, ParseError> {
        let (line_number, token) = self.expect_next("statement")?;
        let next = self.peek(0)?;
        match token {
            Token::For => {
                self.parse_token(Token::LeftParen)?;
                let initializer = self.parse_statement()?;
                self.parse_token(Token::SemiColon)?;
                let condition = self.parse_expression()?;
                self.parse_token(Token::SemiColon)?;
                let iterator = self.parse_statement()?;
                let body = self.parse_block()?;
                Ok(Statement::For(ForStatement {
                    initializer: Box::new(initializer),
                    condition,
                    iterator: Box::new(iterator),
                    body,
                }))
            }
            Token::Var => {
                let name = self.parse_identifier()?;
                self.parse_token(Token::Equal)?;
                let value = self.parse_expression()?;
                Ok(Statement::Var(VarStatement { name, value }))
            }
            Token::LeftBrace => {
                let block = self.parse_block()?;
                self.parse_token(Token::RightBrace)?;
                Ok(Statement::Block(block))
            }
            Token::Identifier(name) if matches!(next, Some(Token::Equal)) => {
                let (line_number, assignment) = self.expect_next("assignment")?;
                let Some(op) = get_assignment_operator(&assignment) else {
                    return Err(ParseError {
                        kind: ParseErrorKind::Unexpected {
                            unexpected: token_indicator(&assignment),
                            expected: "assignment",
                        },
                        line_number,
                    });
                };
                let value = self.parse_expression()?;
                Ok(Statement::Assignment(AssignmentStatement {
                    name,
                    op,
                    value,
                }))
            }
            t => Err(ParseError {
                kind: ParseErrorKind::Unexpected {
                    unexpected: token_indicator(&t),
                    expected: "statement",
                },
                line_number,
            }),
        }
    }

    fn parse_expression(&mut self) -> Result<Expression<S::String>, ParseError> {
        self.parse_sub_expression(MIN_PRIORITY)
    }

    fn parse_sub_expression(
        &mut self,
        priority_limit: Priority,
    ) -> Result<Expression<S::String>, ParseError> {
        let mut expr = if let Some(unary_op) = self.peek(0)?.and_then(get_unary_operator) {
            self.advance(1);
            Expression::Unary(
                unary_op,
                Box::new(self.parse_sub_expression(UNARY_PRIORITY)?),
            )
        } else {
            self.parse_simple_expression()?
        };

        while let Some(binary_op) = self.peek(0)?.and_then(|t| get_binary_operator(t)) {
            let (left_priority, right_priority) = binary_priority(binary_op);
            if left_priority <= priority_limit {
                break;
            }

            self.advance(1);
            let right_expression = self.parse_sub_expression(right_priority)?;
            expr = Expression::Binary(Box::new(expr), binary_op, Box::new(right_expression));
        }

        Ok(expr)
    }

    fn parse_suffixed_expression(&mut self) -> Result<Expression<S::String>, ParseError> {
        let mut expr = self.parse_primary_expression()?;
        loop {
            match self.peek(0)? {
                Some(Token::LeftParen) => {
                    self.advance(1);
                    let arguments = self.parse_expression_list()?;
                    expr = Expression::Call {
                        base: Box::new(expr),
                        arguments,
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_primary_expression(&mut self) -> Result<Expression<S::String>, ParseError> {
        match self.expect_next("<expression>")? {
            (_, Token::LeftParen) => {
                let expr = self.parse_expression()?;
                self.parse_token(Token::RightParen)?;
                Ok(Expression::Group(Box::new(expr)))
            }
            (_, Token::Identifier(n)) => Ok(Expression::Name(n)),
            (line_number, token) => Err(ParseError {
                kind: ParseErrorKind::Unexpected {
                    unexpected: token_indicator(&token),
                    expected: "grouped expression or name",
                },
                line_number,
            }),
        }
    }

    fn parse_simple_expression(&mut self) -> Result<Expression<S::String>, ParseError> {
        match self.expect_next("<expression>")? {
            (_, Token::Undefined) => Ok(Expression::Undefined),
            (_, Token::True) => Ok(Expression::True),
            (_, Token::False) => Ok(Expression::False),
            (_, Token::Float(f)) => Ok(Expression::Float(f)),
            (_, Token::Integer(i)) => Ok(Expression::Integer(i)),
            (_, Token::String(s)) => Ok(Expression::String(s)),
            (_, Token::Identifier(i)) => Ok(Expression::Name(i)),
            _ => self.parse_suffixed_expression(),
        }
    }

    fn parse_expression_list(&mut self) -> Result<Vec<Expression<S::String>>, ParseError> {
        let mut expressions = Vec::new();
        expressions.push(self.parse_expression()?);
        while matches!(self.peek(0)?, Some(Token::Comma)) {
            self.advance(1);
            expressions.push(self.parse_expression()?);
        }
        Ok(expressions)
    }

    fn parse_identifier(&mut self) -> Result<S::String, ParseError> {
        match self.next()? {
            Some((_, Token::Identifier(i))) => Ok(i),
            Some((line_number, t)) => Err(ParseError {
                kind: ParseErrorKind::Unexpected {
                    unexpected: token_indicator(&t),
                    expected: "<identifier>",
                },
                line_number,
            }),
            None => Err(ParseError {
                kind: ParseErrorKind::EndOfStream {
                    expected: "<identifier>",
                },
                line_number: self.lexer.line_number(),
            }),
        }
    }

    fn parse_token(&mut self, expected: Token<&'static str>) -> Result<(), ParseError> {
        if let Some((line_number, next)) = self.next()? {
            if next.as_str() == expected {
                Ok(())
            } else {
                Err(ParseError {
                    kind: ParseErrorKind::Unexpected {
                        unexpected: token_indicator(&next),
                        expected: token_indicator(&expected),
                    },
                    line_number,
                })
            }
        } else {
            Err(ParseError {
                kind: ParseErrorKind::EndOfStream {
                    expected: token_indicator(&expected),
                },
                line_number: self.lexer.line_number(),
            })
        }
    }

    fn peek(&mut self, n: usize) -> Result<Option<&Token<S::String>>, ParseError> {
        Ok(self.look_ahead(n)?.map(|(_, t)| t))
    }

    fn look_ahead(
        &mut self,
        n: usize,
    ) -> Result<Option<(LineNumber, &Token<S::String>)>, ParseError> {
        while self.peek_buffer.len() <= n {
            self.lexer.skip_whitespace();
            let line_number = self.lexer.line_number();
            if let Some(token) = self.lexer.read_token().map_err(|e| ParseError {
                kind: ParseErrorKind::LexError(e),
                line_number: self.lexer.line_number(),
            })? {
                self.peek_buffer.push((line_number, token));
            } else {
                break;
            }
        }
        Ok(self.peek_buffer.get(n).map(|(ln, t)| (*ln, t)))
    }

    fn next(&mut self) -> Result<Option<(LineNumber, Token<S::String>)>, ParseError> {
        self.look_ahead(1)?;
        Ok(if !self.peek_buffer.is_empty() {
            Some(self.peek_buffer.remove(0))
        } else {
            None
        })
    }

    fn expect_next(
        &mut self,
        expected: &'static str,
    ) -> Result<(LineNumber, Token<S::String>), ParseError> {
        if let Some((line_number, token)) = self.next()? {
            Ok((line_number, token))
        } else {
            Err(ParseError {
                kind: ParseErrorKind::EndOfStream { expected },
                line_number: self.lexer.line_number(),
            })
        }
    }

    fn advance(&mut self, n: usize) {
        assert!(
            n <= self.peek_buffer.len(),
            "cannot advance over un-peeked tokens"
        );
        self.peek_buffer.drain(0..n);
    }
}

fn get_unary_operator<S>(token: &Token<S>) -> Option<UnaryOperator> {
    match *token {
        Token::Minus => Some(UnaryOperator::Minus),
        Token::Bang => Some(UnaryOperator::Not),
        _ => None,
    }
}

fn get_binary_operator<S>(token: &Token<S>) -> Option<BinaryOperator> {
    match *token {
        Token::Plus => Some(BinaryOperator::Add),
        Token::Minus => Some(BinaryOperator::Sub),
        Token::Star => Some(BinaryOperator::Mult),
        Token::Slash => Some(BinaryOperator::Div),
        Token::DoubleAmpersand => Some(BinaryOperator::And),
        Token::DoublePipe => Some(BinaryOperator::Or),
        _ => None,
    }
}

fn get_assignment_operator<S>(token: &Token<S>) -> Option<AssignmentOperator> {
    match *token {
        Token::Equal => Some(AssignmentOperator::Equal),
        Token::PlusEqual => Some(AssignmentOperator::PlusEqual),
        Token::MinusEqual => Some(AssignmentOperator::MinusEqual),
        Token::StarEqual => Some(AssignmentOperator::MultEqual),
        Token::SlashEqual => Some(AssignmentOperator::DivEqual),
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
        Token::SemiColon => ";",
        Token::Comma => ",",
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
        Token::LessThan => "<",
        Token::LessEqual => "<=",
        Token::GreaterThan => ">",
        Token::GreaterEqual => ">=",
        Token::DoublePlus => "++",
        Token::DoubleMinus => "--",
        Token::DoubleAmpersand => "&&",
        Token::DoublePipe => "--",
        Token::Var => "var",
        Token::Switch => "switch",
        Token::Case => "case",
        Token::Break => "break",
        Token::For => "for",
        Token::Repeat => "repeat",
        Token::Undefined => "undefined",
        Token::True => "true",
        Token::False => "false",
        Token::Integer(_) => "<integer>",
        Token::Float(_) => "<float>",
        Token::Identifier(_) => "<identifier>",
        Token::String(_) => "<string>",
    }
}

type Priority = u8;

// Priority lower than any unary or binary operator.
const MIN_PRIORITY: Priority = 0;

// Priority of all unary operators.
const UNARY_PRIORITY: Priority = 5;

// Returns the left and right priority of the given binary operator.
fn binary_priority(operator: BinaryOperator) -> (Priority, Priority) {
    match operator {
        BinaryOperator::Mult => (4, 4),
        BinaryOperator::Div => (4, 4),
        BinaryOperator::Add => (3, 3),
        BinaryOperator::Sub => (3, 3),
        BinaryOperator::And => (2, 2),
        BinaryOperator::Or => (1, 1),
    }
}
