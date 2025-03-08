use std::fmt;

use thiserror::Error;

use crate::{
    lexer::{LexError, Lexer, LineNumber, Token},
    string_interner::StringInterner,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Block<S> {
    pub statements: Vec<Statement<S>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<S> {
    For(ForStatement<S>),
    Var(VarStatement<S>),
    Assignment(AssignmentStatement<S>),
    Block(Block<S>),
    Call(FunctionCall<S>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStatement<S> {
    pub initializer: Box<Statement<S>>,
    pub condition: Box<Expression<S>>,
    pub iterator: Box<Statement<S>>,
    pub body: Block<S>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarStatement<S> {
    pub name: S,
    pub value: Box<Expression<S>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentStatement<S> {
    pub name: S,
    pub op: AssignmentOperator,
    pub value: Box<Expression<S>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<S> {
    Undefined,
    True,
    False,
    Float(f64),
    Integer(u64),
    String(S),
    Name(S),
    Group(Box<Expression<S>>),
    Call(FunctionCall<S>),
    Unary(UnaryOperator, Box<Expression<S>>),
    Binary(Box<Expression<S>>, BinaryOperator, Box<Expression<S>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall<S> {
    base: Box<Expression<S>>,
    arguments: Vec<Expression<S>>,
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

pub fn parse<S>(source: &str, interner: S) -> Result<Block<S::String>, ParseError>
where
    S: StringInterner,
    <S as StringInterner>::String: fmt::Debug,
{
    Parser {
        lexer: Lexer::new(source, interner),
        peek_buffer: Vec::new(),
    }
    .parse()
}

struct Parser<'a, S: StringInterner> {
    lexer: Lexer<'a, S>,
    peek_buffer: Vec<(Token<S::String>, LineNumber)>,
}

impl<'a, S: StringInterner> Parser<'a, S>
where
    <S as StringInterner>::String: fmt::Debug,
{
    fn parse(&mut self) -> Result<Block<S::String>, ParseError> {
        let block = self.parse_block()?;

        if let Some((token, line_number)) = self.look_ahead(0)? {
            Err(ParseError {
                kind: ParseErrorKind::Unexpected {
                    unexpected: token_indicator(token),
                    expected: "statement",
                },
                line_number,
            })
        } else {
            Ok(block)
        }
    }

    fn parse_block(&mut self) -> Result<Block<S::String>, ParseError> {
        let mut statements = Vec::new();
        loop {
            if matches!(self.peek(0)?, None | Some(&Token::RightBrace)) {
                break;
            }

            statements.push(self.parse_statement()?);
            if matches!(self.peek(0)?, Some(Token::SemiColon)) {
                self.advance(1);
            }
        }
        Ok(Block { statements })
    }

    fn parse_statement(&mut self) -> Result<Statement<S::String>, ParseError> {
        match self.expect_ahead(0, "<statement>")? {
            (Token::For, _) => {
                self.advance(1);
                self.parse_token(Token::LeftParen)?;
                let initializer = self.parse_statement()?;
                self.parse_token(Token::SemiColon)?;
                let condition = self.parse_expression()?;
                self.parse_token(Token::SemiColon)?;
                let iterator = self.parse_statement()?;
                self.parse_token(Token::RightParen)?;
                self.parse_token(Token::LeftBrace)?;
                let body = self.parse_block()?;
                self.parse_token(Token::RightBrace)?;
                Ok(Statement::For(ForStatement {
                    initializer: Box::new(initializer),
                    condition: Box::new(condition),
                    iterator: Box::new(iterator),
                    body,
                }))
            }
            (Token::Var, _) => {
                self.advance(1);
                let name = self.parse_identifier()?;
                self.parse_token(Token::Equal)?;
                let value = self.parse_expression()?;
                Ok(Statement::Var(VarStatement {
                    name,
                    value: Box::new(value),
                }))
            }
            (Token::LeftBrace, _) => {
                self.advance(1);
                let block = self.parse_block()?;
                self.parse_token(Token::RightBrace)?;
                Ok(Statement::Block(block))
            }
            (token, line_number) => {
                if matches!(token, Token::Identifier(_)) {
                    if let Some(assignment_op) = self.peek(1)?.and_then(get_assignment_operator) {
                        let Ok(Some((Token::Identifier(name), _))) = self.next() else {
                            unreachable!()
                        };
                        self.advance(1);
                        let value = self.parse_expression()?;
                        return Ok(Statement::Assignment(AssignmentStatement {
                            name,
                            op: assignment_op,
                            value: Box::new(value),
                        }));
                    }
                }

                match self.parse_suffixed_expression() {
                    Ok(Expression::Call(call)) => Ok(Statement::Call(call)),
                    Ok(_) => Err(ParseError {
                        kind: ParseErrorKind::Unexpected {
                            unexpected: "<non-statement expression>",
                            expected: "<statement>",
                        },
                        line_number,
                    }),
                    Err(ParseError {
                        kind: ParseErrorKind::Unexpected { unexpected, .. },
                        line_number,
                    }) => Err(ParseError {
                        kind: ParseErrorKind::Unexpected {
                            unexpected,
                            expected: "<statement>",
                        },
                        line_number,
                    }),
                    Err(err) => Err(err),
                }
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
                    expr = Expression::Call(FunctionCall {
                        base: Box::new(expr),
                        arguments,
                    });
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_primary_expression(&mut self) -> Result<Expression<S::String>, ParseError> {
        match self.expect_next("<expression>")? {
            (Token::LeftParen, _) => {
                let expr = self.parse_expression()?;
                self.parse_token(Token::RightParen)?;
                Ok(Expression::Group(Box::new(expr)))
            }
            (Token::Identifier(n), _) => Ok(Expression::Name(n)),
            (token, line_number) => Err(ParseError {
                kind: ParseErrorKind::Unexpected {
                    unexpected: token_indicator(&token),
                    expected: "<grouped expression or name>",
                },
                line_number,
            }),
        }
    }

    fn parse_simple_expression(&mut self) -> Result<Expression<S::String>, ParseError> {
        match self.expect_next("<expression>")? {
            (Token::Undefined, _) => Ok(Expression::Undefined),
            (Token::True, _) => Ok(Expression::True),
            (Token::False, _) => Ok(Expression::False),
            (Token::Float(f), _) => Ok(Expression::Float(f)),
            (Token::Integer(i), _) => Ok(Expression::Integer(i)),
            (Token::String(s), _) => Ok(Expression::String(s)),
            (Token::Identifier(i), _) => Ok(Expression::Name(i)),
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
            Some((Token::Identifier(i), _)) => Ok(i),
            Some((t, line_number)) => Err(ParseError {
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
        if let Some((next, line_number)) = self.next()? {
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

    // Returns the `n`th token ahead in the token stream if it exists, along with the line number on
    // which it is found.
    fn look_ahead(
        &mut self,
        n: usize,
    ) -> Result<Option<(&Token<S::String>, LineNumber)>, ParseError> {
        while self.peek_buffer.len() <= n {
            self.lexer.skip_whitespace();
            let line_number = self.lexer.line_number();
            if let Some(token) = self.lexer.read_token().map_err(|e| ParseError {
                kind: ParseErrorKind::LexError(e),
                line_number: self.lexer.line_number(),
            })? {
                self.peek_buffer.push((token, line_number));
            } else {
                break;
            }
        }
        Ok(self.peek_buffer.get(n).map(|(t, ln)| (t, *ln)))
    }

    // Returns the `n`th token ahead in the token stream, and if it doesn't exist produces a
    // `ParseErrorKind::EndOfStream` error.
    fn expect_ahead(
        &mut self,
        n: usize,
        expected: &'static str,
    ) -> Result<(&Token<S::String>, LineNumber), ParseError> {
        if self.look_ahead(n)?.is_some() {
            // Workaround for lack of polonius borrow checker:
            // https://github.com/rust-lang/rust/issues/54663
            Ok(self.look_ahead(n).unwrap().unwrap())
        } else {
            Err(ParseError {
                kind: ParseErrorKind::EndOfStream { expected },
                line_number: self.lexer.line_number(),
            })
        }
    }

    // Equivalent to `Parser::look_ahead` but only returns the token.
    fn peek(&mut self, n: usize) -> Result<Option<&Token<S::String>>, ParseError> {
        Ok(self.look_ahead(n)?.map(|(t, _)| t))
    }

    // Return the next token in the token stream if it exists and advance the stream.
    fn next(&mut self) -> Result<Option<(Token<S::String>, LineNumber)>, ParseError> {
        self.look_ahead(1)?;
        Ok(if !self.peek_buffer.is_empty() {
            Some(self.peek_buffer.remove(0))
        } else {
            None
        })
    }

    // Return the next token in the token stream, producing a `ParseErrorKind::EndOfStream` error if
    // it doesn't exist.
    fn expect_next(
        &mut self,
        expected: &'static str,
    ) -> Result<(Token<S::String>, LineNumber), ParseError> {
        if let Some((token, line_number)) = self.next()? {
            Ok((token, line_number))
        } else {
            Err(ParseError {
                kind: ParseErrorKind::EndOfStream { expected },
                line_number: self.lexer.line_number(),
            })
        }
    }

    // Advance the token stream `n` tokens.
    //
    // # Panics
    //
    // Panics if this would advance over tokens that have not been observed with a peek method.
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
        Token::DoubleEqual => Some(BinaryOperator::Equal),
        Token::BangEqual => Some(BinaryOperator::NotEqual),
        Token::Less => Some(BinaryOperator::LessThan),
        Token::LessEqual => Some(BinaryOperator::LessEqual),
        Token::Greater => Some(BinaryOperator::GreaterThan),
        Token::GreaterEqual => Some(BinaryOperator::GreaterEqual),
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
        Token::Less => "<",
        Token::LessEqual => "<=",
        Token::Greater => ">",
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
fn binary_priority(operator: BinaryOperator) -> (OperatorPriority, OperatorPriority) {
    match operator {
        BinaryOperator::Add => (4, 4),
        BinaryOperator::Sub => (4, 4),
        BinaryOperator::Mult => (5, 5),
        BinaryOperator::Div => (5, 5),
        BinaryOperator::NotEqual => (3, 3),
        BinaryOperator::Equal => (3, 3),
        BinaryOperator::LessThan => (3, 3),
        BinaryOperator::LessEqual => (3, 3),
        BinaryOperator::GreaterThan => (3, 3),
        BinaryOperator::GreaterEqual => (3, 3),
        BinaryOperator::And => (2, 2),
        BinaryOperator::Or => (1, 1),
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

        super::parse(source, SimpleInterner)
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
        "#;

        assert_eq!(
            parse(SOURCE).unwrap(),
            Block {
                statements: vec![
                    Statement::Var(VarStatement {
                        name: "sum".to_owned(),
                        value: Box::new(Expression::Integer(0))
                    }),
                    Statement::For(ForStatement {
                        initializer: Box::new(Statement::Var(VarStatement {
                            name: "i".to_owned(),
                            value: Box::new(Expression::Integer(0))
                        })),
                        condition: Box::new(Expression::Binary(
                            Box::new(Expression::Name("i".to_owned())),
                            BinaryOperator::LessThan,
                            Box::new(Expression::Integer(1000000))
                        )),
                        iterator: Box::new(Statement::Assignment(AssignmentStatement {
                            name: "i".to_owned(),
                            op: AssignmentOperator::PlusEqual,
                            value: Box::new(Expression::Integer(1)),
                        })),
                        body: Block {
                            statements: vec![Statement::Assignment(AssignmentStatement {
                                name: "sum".to_owned(),
                                op: AssignmentOperator::PlusEqual,
                                value: Box::new(Expression::Name("i".to_owned())),
                            })]
                        }
                    })
                ]
            }
        );
    }
}
