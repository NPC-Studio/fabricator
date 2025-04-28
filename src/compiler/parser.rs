use std::fmt::Debug;

use thiserror::Error;

use crate::compiler::{
    lexer::{LexError, Lexer, LineNumber, Token},
    string_interner::StringInterner,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Block<S> {
    pub statements: Vec<Statement<S>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<S> {
    Var(VarStatement<S>),
    Assignment(AssignmentStatement<S>),
    Return(ReturnStatement<S>),
    If(IfStatement<S>),
    For(ForStatement<S>),
    Block(Block<S>),
    Call(CallExpr<S>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarStatement<S> {
    pub name: S,
    pub value: Box<Expression<S>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentStatement<S> {
    pub target: AssignmentTarget<S>,
    pub op: AssignmentOp,
    pub value: Box<Expression<S>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentTarget<S> {
    Name(S),
    Field(FieldExpr<S>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStatement<S> {
    pub value: Option<Box<Expression<S>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement<S> {
    pub condition: Box<Expression<S>>,
    pub then_stmt: Box<Statement<S>>,
    pub else_stmt: Option<Box<Statement<S>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStatement<S> {
    pub initializer: Box<Statement<S>>,
    pub condition: Box<Expression<S>>,
    pub iterator: Box<Statement<S>>,
    pub body: Box<Statement<S>>,
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
    Object(Vec<(S, Expression<S>)>),
    Unary(UnaryOp, Box<Expression<S>>),
    Binary(Box<Expression<S>>, BinaryOp, Box<Expression<S>>),
    Function(FunctionExpr<S>),
    Call(CallExpr<S>),
    Field(FieldExpr<S>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionExpr<S> {
    pub parameters: Vec<S>,
    pub body: Block<S>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr<S> {
    pub base: Box<Expression<S>>,
    pub arguments: Vec<Expression<S>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldExpr<S> {
    pub base: Box<Expression<S>>,
    pub field: S,
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
#[error("parse error at line {line_number}: {kind}")]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub line_number: LineNumber,
}

pub fn parse<S>(source: &str, interner: S) -> Result<Block<S::String>, ParseError>
where
    S: StringInterner,
{
    Parser::new(source, interner).parse()
}

struct Parser<'a, S: StringInterner> {
    lexer: Lexer<'a, S>,
    look_ahead_buffer: Vec<Option<(Token<S::String>, LineNumber)>>,
}

impl<'a, S: StringInterner> Parser<'a, S> {
    fn new(source: &'a str, interner: S) -> Self {
        Parser {
            lexer: Lexer::new(source, interner),
            look_ahead_buffer: Vec::new(),
        }
    }

    fn parse(&mut self) -> Result<Block<S::String>, ParseError> {
        let block = self.parse_block()?;

        self.look_ahead(1)?;
        if let Some((token, line_number)) = self.peek(0) {
            Err(ParseError {
                kind: ParseErrorKind::Unexpected {
                    unexpected: token_indicator(token),
                    expected: "<statement>",
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
            self.look_ahead(1)?;
            if matches!(self.peek(0), None | Some((Token::RightBrace, _))) {
                break;
            }

            statements.push(self.parse_statement()?);

            self.look_ahead(1)?;
            if matches!(self.peek(0), Some((Token::SemiColon, _))) {
                self.advance(1);
            }
        }

        Ok(Block { statements })
    }

    fn parse_statement(&mut self) -> Result<Statement<S::String>, ParseError> {
        self.look_ahead(1)?;
        match self.peek_expected(0, "<statement>")? {
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
            (Token::Return, _) => {
                self.advance(1);
                self.look_ahead(1)?;
                let value = match self.peek(0) {
                    Some((Token::SemiColon, _)) => None,
                    None => None,
                    _ => Some(self.parse_expression()?),
                };
                Ok(Statement::Return(ReturnStatement {
                    value: value.map(Box::new),
                }))
            }
            (Token::If, _) => {
                self.advance(1);
                let condition = self.parse_expression()?;
                let then_stmt = self.parse_statement()?;
                let mut else_stmt = None;

                self.look_ahead(1)?;
                if matches!(self.peek(0), Some((Token::Else, _))) {
                    self.advance(1);
                    else_stmt = Some(self.parse_statement()?);
                }

                Ok(Statement::If(IfStatement {
                    condition: Box::new(condition),
                    then_stmt: Box::new(then_stmt),
                    else_stmt: else_stmt.map(Box::new),
                }))
            }
            (Token::For, _) => {
                self.advance(1);
                self.parse_token(Token::LeftParen)?;
                let initializer = self.parse_statement()?;
                self.parse_token(Token::SemiColon)?;
                let condition = self.parse_expression()?;
                self.parse_token(Token::SemiColon)?;
                let iterator = self.parse_statement()?;
                self.parse_token(Token::RightParen)?;
                let body = self.parse_statement()?;
                Ok(Statement::For(ForStatement {
                    initializer: Box::new(initializer),
                    condition: Box::new(condition),
                    iterator: Box::new(iterator),
                    body: Box::new(body),
                }))
            }
            (Token::LeftBrace, _) => {
                self.advance(1);
                let block = self.parse_block()?;
                self.parse_token(Token::RightBrace)?;
                Ok(Statement::Block(block))
            }
            (_, line_number) => match self.parse_suffixed_expression() {
                Ok(Expression::Call(call)) => Ok(Statement::Call(call)),
                Ok(expr @ (Expression::Name(_) | Expression::Field(_))) => {
                    let target = match expr {
                        Expression::Name(name) => AssignmentTarget::Name(name),
                        Expression::Field(field) => AssignmentTarget::Field(field),
                        _ => unreachable!(),
                    };

                    self.look_ahead(1)?;
                    if let Some(assignment_op) =
                        self.peek(0).and_then(|(t, _)| get_assignment_operator(t))
                    {
                        self.advance(1);
                        let value = self.parse_expression()?;
                        Ok(Statement::Assignment(AssignmentStatement {
                            target,
                            op: assignment_op,
                            value: Box::new(value),
                        }))
                    } else {
                        Err(ParseError {
                            kind: ParseErrorKind::Unexpected {
                                unexpected: "<non-statement expression>",
                                expected: "<statement>",
                            },
                            line_number,
                        })
                    }
                }
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
            },
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
        let mut expr = if let Some(unary_op) = self.peek(0).and_then(|(t, _)| get_unary_operator(t))
        {
            self.advance(1);
            Expression::Unary(
                unary_op,
                Box::new(self.parse_sub_expression(UNARY_PRIORITY)?),
            )
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
            expr = Expression::Binary(Box::new(expr), binary_op, Box::new(right_expression));
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
                    self.parse_token(Token::RightParen)?;
                    expr = Expression::Call(CallExpr {
                        base: Box::new(expr),
                        arguments,
                    });
                }
                Some((Token::Dot, _)) => {
                    self.advance(1);
                    let field = self.parse_identifier()?;
                    expr = Expression::Field(FieldExpr {
                        base: Box::new(expr),
                        field,
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
        self.look_ahead(1)?;
        match self.peek_expected(0, "<expression>")? {
            (Token::Undefined, _) => {
                self.advance(1);
                Ok(Expression::Undefined)
            }
            (Token::True, _) => {
                self.advance(1);
                Ok(Expression::True)
            }
            (Token::False, _) => {
                self.advance(1);
                Ok(Expression::False)
            }
            (&Token::Float(f), _) => {
                self.advance(1);
                Ok(Expression::Float(f))
            }
            (&Token::Integer(i), _) => {
                self.advance(1);
                Ok(Expression::Integer(i))
            }
            (Token::String(_), _) => {
                let Some((Token::String(s), _)) = self.next().unwrap() else {
                    unreachable!()
                };
                Ok(Expression::String(s))
            }
            (Token::Identifier(_), _) => {
                self.look_ahead(2)?;
                if matches!(self.peek(1), Some((Token::LeftParen | Token::Dot, _))) {
                    self.parse_suffixed_expression()
                } else {
                    let Some((Token::Identifier(i), _)) = self.next().unwrap() else {
                        unreachable!()
                    };
                    Ok(Expression::Name(i))
                }
            }
            (Token::Function, _) => {
                self.advance(1);
                self.parse_token(Token::LeftParen)?;
                let mut parameters = Vec::new();
                self.look_ahead(1)?;
                if !matches!(self.peek(0), Some((Token::RightParen, _))) {
                    loop {
                        parameters.push(self.parse_identifier()?);
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
                self.parse_token(Token::RightBrace)?;

                Ok(Expression::Function(FunctionExpr { parameters, body }))
            }
            (Token::LeftBrace, _) => Ok(Expression::Object(self.parse_object()?)),
            _ => self.parse_suffixed_expression(),
        }
    }

    fn parse_object(&mut self) -> Result<Vec<(S::String, Expression<S::String>)>, ParseError> {
        self.parse_token(Token::LeftBrace)?;
        let mut entries = Vec::new();
        loop {
            let key = self.parse_identifier()?;
            self.parse_token(Token::Colon)?;
            let value = self.parse_expression()?;

            entries.push((key, value));

            self.look_ahead(1)?;
            match self.peek(0) {
                Some((Token::Comma, _)) => {
                    self.advance(1);
                    self.look_ahead(1)?;
                    if matches!(self.peek(0), Some((Token::RightBrace, _))) {
                        self.advance(1);
                        break;
                    }
                }
                Some((Token::RightBrace, _)) => {
                    self.advance(1);
                    break;
                }
                Some((t, line_number)) => {
                    return Err(ParseError {
                        kind: ParseErrorKind::Unexpected {
                            unexpected: token_indicator(t),
                            expected: "',' or '}'",
                        },
                        line_number,
                    });
                }
                None => {
                    return Err(ParseError {
                        kind: ParseErrorKind::EndOfStream {
                            expected: "',' or '}'",
                        },
                        line_number: self.lexer.line_number(),
                    });
                }
            }
        }

        Ok(entries)
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

    // Look ahead `n` tokens in the lexer, making them available to peek methods.
    fn look_ahead(&mut self, n: usize) -> Result<(), ParseError> {
        while self.look_ahead_buffer.len() < n {
            self.lexer.skip_whitespace();
            let line_number = self.lexer.line_number();
            if let Some(token) = self.lexer.read_token().map_err(|e| ParseError {
                kind: ParseErrorKind::LexError(e),
                line_number: self.lexer.line_number(),
            })? {
                self.look_ahead_buffer.push(Some((token, line_number)));
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
    fn peek(&self, n: usize) -> Option<(&Token<S::String>, LineNumber)> {
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
    ) -> Result<(&Token<S::String>, LineNumber), ParseError> {
        self.peek(n).ok_or_else(|| ParseError {
            kind: ParseErrorKind::EndOfStream { expected },
            line_number: self.lexer.line_number(),
        })
    }

    // Return the next token in the token stream if it exists and advance the stream.
    fn next(&mut self) -> Result<Option<(Token<S::String>, LineNumber)>, ParseError> {
        self.look_ahead(1)?;
        Ok(self.look_ahead_buffer.remove(0))
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

            if sum > 100 {
                print("yes");
            }

            test.foo = 1;
            test.bar = {
                a: 1,
                b: 2,
            };
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
                            BinaryOp::LessThan,
                            Box::new(Expression::Integer(1000000))
                        )),
                        iterator: Box::new(Statement::Assignment(AssignmentStatement {
                            target: AssignmentTarget::Name("i".to_owned()),
                            op: AssignmentOp::PlusEqual,
                            value: Box::new(Expression::Integer(1)),
                        })),
                        body: Box::new(Statement::Block(Block {
                            statements: vec![Statement::Assignment(AssignmentStatement {
                                target: AssignmentTarget::Name("sum".to_owned()),
                                op: AssignmentOp::PlusEqual,
                                value: Box::new(Expression::Name("i".to_owned())),
                            })]
                        }))
                    }),
                    Statement::If(IfStatement {
                        condition: Box::new(Expression::Binary(
                            Box::new(Expression::Name("sum".to_owned())),
                            BinaryOp::GreaterThan,
                            Box::new(Expression::Integer(100)),
                        )),
                        then_stmt: Box::new(Statement::Block(Block {
                            statements: vec![Statement::Call(CallExpr {
                                base: Box::new(Expression::Name("print".to_owned())),
                                arguments: vec![Expression::String("yes".to_owned())],
                            })]
                        })),
                        else_stmt: None,
                    }),
                    Statement::Assignment(AssignmentStatement {
                        target: AssignmentTarget::Field(FieldExpr {
                            base: Box::new(Expression::Name("test".to_owned())),
                            field: "foo".to_owned(),
                        }),
                        op: AssignmentOp::Equal,
                        value: Box::new(Expression::Integer(1)),
                    }),
                    Statement::Assignment(AssignmentStatement {
                        target: AssignmentTarget::Field(FieldExpr {
                            base: Box::new(Expression::Name("test".to_owned())),
                            field: "bar".to_owned(),
                        }),
                        op: AssignmentOp::Equal,
                        value: Box::new(Expression::Object(vec![
                            ("a".to_owned(), Expression::Integer(1)),
                            ("b".to_owned(), Expression::Integer(2)),
                        ])),
                    }),
                ]
            }
        );
    }
}
