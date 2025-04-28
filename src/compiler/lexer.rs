use std::fmt;

use thiserror::Error;

use crate::compiler::string_interner::StringInterner;

#[derive(Debug, Copy, Clone)]
pub enum Token<S> {
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,

    Colon,
    SemiColon,
    Comma,

    Dot,

    Plus,
    Minus,
    Bang,
    Slash,
    Star,
    Mod,
    Div,
    Percent,
    Ampersand,
    Pipe,

    Equal,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    PercentEqual,

    DoubleEqual,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    DoublePlus,
    DoubleMinus,

    DoubleAmpersand,
    DoublePipe,

    Var,
    Function,

    Switch,
    Case,
    Break,
    If,
    Else,
    For,
    Repeat,
    Return,

    Undefined,
    True,
    False,

    Integer(u64),
    Float(f64),
    Identifier(S),
    String(S),
}

impl<S: AsRef<str>> Token<S> {
    pub fn as_str(&self) -> Token<&str> {
        match self {
            Token::LeftParen => Token::LeftParen,
            Token::RightParen => Token::RightParen,
            Token::LeftBracket => Token::LeftBracket,
            Token::RightBracket => Token::RightBracket,
            Token::LeftBrace => Token::LeftBrace,
            Token::RightBrace => Token::RightBrace,
            Token::Colon => Token::Colon,
            Token::SemiColon => Token::SemiColon,
            Token::Comma => Token::Comma,
            Token::Dot => Token::Dot,
            Token::Plus => Token::Plus,
            Token::Minus => Token::Minus,
            Token::Bang => Token::Bang,
            Token::Slash => Token::Slash,
            Token::Star => Token::Star,
            Token::Mod => Token::Mod,
            Token::Div => Token::Div,
            Token::Percent => Token::Percent,
            Token::Ampersand => Token::Ampersand,
            Token::Pipe => Token::Pipe,
            Token::Equal => Token::Equal,
            Token::PlusEqual => Token::PlusEqual,
            Token::MinusEqual => Token::MinusEqual,
            Token::StarEqual => Token::StarEqual,
            Token::SlashEqual => Token::SlashEqual,
            Token::PercentEqual => Token::PercentEqual,
            Token::DoubleEqual => Token::DoubleEqual,
            Token::BangEqual => Token::BangEqual,
            Token::Less => Token::Less,
            Token::LessEqual => Token::LessEqual,
            Token::Greater => Token::Greater,
            Token::GreaterEqual => Token::GreaterEqual,
            Token::DoublePlus => Token::DoublePlus,
            Token::DoubleMinus => Token::DoubleMinus,
            Token::DoubleAmpersand => Token::DoubleAmpersand,
            Token::DoublePipe => Token::DoublePipe,
            Token::Var => Token::Var,
            Token::Function => Token::Function,
            Token::Switch => Token::Switch,
            Token::Case => Token::Case,
            Token::Break => Token::Break,
            Token::If => Token::If,
            Token::Else => Token::Else,
            Token::For => Token::For,
            Token::Repeat => Token::Repeat,
            Token::Return => Token::Return,
            Token::Undefined => Token::Undefined,
            Token::True => Token::True,
            Token::False => Token::False,
            Token::Integer(i) => Token::Integer(*i),
            Token::Float(f) => Token::Float(*f),
            Token::Identifier(i) => Token::Identifier(i.as_ref()),
            Token::String(s) => Token::String(s.as_ref()),
        }
    }
}

impl<S: PartialEq> PartialEq for Token<S> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Token::LeftParen, Token::LeftParen) => true,
            (Token::RightParen, Token::RightParen) => true,
            (Token::LeftBracket, Token::LeftBracket) => true,
            (Token::RightBracket, Token::RightBracket) => true,
            (Token::LeftBrace, Token::LeftBrace) => true,
            (Token::RightBrace, Token::RightBrace) => true,
            (Token::Colon, Token::Colon) => true,
            (Token::SemiColon, Token::SemiColon) => true,
            (Token::Comma, Token::Comma) => true,
            (Token::Dot, Token::Dot) => true,
            (Token::Plus, Token::Plus) => true,
            (Token::Minus, Token::Minus) => true,
            (Token::Bang, Token::Bang) => true,
            (Token::Slash, Token::Slash) => true,
            (Token::Star, Token::Star) => true,
            (Token::Mod, Token::Mod) => true,
            (Token::Div, Token::Div) => true,
            (Token::Percent, Token::Percent) => true,
            (Token::Ampersand, Token::Ampersand) => true,
            (Token::Pipe, Token::Pipe) => true,
            (Token::Equal, Token::Equal) => true,
            (Token::PlusEqual, Token::PlusEqual) => true,
            (Token::MinusEqual, Token::MinusEqual) => true,
            (Token::StarEqual, Token::StarEqual) => true,
            (Token::SlashEqual, Token::SlashEqual) => true,
            (Token::PercentEqual, Token::PercentEqual) => true,
            (Token::DoubleEqual, Token::DoubleEqual) => true,
            (Token::BangEqual, Token::BangEqual) => true,
            (Token::Less, Token::Less) => true,
            (Token::LessEqual, Token::LessEqual) => true,
            (Token::Greater, Token::Greater) => true,
            (Token::GreaterEqual, Token::GreaterEqual) => true,
            (Token::DoublePlus, Token::DoublePlus) => true,
            (Token::DoubleMinus, Token::DoubleMinus) => true,
            (Token::DoubleAmpersand, Token::DoubleAmpersand) => true,
            (Token::DoublePipe, Token::DoublePipe) => true,
            (Token::Var, Token::Var) => true,
            (Token::Function, Token::Function) => true,
            (Token::Switch, Token::Switch) => true,
            (Token::Case, Token::Case) => true,
            (Token::Break, Token::Break) => true,
            (Token::If, Token::If) => true,
            (Token::Else, Token::Else) => true,
            (Token::For, Token::For) => true,
            (Token::Repeat, Token::Repeat) => true,
            (Token::Return, Token::Return) => true,
            (Token::Undefined, Token::Undefined) => true,
            (Token::True, Token::True) => true,
            (Token::False, Token::False) => true,
            (Token::Integer(a), Token::Integer(b)) => a == b,
            (Token::Float(a), Token::Float(b)) => a.to_bits() == b.to_bits(),
            (Token::Identifier(a), Token::Identifier(b)) => a == b,
            (Token::String(a), Token::String(b)) => a == b,
            _ => false,
        }
    }
}

impl<S: Eq> Eq for Token<S> {}

#[derive(Debug, Error)]
pub enum LexError {
    #[error("expected matching '\"' for string")]
    UnfinishedString,
    #[error("invalid string escape sequence")]
    InvalidStringEscape,
    #[error("unexpected character: {0:?}")]
    UnexpectedCharacter(char),
    #[error("malformed number")]
    BadNumber,
}

/// The current line number of the source input.
///
/// It is stored as 0-indexed internally, but will display as a more human-readable 1-indexed line
/// number.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct LineNumber(pub usize);

impl fmt::Display for LineNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0 + 1)
    }
}

pub struct Lexer<'a, S> {
    source: &'a str,
    interner: S,
    peek_buffer: Vec<char>,
    string_buffer: String,
    line_number: usize,
}

impl<'a, S> Lexer<'a, S>
where
    S: StringInterner,
{
    pub fn new(source: &'a str, interner: S) -> Lexer<'a, S> {
        Lexer {
            source,
            interner,
            peek_buffer: Vec::new(),
            string_buffer: String::new(),
            line_number: 0,
        }
    }

    /// Returns the current line number of the source file.
    pub fn line_number(&self) -> LineNumber {
        LineNumber(self.line_number)
    }

    /// Skips any leading whitespace before the next token in the stream.
    ///
    /// It is not necessary to call this explicitly, but doing so will make the current line number
    /// accurate for the start of the next returned token.
    pub fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek(0) {
            let nc = self.peek(1);

            match (c, nc) {
                _ if is_newline(c) => {
                    self.read_line_end();
                }
                _ if c.is_ascii_whitespace() => {
                    self.advance(1);
                }
                ('/', Some('/')) => {
                    self.advance(2);
                    // Read until end of line
                    while let Some(c) = self.peek(0) {
                        if is_newline(c) {
                            break;
                        } else {
                            self.advance(1);
                        }
                    }
                }
                ('/', Some('*')) => {
                    self.advance(2);
                    // read until '*/'
                    loop {
                        match (self.peek(0), self.peek(1)) {
                            (Some('*'), Some('/')) => {
                                self.advance(2);
                                break;
                            }
                            (Some(c), _) if is_newline(c) => {
                                self.read_line_end();
                            }
                            (None, _) => break,
                            _ => self.advance(1),
                        }
                    }
                }
                _ => break,
            }
        }
    }

    /// Read and return the next token in the stream.
    ///
    /// Returns `None` once the token stream is finished.
    pub fn read_token(&mut self) -> Result<Option<Token<S::String>>, LexError> {
        self.skip_whitespace();

        Ok(match (self.peek(0), self.peek(1)) {
            (Some(c), _) if c.is_ascii_whitespace() => {
                unreachable!("whitespace should have been skipped")
            }
            (Some('='), Some('=')) => {
                self.advance(2);
                Some(Token::DoubleEqual)
            }
            (Some('+'), Some('=')) => {
                self.advance(2);
                Some(Token::PlusEqual)
            }
            (Some('-'), Some('=')) => {
                self.advance(2);
                Some(Token::MinusEqual)
            }
            (Some('*'), Some('=')) => {
                self.advance(2);
                Some(Token::StarEqual)
            }
            (Some('/'), Some('=')) => {
                self.advance(2);
                Some(Token::SlashEqual)
            }
            (Some('%'), Some('=')) => {
                self.advance(2);
                Some(Token::PercentEqual)
            }
            (Some('<'), Some('=')) => {
                self.advance(2);
                Some(Token::LessEqual)
            }
            (Some('>'), Some('=')) => {
                self.advance(2);
                Some(Token::GreaterEqual)
            }
            (Some('+'), Some('+')) => {
                self.advance(2);
                Some(Token::DoublePlus)
            }
            (Some('-'), Some('-')) => {
                self.advance(2);
                Some(Token::DoubleMinus)
            }
            (Some('&'), Some('&')) => {
                self.advance(2);
                Some(Token::DoubleAmpersand)
            }
            (Some('|'), Some('|')) => {
                self.advance(2);
                Some(Token::DoublePipe)
            }
            (Some('('), _) => {
                self.advance(1);
                Some(Token::LeftParen)
            }
            (Some(')'), _) => {
                self.advance(1);
                Some(Token::RightParen)
            }
            (Some('['), _) => {
                self.advance(1);
                Some(Token::LeftBracket)
            }
            (Some(']'), _) => {
                self.advance(1);
                Some(Token::RightBracket)
            }
            (Some('{'), _) => {
                self.advance(1);
                Some(Token::LeftBrace)
            }
            (Some('}'), _) => {
                self.advance(1);
                Some(Token::RightBrace)
            }
            (Some(':'), _) => {
                self.advance(1);
                Some(Token::Colon)
            }
            (Some(';'), _) => {
                self.advance(1);
                Some(Token::SemiColon)
            }
            (Some(','), _) => {
                self.advance(1);
                Some(Token::Comma)
            }
            (Some('.'), _) => {
                self.advance(1);
                Some(Token::Dot)
            }
            (Some('+'), _) => {
                self.advance(1);
                Some(Token::Plus)
            }
            (Some('-'), _) => {
                self.advance(1);
                Some(Token::Minus)
            }
            (Some('!'), _) => {
                self.advance(1);
                Some(Token::Bang)
            }
            (Some('/'), _) => {
                self.advance(1);
                Some(Token::Slash)
            }
            (Some('*'), _) => {
                self.advance(1);
                Some(Token::Star)
            }
            (Some('%'), _) => {
                self.advance(1);
                Some(Token::Percent)
            }
            (Some('&'), _) => {
                self.advance(1);
                Some(Token::Ampersand)
            }
            (Some('|'), _) => {
                self.advance(1);
                Some(Token::Pipe)
            }
            (Some('='), _) => {
                self.advance(1);
                Some(Token::Equal)
            }
            (Some('<'), _) => {
                self.advance(1);
                Some(Token::Less)
            }
            (Some('>'), _) => {
                self.advance(1);
                Some(Token::Greater)
            }
            (Some('"'), _) => {
                self.read_string()?;
                Some(Token::String(
                    self.interner.intern(self.string_buffer.as_str()),
                ))
            }
            (Some(c), _) if is_identifier_start_char(c) => {
                self.read_identifier();
                match self.string_buffer.as_str() {
                    "var" => Some(Token::Var),
                    "function" => Some(Token::Function),
                    "switch" => Some(Token::Switch),
                    "case" => Some(Token::Case),
                    "break" => Some(Token::Break),
                    "if" => Some(Token::If),
                    "else" => Some(Token::Else),
                    "for" => Some(Token::For),
                    "repeat" => Some(Token::Repeat),
                    "return" => Some(Token::Return),
                    "true" => Some(Token::True),
                    "false" => Some(Token::False),
                    "undefined" => Some(Token::Undefined),
                    id => Some(Token::Identifier(self.interner.intern(id))),
                }
            }
            (Some(c), _) if c.is_ascii_digit() => Some(self.read_numeral()?),
            (Some(c), _) => return Err(LexError::UnexpectedCharacter(c)),
            (None, _) => None,
        })
    }

    // Read any of "\n", "\r", "\n\r", or "\r\n" as a single newline, and increment the current line
    // number.
    fn read_line_end(&mut self) {
        let newline = self.peek(0).unwrap();
        assert!(is_newline(newline));
        self.advance(1);

        if let Some(next_newline) = self.peek(0) {
            if is_newline(next_newline) && next_newline != newline {
                self.advance(1);
            }
        }

        self.line_number += 1;
    }

    // Read an identifier into the string buffer.
    fn read_identifier(&mut self) {
        let start = self.peek(0).expect("no start char for identifier");
        assert!(
            is_identifier_start_char(start),
            "identifier does not start with alphabetic character"
        );

        self.string_buffer.clear();
        self.string_buffer.push(start);
        self.advance(1);

        while let Some(c) = self.peek(0) {
            if !is_identifier_char(c) {
                break;
            }

            self.string_buffer.push(c);
            self.advance(1);
        }
    }

    // Read a short (not multi-line) string literal into the string buffer.
    fn read_string(&mut self) -> Result<(), LexError> {
        assert!(self.peek(0).unwrap() == '"');
        self.advance(1);

        self.string_buffer.clear();

        loop {
            let c = if let Some(c) = self.peek(0) {
                c
            } else {
                return Err(LexError::UnfinishedString);
            };

            if is_newline(c) {
                return Err(LexError::UnfinishedString);
            }

            self.advance(1);
            if c == '\\' {
                match self.peek(0).ok_or_else(|| LexError::UnfinishedString)? {
                    'n' => {
                        self.advance(1);
                        self.string_buffer.push('\n');
                    }

                    'r' => {
                        self.advance(1);
                        self.string_buffer.push('\r');
                    }

                    't' => {
                        self.advance(1);
                        self.string_buffer.push('\t');
                    }

                    '\\' => {
                        self.advance(1);
                        self.string_buffer.push('\\');
                    }

                    '"' => {
                        self.advance(1);
                        self.string_buffer.push('"');
                    }

                    _ => {
                        return Err(LexError::InvalidStringEscape);
                    }
                }
            } else if c == '"' {
                break;
            } else {
                self.string_buffer.push(c);
            }
        }

        Ok(())
    }

    // Reads a hex or decimal integer or floating point number. Allows decimal integers (123), hex
    // integers (0xdeadbeef), and decimal floating point with optional exponent and exponent sign
    // (3.21e+1).
    fn read_numeral(&mut self) -> Result<Token<S::String>, LexError> {
        let p1 = self.peek(0).unwrap();
        assert!(p1.is_ascii_digit());

        self.string_buffer.clear();
        self.string_buffer.push(p1);
        self.advance(1);

        let mut is_hex = false;
        match (p1, self.peek(1)) {
            ('0', Some(p2)) if p2 == 'x' || p2 == 'X' => {
                is_hex = true;
                self.string_buffer.push(p2);
                self.advance(1);
            }
            _ => {}
        }

        let mut has_radix = false;
        while let Some(c) = self.peek(0) {
            if c == '.' && !has_radix {
                self.string_buffer.push('.');
                has_radix = true;
                self.advance(1);
            } else if (!is_hex && c.is_ascii_digit()) || (is_hex && c.is_ascii_hexdigit()) {
                self.string_buffer.push(c);
                self.advance(1);
            } else {
                break;
            }
        }

        let mut has_exp = false;
        if !is_hex {
            if let Some(exp_begin) = self.peek(0) {
                if exp_begin == 'e' || exp_begin == 'E' {
                    self.string_buffer.push(exp_begin);
                    has_exp = true;
                    self.advance(1);

                    if let Some(sign) = self.peek(0) {
                        if sign == '+' || sign == '-' {
                            self.string_buffer.push(sign);
                            self.advance(1);
                        }
                    }

                    while let Some(c) = self.peek(0) {
                        if c.is_ascii_digit() {
                            self.string_buffer.push(c);
                            self.advance(1);
                        } else {
                            break;
                        }
                    }
                }
            }
        }

        if !has_exp && !has_radix {
            if let Some(i) = if is_hex {
                read_hex_integer(&self.string_buffer)
            } else {
                read_dec_integer(&self.string_buffer)
            } {
                return Ok(Token::Integer(i));
            }

            // Fall back to parsing as a float if parsing as an integer fails...
        }

        if is_hex {
            // Hex floats are not supported
            return Err(LexError::BadNumber);
        } else {
            Ok(Token::Float(
                read_dec_float(&self.string_buffer).ok_or(LexError::BadNumber)?,
            ))
        }
    }

    // Peek to the `n`th character ahead in the character stream.
    fn peek(&mut self, n: usize) -> Option<char> {
        while self.peek_buffer.len() <= n {
            let mut chars = self.source.chars();
            if let Some(c) = chars.next() {
                self.peek_buffer.push(c);
                self.source = chars.as_str();
            } else {
                break;
            }
        }

        self.peek_buffer.get(n).copied()
    }

    // Advance the character stream `n` characters.
    //
    // # Panics
    //
    // Panics if this would advance over characters that have not been observed with `Lexer::peek`.
    fn advance(&mut self, n: usize) {
        assert!(
            n <= self.peek_buffer.len(),
            "cannot advance over un-peeked characters"
        );
        self.peek_buffer.drain(0..n);
    }
}

fn is_newline(c: char) -> bool {
    c == '\n' || c == '\r'
}

fn is_identifier_start_char(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_identifier_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

fn read_dec_integer(s: &str) -> Option<u64> {
    u64::from_str_radix(s, 10).ok()
}

fn read_hex_integer(s: &str) -> Option<u64> {
    let mut chars = s.chars();
    let c0 = chars.next()?;
    let c1 = chars.next()?;

    if c0 != '0' || (c1 != 'x' && c1 != 'X') || s.is_empty() {
        return None;
    }

    u64::from_str_radix(chars.as_str(), 16).ok()
}

pub fn read_dec_float(s: &str) -> Option<f64> {
    str::parse(s).ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(source: &str) -> Result<Vec<Token<String>>, LexError> {
        struct SimpleInterner;

        impl StringInterner for SimpleInterner {
            type String = String;

            fn intern(&mut self, s: &str) -> Self::String {
                s.to_owned()
            }
        }

        let mut tokens = Vec::new();
        let mut lexer = Lexer::new(source, SimpleInterner);
        while let Some(token) = lexer.read_token()? {
            tokens.push(token);
        }

        Ok(tokens)
    }

    #[test]
    fn test_lexer() {
        const SOURCE: &str = r#"
            // Line comment
            var sum = 0;
            for (var i = 0; i < 1000000; ++i) {
                /*
                    Multiline comment
                */
                sum += i;
            }
        "#;

        assert_eq!(
            lex(SOURCE).unwrap(),
            vec![
                Token::Var,
                Token::Identifier("sum".to_owned()),
                Token::Equal,
                Token::Integer(0),
                Token::SemiColon,
                Token::For,
                Token::LeftParen,
                Token::Var,
                Token::Identifier("i".to_owned()),
                Token::Equal,
                Token::Integer(0),
                Token::SemiColon,
                Token::Identifier("i".to_owned()),
                Token::Less,
                Token::Integer(1000000),
                Token::SemiColon,
                Token::DoublePlus,
                Token::Identifier("i".to_owned()),
                Token::RightParen,
                Token::LeftBrace,
                Token::Identifier("sum".to_owned()),
                Token::PlusEqual,
                Token::Identifier("i".to_owned()),
                Token::SemiColon,
                Token::RightBrace,
            ]
        );
    }
}
