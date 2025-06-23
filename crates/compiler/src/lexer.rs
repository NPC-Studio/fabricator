use std::mem;

use arrayvec::ArrayVec;
use fabricator_vm::Span;
use gc_arena::Collect;
use thiserror::Error;

use crate::string_interner::StringInterner;

#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub enum TokenKind<S> {
    EndOfStream,
    Newline,

    Macro,

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

    Enum,
    Function,
    Var,

    Switch,
    Case,
    Break,
    If,
    Else,
    For,
    Repeat,
    Return,
    Exit,

    Undefined,
    True,
    False,

    This,

    Integer(u64),
    Float(f64),
    Identifier(S),
    String(S),
}

impl<S> TokenKind<S> {
    pub fn as_string_ref(&self) -> TokenKind<&S> {
        match self {
            TokenKind::EndOfStream => TokenKind::EndOfStream,
            TokenKind::Newline => TokenKind::Newline,
            TokenKind::Macro => TokenKind::Macro,
            TokenKind::LeftParen => TokenKind::LeftParen,
            TokenKind::RightParen => TokenKind::RightParen,
            TokenKind::LeftBracket => TokenKind::LeftBracket,
            TokenKind::RightBracket => TokenKind::RightBracket,
            TokenKind::LeftBrace => TokenKind::LeftBrace,
            TokenKind::RightBrace => TokenKind::RightBrace,
            TokenKind::Colon => TokenKind::Colon,
            TokenKind::SemiColon => TokenKind::SemiColon,
            TokenKind::Comma => TokenKind::Comma,
            TokenKind::Dot => TokenKind::Dot,
            TokenKind::Plus => TokenKind::Plus,
            TokenKind::Minus => TokenKind::Minus,
            TokenKind::Bang => TokenKind::Bang,
            TokenKind::Slash => TokenKind::Slash,
            TokenKind::Star => TokenKind::Star,
            TokenKind::Mod => TokenKind::Mod,
            TokenKind::Div => TokenKind::Div,
            TokenKind::Percent => TokenKind::Percent,
            TokenKind::Ampersand => TokenKind::Ampersand,
            TokenKind::Pipe => TokenKind::Pipe,
            TokenKind::Equal => TokenKind::Equal,
            TokenKind::PlusEqual => TokenKind::PlusEqual,
            TokenKind::MinusEqual => TokenKind::MinusEqual,
            TokenKind::StarEqual => TokenKind::StarEqual,
            TokenKind::SlashEqual => TokenKind::SlashEqual,
            TokenKind::PercentEqual => TokenKind::PercentEqual,
            TokenKind::DoubleEqual => TokenKind::DoubleEqual,
            TokenKind::BangEqual => TokenKind::BangEqual,
            TokenKind::Less => TokenKind::Less,
            TokenKind::LessEqual => TokenKind::LessEqual,
            TokenKind::Greater => TokenKind::Greater,
            TokenKind::GreaterEqual => TokenKind::GreaterEqual,
            TokenKind::DoublePlus => TokenKind::DoublePlus,
            TokenKind::DoubleMinus => TokenKind::DoubleMinus,
            TokenKind::DoubleAmpersand => TokenKind::DoubleAmpersand,
            TokenKind::DoublePipe => TokenKind::DoublePipe,
            TokenKind::Enum => TokenKind::Enum,
            TokenKind::Function => TokenKind::Function,
            TokenKind::Var => TokenKind::Var,
            TokenKind::Switch => TokenKind::Switch,
            TokenKind::Case => TokenKind::Case,
            TokenKind::Break => TokenKind::Break,
            TokenKind::If => TokenKind::If,
            TokenKind::Else => TokenKind::Else,
            TokenKind::For => TokenKind::For,
            TokenKind::Repeat => TokenKind::Repeat,
            TokenKind::Return => TokenKind::Return,
            TokenKind::Exit => TokenKind::Exit,
            TokenKind::Undefined => TokenKind::Undefined,
            TokenKind::True => TokenKind::True,
            TokenKind::False => TokenKind::False,
            TokenKind::This => TokenKind::This,
            TokenKind::Integer(i) => TokenKind::Integer(*i),
            TokenKind::Float(f) => TokenKind::Float(*f),
            TokenKind::Identifier(i) => TokenKind::Identifier(i),
            TokenKind::String(s) => TokenKind::String(s),
        }
    }

    pub fn map_string<S2>(self, map: impl Fn(S) -> S2) -> TokenKind<S2> {
        match self {
            TokenKind::EndOfStream => TokenKind::EndOfStream,
            TokenKind::Newline => TokenKind::Newline,
            TokenKind::Macro => TokenKind::Macro,
            TokenKind::LeftParen => TokenKind::LeftParen,
            TokenKind::RightParen => TokenKind::RightParen,
            TokenKind::LeftBracket => TokenKind::LeftBracket,
            TokenKind::RightBracket => TokenKind::RightBracket,
            TokenKind::LeftBrace => TokenKind::LeftBrace,
            TokenKind::RightBrace => TokenKind::RightBrace,
            TokenKind::Colon => TokenKind::Colon,
            TokenKind::SemiColon => TokenKind::SemiColon,
            TokenKind::Comma => TokenKind::Comma,
            TokenKind::Dot => TokenKind::Dot,
            TokenKind::Plus => TokenKind::Plus,
            TokenKind::Minus => TokenKind::Minus,
            TokenKind::Bang => TokenKind::Bang,
            TokenKind::Slash => TokenKind::Slash,
            TokenKind::Star => TokenKind::Star,
            TokenKind::Mod => TokenKind::Mod,
            TokenKind::Div => TokenKind::Div,
            TokenKind::Percent => TokenKind::Percent,
            TokenKind::Ampersand => TokenKind::Ampersand,
            TokenKind::Pipe => TokenKind::Pipe,
            TokenKind::Equal => TokenKind::Equal,
            TokenKind::PlusEqual => TokenKind::PlusEqual,
            TokenKind::MinusEqual => TokenKind::MinusEqual,
            TokenKind::StarEqual => TokenKind::StarEqual,
            TokenKind::SlashEqual => TokenKind::SlashEqual,
            TokenKind::PercentEqual => TokenKind::PercentEqual,
            TokenKind::DoubleEqual => TokenKind::DoubleEqual,
            TokenKind::BangEqual => TokenKind::BangEqual,
            TokenKind::Less => TokenKind::Less,
            TokenKind::LessEqual => TokenKind::LessEqual,
            TokenKind::Greater => TokenKind::Greater,
            TokenKind::GreaterEqual => TokenKind::GreaterEqual,
            TokenKind::DoublePlus => TokenKind::DoublePlus,
            TokenKind::DoubleMinus => TokenKind::DoubleMinus,
            TokenKind::DoubleAmpersand => TokenKind::DoubleAmpersand,
            TokenKind::DoublePipe => TokenKind::DoublePipe,
            TokenKind::Enum => TokenKind::Enum,
            TokenKind::Function => TokenKind::Function,
            TokenKind::Var => TokenKind::Var,
            TokenKind::Switch => TokenKind::Switch,
            TokenKind::Case => TokenKind::Case,
            TokenKind::Break => TokenKind::Break,
            TokenKind::If => TokenKind::If,
            TokenKind::Else => TokenKind::Else,
            TokenKind::For => TokenKind::For,
            TokenKind::Repeat => TokenKind::Repeat,
            TokenKind::Return => TokenKind::Return,
            TokenKind::Exit => TokenKind::Exit,
            TokenKind::Undefined => TokenKind::Undefined,
            TokenKind::True => TokenKind::True,
            TokenKind::False => TokenKind::False,
            TokenKind::This => TokenKind::This,
            TokenKind::Integer(i) => TokenKind::Integer(i),
            TokenKind::Float(f) => TokenKind::Float(f),
            TokenKind::Identifier(i) => TokenKind::Identifier(map(i)),
            TokenKind::String(s) => TokenKind::String(map(s)),
        }
    }
}

impl<R, S: PartialEq<R>> PartialEq<TokenKind<R>> for TokenKind<S> {
    fn eq(&self, other: &TokenKind<R>) -> bool {
        match (self, other) {
            (TokenKind::EndOfStream, TokenKind::EndOfStream) => true,
            (TokenKind::EndOfStream, _) => false,
            (TokenKind::Newline, TokenKind::Newline) => true,
            (TokenKind::Newline, _) => false,
            (TokenKind::Macro, TokenKind::Macro) => true,
            (TokenKind::Macro, _) => false,
            (TokenKind::LeftParen, TokenKind::LeftParen) => true,
            (TokenKind::LeftParen, _) => false,
            (TokenKind::RightParen, TokenKind::RightParen) => true,
            (TokenKind::RightParen, _) => false,
            (TokenKind::LeftBracket, TokenKind::LeftBracket) => true,
            (TokenKind::LeftBracket, _) => false,
            (TokenKind::RightBracket, TokenKind::RightBracket) => true,
            (TokenKind::RightBracket, _) => false,
            (TokenKind::LeftBrace, TokenKind::LeftBrace) => true,
            (TokenKind::LeftBrace, _) => false,
            (TokenKind::RightBrace, TokenKind::RightBrace) => true,
            (TokenKind::RightBrace, _) => false,
            (TokenKind::Colon, TokenKind::Colon) => true,
            (TokenKind::Colon, _) => false,
            (TokenKind::SemiColon, TokenKind::SemiColon) => true,
            (TokenKind::SemiColon, _) => false,
            (TokenKind::Comma, TokenKind::Comma) => true,
            (TokenKind::Comma, _) => false,
            (TokenKind::Dot, TokenKind::Dot) => true,
            (TokenKind::Dot, _) => false,
            (TokenKind::Plus, TokenKind::Plus) => true,
            (TokenKind::Plus, _) => false,
            (TokenKind::Minus, TokenKind::Minus) => true,
            (TokenKind::Minus, _) => false,
            (TokenKind::Bang, TokenKind::Bang) => true,
            (TokenKind::Bang, _) => false,
            (TokenKind::Slash, TokenKind::Slash) => true,
            (TokenKind::Slash, _) => false,
            (TokenKind::Star, TokenKind::Star) => true,
            (TokenKind::Star, _) => false,
            (TokenKind::Mod, TokenKind::Mod) => true,
            (TokenKind::Mod, _) => false,
            (TokenKind::Div, TokenKind::Div) => true,
            (TokenKind::Div, _) => false,
            (TokenKind::Percent, TokenKind::Percent) => true,
            (TokenKind::Percent, _) => false,
            (TokenKind::Ampersand, TokenKind::Ampersand) => true,
            (TokenKind::Ampersand, _) => false,
            (TokenKind::Pipe, TokenKind::Pipe) => true,
            (TokenKind::Pipe, _) => false,
            (TokenKind::Equal, TokenKind::Equal) => true,
            (TokenKind::Equal, _) => false,
            (TokenKind::PlusEqual, TokenKind::PlusEqual) => true,
            (TokenKind::PlusEqual, _) => false,
            (TokenKind::MinusEqual, TokenKind::MinusEqual) => true,
            (TokenKind::MinusEqual, _) => false,
            (TokenKind::StarEqual, TokenKind::StarEqual) => true,
            (TokenKind::StarEqual, _) => false,
            (TokenKind::SlashEqual, TokenKind::SlashEqual) => true,
            (TokenKind::SlashEqual, _) => false,
            (TokenKind::PercentEqual, TokenKind::PercentEqual) => true,
            (TokenKind::PercentEqual, _) => false,
            (TokenKind::DoubleEqual, TokenKind::DoubleEqual) => true,
            (TokenKind::DoubleEqual, _) => false,
            (TokenKind::BangEqual, TokenKind::BangEqual) => true,
            (TokenKind::BangEqual, _) => false,
            (TokenKind::Less, TokenKind::Less) => true,
            (TokenKind::Less, _) => false,
            (TokenKind::LessEqual, TokenKind::LessEqual) => true,
            (TokenKind::LessEqual, _) => false,
            (TokenKind::Greater, TokenKind::Greater) => true,
            (TokenKind::Greater, _) => false,
            (TokenKind::GreaterEqual, TokenKind::GreaterEqual) => true,
            (TokenKind::GreaterEqual, _) => false,
            (TokenKind::DoublePlus, TokenKind::DoublePlus) => true,
            (TokenKind::DoublePlus, _) => false,
            (TokenKind::DoubleMinus, TokenKind::DoubleMinus) => true,
            (TokenKind::DoubleMinus, _) => false,
            (TokenKind::DoubleAmpersand, TokenKind::DoubleAmpersand) => true,
            (TokenKind::DoubleAmpersand, _) => false,
            (TokenKind::DoublePipe, TokenKind::DoublePipe) => true,
            (TokenKind::DoublePipe, _) => false,
            (TokenKind::Enum, TokenKind::Enum) => true,
            (TokenKind::Enum, _) => false,
            (TokenKind::Function, TokenKind::Function) => true,
            (TokenKind::Function, _) => false,
            (TokenKind::Var, TokenKind::Var) => true,
            (TokenKind::Var, _) => false,
            (TokenKind::Switch, TokenKind::Switch) => true,
            (TokenKind::Switch, _) => false,
            (TokenKind::Case, TokenKind::Case) => true,
            (TokenKind::Case, _) => false,
            (TokenKind::Break, TokenKind::Break) => true,
            (TokenKind::Break, _) => false,
            (TokenKind::If, TokenKind::If) => true,
            (TokenKind::If, _) => false,
            (TokenKind::Else, TokenKind::Else) => true,
            (TokenKind::Else, _) => false,
            (TokenKind::For, TokenKind::For) => true,
            (TokenKind::For, _) => false,
            (TokenKind::Repeat, TokenKind::Repeat) => true,
            (TokenKind::Repeat, _) => false,
            (TokenKind::Return, TokenKind::Return) => true,
            (TokenKind::Return, _) => false,
            (TokenKind::Exit, TokenKind::Exit) => true,
            (TokenKind::Exit, _) => false,
            (TokenKind::Undefined, TokenKind::Undefined) => true,
            (TokenKind::Undefined, _) => false,
            (TokenKind::True, TokenKind::True) => true,
            (TokenKind::True, _) => false,
            (TokenKind::False, TokenKind::False) => true,
            (TokenKind::False, _) => false,
            (TokenKind::This, TokenKind::This) => true,
            (TokenKind::This, _) => false,
            (TokenKind::Integer(a), TokenKind::Integer(b)) => a == b,
            (TokenKind::Integer(_), _) => false,
            (TokenKind::Float(a), TokenKind::Float(b)) => a.to_bits() == b.to_bits(),
            (TokenKind::Float(_), _) => false,
            (TokenKind::Identifier(a), TokenKind::Identifier(b)) => a == b,
            (TokenKind::Identifier(_), _) => false,
            (TokenKind::String(a), TokenKind::String(b)) => a == b,
            (TokenKind::String(_), _) => false,
        }
    }
}

impl<S: Eq> Eq for TokenKind<S> {}

#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Token<S> {
    pub kind: TokenKind<S>,
    pub span: Span,
}

#[derive(Debug, Error)]
pub enum LexErrorKind {
    #[error("expected matching '\"' for string")]
    UnfinishedString,
    #[error("invalid string escape char {0:?}")]
    InvalidStringEscape(char),
    #[error("unexpected character: {0:?}")]
    UnexpectedCharacter(char),
    #[error("no such # directive \"#{0}\"")]
    BadDirective(String),
    #[error("malformed number")]
    BadNumber,
}

#[derive(Debug, Error)]
#[error("{kind}")]
pub struct LexError {
    pub kind: LexErrorKind,
    pub span: Span,
}

pub struct Lexer<'a, S> {
    interner: S,
    source: &'a str,
    peek_buffer: ArrayVec<char, 2>,
    string_buffer: String,
    position: usize,
}

impl<'a, S> Lexer<'a, S>
where
    S: StringInterner,
{
    /// Lexes the provided source completely, placing lexed tokens into the provided buffer.
    ///
    /// Always pushes a single [`TokenKind::EndOfStream`] token at the end of the stream.
    pub fn tokenize(
        interner: S,
        source: &'a str,
        tokens: &mut Vec<Token<S::String>>,
    ) -> Result<(), LexError> {
        let mut this = Self::new(interner, source);
        loop {
            let token = this.read_token()?;
            let at_end = matches!(token.kind, TokenKind::EndOfStream);
            tokens.push(token);
            if at_end {
                break;
            }
        }
        Ok(())
    }

    pub fn new(interner: S, source: &'a str) -> Lexer<'a, S> {
        Lexer {
            interner,
            source,
            peek_buffer: ArrayVec::new(),
            string_buffer: String::new(),
            position: 0,
        }
    }

    /// Returns the current byte position in the source file.
    pub fn position(&self) -> usize {
        self.position
    }

    /// Skips any leading whitespace before the next token in the stream.
    ///
    /// It is not necessary to call this explicitly, but doing so will make the current byte
    /// position accurate for the start of the next returned token.
    pub fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek(0) {
            let nc = self.peek(1);

            match (c, nc) {
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
                            (None, _) => break,
                            _ => self.advance(1),
                        }
                    }
                }
                _ if c.is_ascii_whitespace() && !is_newline(c) => {
                    self.advance(1);
                }
                _ => break,
            }
        }
    }

    /// Read and return the next token in the stream.
    ///
    /// Returns `TokenKind::EndOfStream` once the token stream is finished.
    pub fn read_token(&mut self) -> Result<Token<S::String>, LexError> {
        self.skip_whitespace();

        let start_position = self.position;
        let kind = match (self.peek(0), self.peek(1)) {
            (Some(c), n) if is_newline(c) => {
                self.advance(1);
                // If we have a second newline character immediately following the first one and
                // it is a *different* newline character, then lex the pair "\n\r" or "\r\n" as a
                // single newline.
                if n.is_some_and(|nc| is_newline(nc) && nc != c) {
                    self.advance(1);
                }
                TokenKind::Newline
            }
            (Some(c), _) if c.is_ascii_whitespace() => {
                unreachable!("whitespace should have been skipped")
            }
            (Some(c), _) if c == '#' => {
                self.advance(1);
                self.read_identifier();
                match self.string_buffer.as_str() {
                    "macro" => TokenKind::Macro,
                    _ => {
                        return Err(LexError {
                            kind: LexErrorKind::BadDirective(mem::take(&mut self.string_buffer)),
                            span: Span::new(start_position, self.position),
                        });
                    }
                }
            }
            (Some('!'), Some('=')) => {
                self.advance(2);
                TokenKind::BangEqual
            }
            (Some('='), Some('=')) => {
                self.advance(2);
                TokenKind::DoubleEqual
            }
            (Some('+'), Some('=')) => {
                self.advance(2);
                TokenKind::PlusEqual
            }
            (Some('-'), Some('=')) => {
                self.advance(2);
                TokenKind::MinusEqual
            }
            (Some('*'), Some('=')) => {
                self.advance(2);
                TokenKind::StarEqual
            }
            (Some('/'), Some('=')) => {
                self.advance(2);
                TokenKind::SlashEqual
            }
            (Some('%'), Some('=')) => {
                self.advance(2);
                TokenKind::PercentEqual
            }
            (Some('<'), Some('=')) => {
                self.advance(2);
                TokenKind::LessEqual
            }
            (Some('>'), Some('=')) => {
                self.advance(2);
                TokenKind::GreaterEqual
            }
            (Some('+'), Some('+')) => {
                self.advance(2);
                TokenKind::DoublePlus
            }
            (Some('-'), Some('-')) => {
                self.advance(2);
                TokenKind::DoubleMinus
            }
            (Some('&'), Some('&')) => {
                self.advance(2);
                TokenKind::DoubleAmpersand
            }
            (Some('|'), Some('|')) => {
                self.advance(2);
                TokenKind::DoublePipe
            }
            (Some('('), _) => {
                self.advance(1);
                TokenKind::LeftParen
            }
            (Some(')'), _) => {
                self.advance(1);
                TokenKind::RightParen
            }
            (Some('['), _) => {
                self.advance(1);
                TokenKind::LeftBracket
            }
            (Some(']'), _) => {
                self.advance(1);
                TokenKind::RightBracket
            }
            (Some('{'), _) => {
                self.advance(1);
                TokenKind::LeftBrace
            }
            (Some('}'), _) => {
                self.advance(1);
                TokenKind::RightBrace
            }
            (Some(':'), _) => {
                self.advance(1);
                TokenKind::Colon
            }
            (Some(';'), _) => {
                self.advance(1);
                TokenKind::SemiColon
            }
            (Some(','), _) => {
                self.advance(1);
                TokenKind::Comma
            }
            (Some('.'), _) => {
                self.advance(1);
                TokenKind::Dot
            }
            (Some('+'), _) => {
                self.advance(1);
                TokenKind::Plus
            }
            (Some('-'), _) => {
                self.advance(1);
                TokenKind::Minus
            }
            (Some('!'), _) => {
                self.advance(1);
                TokenKind::Bang
            }
            (Some('/'), _) => {
                self.advance(1);
                TokenKind::Slash
            }
            (Some('*'), _) => {
                self.advance(1);
                TokenKind::Star
            }
            (Some('%'), _) => {
                self.advance(1);
                TokenKind::Percent
            }
            (Some('&'), _) => {
                self.advance(1);
                TokenKind::Ampersand
            }
            (Some('|'), _) => {
                self.advance(1);
                TokenKind::Pipe
            }
            (Some('='), _) => {
                self.advance(1);
                TokenKind::Equal
            }
            (Some('<'), _) => {
                self.advance(1);
                TokenKind::Less
            }
            (Some('>'), _) => {
                self.advance(1);
                TokenKind::Greater
            }
            (Some('"'), _) => {
                self.read_string()?;
                TokenKind::String(self.interner.intern(self.string_buffer.as_str()))
            }
            (Some(c), _) if is_identifier_start_char(c) => {
                self.read_identifier();
                match self.string_buffer.as_str() {
                    "enum" => TokenKind::Enum,
                    "function" => TokenKind::Function,
                    "var" => TokenKind::Var,
                    "switch" => TokenKind::Switch,
                    "case" => TokenKind::Case,
                    "break" => TokenKind::Break,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "for" => TokenKind::For,
                    "repeat" => TokenKind::Repeat,
                    "return" => TokenKind::Return,
                    "exit" => TokenKind::Exit,
                    "undefined" => TokenKind::Undefined,
                    "true" => TokenKind::True,
                    "false" => TokenKind::False,
                    "self" => TokenKind::This,
                    id => TokenKind::Identifier(self.interner.intern(id)),
                }
            }
            (Some(c), _) if c.is_ascii_digit() => self.read_numeral()?,
            (Some(c), _) => {
                return Err(LexError {
                    kind: LexErrorKind::UnexpectedCharacter(c),
                    span: Span::new(start_position, start_position + 1),
                });
            }
            (None, _) => TokenKind::EndOfStream,
        };

        Ok(Token {
            kind,
            span: Span::new(start_position, self.position),
        })
    }

    /// Read an identifier into the string buffer.
    fn read_identifier(&mut self) {
        let start = self.peek(0).unwrap();
        assert!(is_identifier_start_char(start));

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

    /// Read a short (not multi-line) string literal into the string buffer.
    fn read_string(&mut self) -> Result<(), LexError> {
        assert!(self.peek(0).unwrap() == '"');
        self.advance(1);

        self.string_buffer.clear();

        loop {
            let c = if let Some(c) = self.peek(0) {
                c
            } else {
                return Err(LexError {
                    kind: LexErrorKind::UnfinishedString,
                    span: Span::empty(self.position),
                });
            };

            if is_newline(c) {
                return Err(LexError {
                    kind: LexErrorKind::UnfinishedString,
                    span: Span::empty(self.position),
                });
            }

            self.advance(1);
            if c == '\\' {
                match self.peek(0).ok_or(LexError {
                    kind: LexErrorKind::UnfinishedString,
                    span: Span::empty(self.position),
                })? {
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

                    c => {
                        return Err(LexError {
                            kind: LexErrorKind::InvalidStringEscape(c),
                            span: Span::new(self.position - 1, self.position + 1),
                        });
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

    /// Reads a hex or decimal integer or floating point number. Allows decimal integers (123), hex
    /// integers (0xdeadbeef), and decimal floating point with optional exponent and exponent sign
    /// (3.21e+1).
    fn read_numeral(&mut self) -> Result<TokenKind<S::String>, LexError> {
        let start_position = self.position;

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

        let span = Span::new(start_position, self.position);

        if !has_exp && !has_radix {
            if let Some(i) = if is_hex {
                read_hex_integer(&self.string_buffer)
            } else {
                read_dec_integer(&self.string_buffer)
            } {
                return Ok(TokenKind::Integer(i));
            }

            // Fall back to parsing as a float if parsing as an integer fails...
        }

        if is_hex {
            // Hex floats are not supported
            Err(LexError {
                kind: LexErrorKind::BadNumber,
                span,
            })
        } else {
            Ok(TokenKind::Float(
                read_dec_float(&self.string_buffer).ok_or(LexError {
                    kind: LexErrorKind::BadNumber,
                    span,
                })?,
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

    /// Advance the character stream `n` characters.
    ///
    /// # Panics
    ///
    /// Panics if this would advance over characters that have not been observed with `Lexer::peek`.
    fn advance(&mut self, n: usize) {
        assert!(
            n <= self.peek_buffer.len(),
            "cannot advance over un-peeked characters"
        );
        self.position += n;
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

    fn lex(source: &str) -> Result<Vec<TokenKind<String>>, LexError> {
        struct SimpleInterner;

        impl StringInterner for SimpleInterner {
            type String = String;

            fn intern(&mut self, s: &str) -> Self::String {
                s.to_owned()
            }
        }

        let mut tokens = Vec::new();
        let mut lexer = Lexer::new(SimpleInterner, source);
        loop {
            let token = lexer.read_token()?;
            match &token.kind {
                TokenKind::Newline => {}
                TokenKind::EndOfStream => break,
                _ => {
                    tokens.push(token.kind);
                }
            }
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
                TokenKind::Var,
                TokenKind::Identifier("sum".to_owned()),
                TokenKind::Equal,
                TokenKind::Integer(0),
                TokenKind::SemiColon,
                TokenKind::For,
                TokenKind::LeftParen,
                TokenKind::Var,
                TokenKind::Identifier("i".to_owned()),
                TokenKind::Equal,
                TokenKind::Integer(0),
                TokenKind::SemiColon,
                TokenKind::Identifier("i".to_owned()),
                TokenKind::Less,
                TokenKind::Integer(1000000),
                TokenKind::SemiColon,
                TokenKind::DoublePlus,
                TokenKind::Identifier("i".to_owned()),
                TokenKind::RightParen,
                TokenKind::LeftBrace,
                TokenKind::Identifier("sum".to_owned()),
                TokenKind::PlusEqual,
                TokenKind::Identifier("i".to_owned()),
                TokenKind::SemiColon,
                TokenKind::RightBrace,
            ]
        );
    }
}
