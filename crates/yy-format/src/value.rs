use std::{collections::HashMap, fmt, mem};

use thiserror::Error;

pub type Object = HashMap<String, Value>;
pub type Array = Vec<Value>;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Object(Object),
    Array(Array),
}

impl Value {
    pub fn parse(src: &str) -> Result<Self, ParseError> {
        Parser::new(src).parse()
    }

    pub fn to_string_pretty(&self) -> String {
        fn write_indent(f: &mut dyn fmt::Write, indent: usize) -> fmt::Result {
            write!(f, "{:indent$}", "")?;
            Ok(())
        }

        fn write_value(f: &mut dyn fmt::Write, val: &Value, indent: usize) -> fmt::Result {
            match val {
                Value::Null => write!(f, "null"),
                Value::Boolean(b) => write!(f, "{}", b),
                Value::Integer(i) => write!(f, "{}", i),
                Value::Float(n) => {
                    assert!(n.is_finite());
                    write!(f, "{}", n)
                }
                Value::String(s) => write!(f, "\"{}\"", s),
                Value::Object(obj) => {
                    writeln!(f, "{{")?;

                    for (key, val) in obj {
                        write_indent(f, indent + 4)?;
                        write!(f, "\"{}\": ", key)?;
                        write_value(f, val, indent + 4)?;
                        writeln!(f, ",")?;
                    }

                    write_indent(f, indent)?;
                    write!(f, "}}")
                }
                Value::Array(arr) => {
                    writeln!(f, "[")?;

                    for val in arr {
                        write_indent(f, indent + 4)?;
                        write_value(f, val, indent + 4)?;
                        writeln!(f, ",")?;
                    }

                    write_indent(f, indent)?;
                    write!(f, "]")
                }
            }
        }

        let mut buf = String::new();
        write_value(&mut buf, self, 0).unwrap();
        buf
    }
}

#[derive(Debug, Error)]
pub enum ParseErrorKind {
    #[error("unexpected <eof>")]
    UnexpectedEof,
    #[error("invalid string escape sequence `\\{0}`")]
    InvalidStringEscape(char),
    #[error("malformed number `{0:?}`")]
    BadNumber(String),
    #[error("invalid identifier {0:?}")]
    InvalidIdentifier(String),
    #[error("unexpected character: {0:?}")]
    UnexpectedChar(char),
}

#[derive(Debug, Error)]
#[error("parse error at line {}: {}", .line_number + 1, .kind)]
pub struct ParseError {
    #[source]
    kind: ParseErrorKind,
    line_number: usize,
}

pub struct Parser<'a> {
    source: &'a str,
    peek_buffer: Vec<char>,
    last_newline: Option<char>,
    string_buffer: String,
    line_number: usize,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Parser<'a> {
        Parser {
            source,
            peek_buffer: Vec::new(),
            last_newline: None,
            string_buffer: String::new(),
            line_number: 0,
        }
    }

    fn parse(&mut self) -> Result<Value, ParseError> {
        match self.read_top() {
            Ok(val) => Ok(val),
            Err(kind) => Err(ParseError {
                kind,
                line_number: self.line_number,
            }),
        }
    }

    fn read_top(&mut self) -> Result<Value, ParseErrorKind> {
        self.skip_whitespace();
        let val = self.read_value()?;
        self.skip_whitespace();
        if let Some(n) = self.peek(0) {
            return Err(ParseErrorKind::UnexpectedChar(n));
        }

        Ok(val)
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek(0) {
            if c.is_ascii_whitespace() {
                self.advance(1);
            } else {
                break;
            }
        }
    }

    fn read_value(&mut self) -> Result<Value, ParseErrorKind> {
        match self.peek(0).ok_or(ParseErrorKind::UnexpectedEof)? {
            '{' => Ok(Value::Object(self.read_object()?)),
            '[' => Ok(Value::Array(self.read_array()?)),
            '"' => {
                self.read_string()?;
                Ok(Value::String(mem::take(&mut self.string_buffer)))
            }
            c if c == '-' || c.is_ascii_digit() => self.read_number(),
            c if c.is_ascii_alphabetic() => {
                self.read_identifier()?;
                match self.string_buffer.as_str() {
                    "null" => Ok(Value::Null),
                    "true" => Ok(Value::Boolean(true)),
                    "false" => Ok(Value::Boolean(false)),
                    _ => Err(ParseErrorKind::InvalidIdentifier(mem::take(
                        &mut self.string_buffer,
                    ))),
                }
            }
            c => Err(ParseErrorKind::UnexpectedChar(c)),
        }
    }

    fn read_object(&mut self) -> Result<Object, ParseErrorKind> {
        self.expect_char('{')?;
        self.skip_whitespace();

        let mut obj = Object::new();
        let mut has_separator = true;

        loop {
            let next = self.peek(0).ok_or(ParseErrorKind::UnexpectedEof)?;
            if next == '}' {
                self.advance(1);
                return Ok(obj);
            }

            if !has_separator {
                return Err(ParseErrorKind::UnexpectedChar(next));
            }

            self.read_string()?;
            let key = mem::take(&mut self.string_buffer);

            self.skip_whitespace();
            self.expect_char(':')?;

            self.skip_whitespace();
            let value = self.read_value()?;

            obj.insert(key, value);

            self.skip_whitespace();
            if self.peek(0) == Some(',') {
                self.advance(1);
                has_separator = true;
                self.skip_whitespace();
            } else {
                has_separator = false;
            }
        }
    }

    fn read_array(&mut self) -> Result<Array, ParseErrorKind> {
        self.expect_char('[')?;
        self.skip_whitespace();

        let mut array = Array::new();
        let mut has_separator = true;

        loop {
            let next = self.peek(0).ok_or(ParseErrorKind::UnexpectedEof)?;
            if next == ']' {
                self.advance(1);
                return Ok(array);
            }

            if !has_separator {
                return Err(ParseErrorKind::UnexpectedChar(next));
            }

            let value = self.read_value()?;
            array.push(value);

            self.skip_whitespace();
            if self.peek(0) == Some(',') {
                self.advance(1);
                has_separator = true;
                self.skip_whitespace();
            } else {
                has_separator = false;
            }
        }
    }

    fn read_identifier(&mut self) -> Result<(), ParseErrorKind> {
        let start = self.peek(0).unwrap();
        if !start.is_ascii_alphabetic() {
            return Err(ParseErrorKind::UnexpectedChar(start));
        }

        self.string_buffer.clear();
        self.string_buffer.push(start);
        self.advance(1);

        while let Some(c) = self.peek(0) {
            if !c.is_ascii_alphabetic() {
                break;
            }

            self.string_buffer.push(c);
            self.advance(1);
        }

        Ok(())
    }

    fn read_string(&mut self) -> Result<(), ParseErrorKind> {
        self.expect_char('"')?;
        self.string_buffer.clear();

        loop {
            let c = if let Some(c) = self.peek(0) {
                c
            } else {
                return Err(ParseErrorKind::UnexpectedEof);
            };

            self.advance(1);
            if c == '\\' {
                match self.peek(0).ok_or(ParseErrorKind::UnexpectedEof)? {
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
                        return Err(ParseErrorKind::InvalidStringEscape(c));
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

    fn read_number(&mut self) -> Result<Value, ParseErrorKind> {
        match self.peek(0).ok_or(ParseErrorKind::UnexpectedEof)? {
            c if c == '-' => {
                self.string_buffer.clear();
                self.advance(1);
            }
            c if c.is_ascii_digit() => {
                self.string_buffer.clear();
                self.string_buffer.push(c);
                self.advance(1);
            }
            c => return Err(ParseErrorKind::UnexpectedChar(c)),
        }

        let mut has_radix = false;
        while let Some(c) = self.peek(0) {
            if c == '.' && !has_radix {
                self.string_buffer.push('.');
                has_radix = true;
                self.advance(1);
            } else if c.is_ascii_digit() {
                self.string_buffer.push(c);
                self.advance(1);
            } else {
                break;
            }
        }

        let mut has_exp = false;
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

        if !has_exp && !has_radix {
            if let Ok(i) = str::parse(&self.string_buffer) {
                return Ok(Value::Integer(i));
            }

            // Fall back to parsing as a float if parsing as an integer fails...
        }

        Ok(Value::Float(str::parse(&self.string_buffer).map_err(
            |_| ParseErrorKind::BadNumber(mem::take(&mut self.string_buffer)),
        )?))
    }

    fn expect_char(&mut self, c: char) -> Result<(), ParseErrorKind> {
        let n = self.peek(0).ok_or(ParseErrorKind::UnexpectedEof)?;
        if n != c {
            return Err(ParseErrorKind::UnexpectedChar(n));
        }
        self.advance(1);
        Ok(())
    }

    /// Peek to the `n`th character ahead in the character stream.
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
    /// Panics if this would advance over characters that have not been observed with `Parser::peek`.
    fn advance(&mut self, n: usize) {
        assert!(
            n <= self.peek_buffer.len(),
            "cannot advance over un-peeked characters"
        );

        for c in self.peek_buffer.drain(0..n) {
            if c == '\n' || c == '\r' {
                // Don't increment the line number again if the last character was also a newline
                // char that is different from the current one. We assume that these are newline
                // pairs (e.g. `"\r\n"`).
                if self.last_newline.is_some_and(|last_nl| c != last_nl) {
                    self.last_newline = None;
                } else {
                    self.line_number += 1;
                    self.last_newline = Some(c);
                }
            } else {
                self.last_newline = None;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_yy_parse() {
        assert_eq!(
            Value::parse(
                r#"
                    {
                        "foo": "bar",
                    }
                
                "#
            )
            .unwrap(),
            Value::Object(
                [("foo".to_owned(), Value::String("bar".to_owned()))]
                    .into_iter()
                    .collect::<Object>()
            )
        );

        assert_eq!(
            Value::parse(
                r#"
                    {
                        "foo": "bar",
                        "baz": "qux"
                    }
                
                "#
            )
            .unwrap(),
            Value::Object(
                [
                    ("foo".to_owned(), Value::String("bar".to_owned())),
                    ("baz".to_owned(), Value::String("qux".to_owned())),
                ]
                .into_iter()
                .collect::<Object>()
            )
        );

        assert_eq!(
            Value::parse(
                r#"
                    {
                        "arr": [
                            "foo",
                            "bar",
                            13,
                        ],
                    }
                
                "#
            )
            .unwrap(),
            Value::Object(
                [(
                    "arr".to_owned(),
                    Value::Array(
                        [
                            Value::String("foo".to_owned()),
                            Value::String("bar".to_owned()),
                            Value::Integer(13),
                        ]
                        .into_iter()
                        .collect()
                    )
                ),]
                .into_iter()
                .collect::<Object>()
            )
        );

        assert_eq!(
            Value::parse(
                r#"
                    {
                        "arr": [
                            true,
                            false,
                            null,
                            0.2
                        ],
                    }
                
                "#
            )
            .unwrap(),
            Value::Object(
                [(
                    "arr".to_owned(),
                    Value::Array(
                        [
                            Value::Boolean(true),
                            Value::Boolean(false),
                            Value::Null,
                            Value::Float(0.2),
                        ]
                        .into_iter()
                        .collect()
                    )
                ),]
                .into_iter()
                .collect::<Object>()
            )
        );

        assert_eq!(
            Value::parse(
                r#"
                    {
                        "field" : 1.2e1,
                    }
                
                "#
            )
            .unwrap(),
            Value::Object(
                [("field".to_owned(), Value::Float(12.0))]
                    .into_iter()
                    .collect::<Object>()
            )
        );
    }
}
