use std::io::{self, BufRead, Read};

pub struct StripJsonTrailingCommas<T> {
    inner: T,
    state: State,
}

enum State {
    Top,
    InString,
    StringEscape,
    CommaPending,
}

impl<T: BufRead> StripJsonTrailingCommas<T> {
    pub fn new(read: T) -> Self {
        Self {
            inner: read,
            state: State::Top,
        }
    }
}

impl<T: BufRead> Read for StripJsonTrailingCommas<T> {
    fn read(&mut self, write_buf: &mut [u8]) -> io::Result<usize> {
        let mut write_pos = 0;
        let mut read_pos = 0;

        let mut read_buf = self.inner.fill_buf()?;

        loop {
            if write_pos == write_buf.len() {
                break;
            }

            if read_buf.is_empty() {
                // If we have reached the end of the read buffer but still have a comma pending, go
                // ahead and write it so that the resulting error message is more accurate.
                if matches!(self.state, State::CommaPending) {
                    write_buf[write_pos] = b',';
                    write_pos += 1;
                }

                break;
            }

            if read_pos == read_buf.len() {
                // We only refill the read buffer if we haven't yet written any characters (to
                // distinguish from EOF), otherwise we can just return.
                if write_pos != 0 {
                    break;
                }

                self.inner.consume(read_pos);
                read_pos = 0;
                read_buf = self.inner.fill_buf()?;
            }

            let mut next_char = read_buf[read_pos];

            let mut write_char = true;
            let mut advance_read = true;

            match self.state {
                State::Top => match next_char {
                    b'"' => {
                        self.state = State::InString;
                    }
                    b',' => {
                        // Delay writing commas until we encounter the next non-whitespace
                        // character, in case we need to remove it if it is a trailing comma.
                        self.state = State::CommaPending;
                        write_char = false;
                    }
                    _ => {}
                },
                State::InString => match next_char {
                    b'\\' => {
                        // Any character following an escape cannot end the current string.
                        self.state = State::StringEscape;
                    }
                    b'"' => {
                        self.state = State::Top;
                    }
                    _ => {}
                },
                State::StringEscape => {
                    self.state = State::InString;
                }
                State::CommaPending => {
                    // If we have a pending comma, scan forward until the next non-whitespace
                    // character. If it is a `]` or a `}`, don't write the comma we skipped,
                    // otherwise, write it.
                    match next_char {
                        c if c.is_ascii_whitespace() => {}
                        b']' | b'}' => {
                            self.state = State::Top;
                        }
                        _ => {
                            advance_read = false;
                            next_char = b',';
                            self.state = State::Top;
                        }
                    }
                }
            }

            if advance_read {
                read_pos += 1;
            }

            if write_char {
                write_buf[write_pos] = next_char;
                write_pos += 1;
            }
        }

        self.inner.consume(read_pos);

        Ok(write_pos)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_strip_json_trailing_commas() {
        #[derive(serde::Deserialize)]
        struct Test {
            a: String,
            b: Vec<i32>,
        }

        let test: Test = serde_json::from_reader(StripJsonTrailingCommas::new(
            br#"
                {
                    "a": "\"\"\\\\\\,],}\"",
                    "b": [,],
                }
            "#
            .as_slice(),
        ))
        .unwrap();

        assert_eq!(test.a, "\"\"\\\\\\,],}\"");
        assert_eq!(test.b, [0i32; 0]);

        let err: Result<Test, _> = serde_json::from_reader(StripJsonTrailingCommas::new(
            br#"
                {
                    "a": "",
                    "b": [,,],
                }
            "#
            .as_slice(),
        ));

        assert!(err.is_err());
    }
}
