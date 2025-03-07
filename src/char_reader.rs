use std::io::{self, Read};

#[derive(Debug)]
pub enum ReadCharError<'a> {
    Eof,
    BadChar(&'a [u8]),
    IoError(io::Error),
}

pub struct CharReader<R> {
    reader: R,
    num_bytes: usize,
    char_buf: [u8; 4],
}

const MAX_CHAR_LEN: usize = 4;

impl<R: Read> CharReader<R> {
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            num_bytes: 0,
            char_buf: [0; MAX_CHAR_LEN],
        }
    }

    pub fn read_char(&mut self) -> Result<char, ReadCharError> {
        loop {
            if let Some(next) = self.char_buf[0..self.num_bytes].utf8_chunks().next() {
                if let Some(c) = next.valid().chars().next() {
                    let char_len = c.len_utf8();
                    self.num_bytes -= char_len;
                    self.char_buf.rotate_left(char_len);
                    return Ok(c);
                }

                let invalid_len = next.invalid().len();
                if invalid_len < self.num_bytes {
                    self.num_bytes -= invalid_len;
                    self.char_buf.rotate_left(invalid_len);
                    return Err(ReadCharError::BadChar(
                        &self.char_buf[MAX_CHAR_LEN - invalid_len..MAX_CHAR_LEN],
                    ));
                }

                // `slice::utf8_chunks` maximum invalid char length is 3,
                debug_assert!(self.num_bytes != MAX_CHAR_LEN);
            }

            let len = loop {
                match self.reader.read(&mut self.char_buf[self.num_bytes..]) {
                    Ok(len) => break len,
                    Err(e) => {
                        if e.kind() != io::ErrorKind::Interrupted {
                            return Err(ReadCharError::IoError(e));
                        }
                    }
                }
            };

            if len == 0 {
                if self.num_bytes == 0 {
                    return Err(ReadCharError::Eof);
                } else {
                    let bad_len = self.num_bytes;
                    self.num_bytes = 0;
                    return Err(ReadCharError::BadChar(&self.char_buf[0..bad_len]));
                }
            }

            self.num_bytes += len;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_char() {
        let mut test_bytes = Vec::new();
        test_bytes.extend_from_slice(&[255, 254]);
        test_bytes.extend_from_slice("foo".as_bytes());
        test_bytes.extend_from_slice(&[254, 255]);
        test_bytes.extend_from_slice("東京".as_bytes());
        test_bytes.extend_from_slice("💣".as_bytes());
        test_bytes.extend_from_slice(&[240]);

        let mut reader = CharReader::new(test_bytes.as_slice());
        assert!(matches!(
            reader.read_char(),
            Err(ReadCharError::BadChar(&[255]))
        ));
        assert!(matches!(
            reader.read_char(),
            Err(ReadCharError::BadChar(&[254]))
        ));

        assert!(matches!(reader.read_char(), Ok('f')));
        assert!(matches!(reader.read_char(), Ok('o')));
        assert!(matches!(reader.read_char(), Ok('o')));

        assert!(matches!(
            reader.read_char(),
            Err(ReadCharError::BadChar(&[254]))
        ));
        assert!(matches!(
            reader.read_char(),
            Err(ReadCharError::BadChar(&[255]))
        ));

        assert!(matches!(reader.read_char(), Ok('東')));
        assert!(matches!(reader.read_char(), Ok('京')));

        assert!(matches!(reader.read_char(), Ok('💣')));
        assert!(matches!(
            reader.read_char(),
            Err(ReadCharError::BadChar(&[240]))
        ));

        assert!(matches!(reader.read_char(), Err(ReadCharError::Eof)));
    }
}
