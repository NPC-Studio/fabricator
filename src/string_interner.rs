pub trait StringInterner {
    type String: AsRef<str> + Clone;

    fn intern(&mut self, s: &str) -> Self::String;
}
