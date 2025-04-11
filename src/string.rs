use std::string::String as StdString;

use gc_arena::{Collect, Gc, Mutation};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Collect)]
#[collect(no_drop)]
pub struct String<'gc>(Gc<'gc, StdString>);

impl<'gc> String<'gc> {
    pub fn new(mc: &Mutation<'gc>, s: &str) -> String<'gc> {
        String(Gc::new(mc, s.to_owned()))
    }
}

impl<'gc> AsRef<str> for String<'gc> {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
