use std::{borrow::Borrow, fmt, ops::Deref, string::String as StdString};

use gc_arena::{Collect, Gc, Mutation};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Collect)]
#[collect(no_drop)]
pub struct String<'gc>(Gc<'gc, StdString>);

impl<'gc> fmt::Display for String<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<'gc> String<'gc> {
    pub fn new(mc: &Mutation<'gc>, s: &str) -> String<'gc> {
        String(Gc::new(mc, s.to_owned()))
    }

    pub fn as_str(self) -> &'gc str {
        self.0.as_ref().as_str()
    }
}

impl<'gc> AsRef<str> for String<'gc> {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl<'gc> Borrow<str> for String<'gc> {
    fn borrow(&self) -> &str {
        self.as_ref()
    }
}

impl<'gc> Deref for String<'gc> {
    type Target = str;

    fn deref(&self) -> &str {
        self.as_str()
    }
}
