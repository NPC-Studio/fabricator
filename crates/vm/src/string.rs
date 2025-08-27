use std::{
    borrow::Borrow,
    fmt,
    ops::{self, Deref},
    string::String as StdString,
    sync::Arc,
};

use gc_arena::{Collect, Gc, Mutation};

/// A shared string with 'static lifetime.
#[derive(Debug, Clone, Eq, PartialEq, Hash, Collect)]
#[collect(require_static)]
pub struct SharedStr(Arc<str>);

impl fmt::Display for SharedStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl ops::Deref for SharedStr {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl SharedStr {
    pub fn new(name: impl Into<StdString>) -> Self {
        Self(name.into().into_boxed_str().into())
    }

    pub fn as_str(&self) -> &str {
        self.0.as_ref()
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Collect)]
#[collect(no_drop)]
pub struct String<'gc>(Gc<'gc, SharedStr>);

impl<'gc> fmt::Display for String<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<'gc> String<'gc> {
    pub fn new(mc: &Mutation<'gc>, s: &str) -> String<'gc> {
        String(Gc::new(mc, SharedStr::new(s)))
    }

    pub fn as_str(self) -> &'gc str {
        self.0.as_ref().as_ref()
    }

    pub fn as_shared(self) -> &'gc SharedStr {
        self.0.as_ref()
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
