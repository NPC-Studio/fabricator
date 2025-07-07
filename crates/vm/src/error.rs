use std::{fmt, ops};

use gc_arena::Collect;
use thiserror::Error;

#[derive(Debug, Clone, Copy, Error)]
#[error("type error, expected {expected}, found {found}")]
pub struct TypeError {
    pub expected: &'static str,
    pub found: &'static str,
}

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct Error(anyhow::Error);

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<E: Into<anyhow::Error>> From<E> for Error {
    fn from(error: E) -> Self {
        Self(error.into())
    }
}

impl Error {
    pub fn new(err: impl Into<anyhow::Error>) -> Self {
        Self(err.into())
    }

    pub fn msg<M>(message: M) -> Self
    where
        M: fmt::Display + fmt::Debug + Send + Sync + 'static,
    {
        Self(anyhow::Error::msg(message))
    }

    pub fn into_inner(self) -> anyhow::Error {
        self.0
    }

    pub fn context<C>(self, context: C) -> Self
    where
        C: fmt::Display + Send + Sync + 'static,
    {
        Self(self.0.context(context))
    }
}

impl ops::Deref for Error {
    type Target = anyhow::Error;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
