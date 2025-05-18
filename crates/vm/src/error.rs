use std::{error::Error as StdError, fmt};

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
pub struct Error(Box<dyn StdError + Send + Sync>);

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<E: Into<Box<dyn StdError + Send + Sync>>> From<E> for Error {
    fn from(err: E) -> Self {
        Self::new(err)
    }
}

impl Error {
    pub fn new(err: impl Into<Box<dyn StdError + Send + Sync>>) -> Self {
        Self(err.into())
    }

    pub fn into_boxed_err(self) -> Box<dyn StdError + Send + Sync> {
        self.0
    }
}
