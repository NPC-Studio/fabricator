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
pub struct Error(anyhow::Error);

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<E> From<E> for Error
where
    E: StdError + Send + Sync + 'static,
{
    fn from(error: E) -> Self {
        Self(error.into())
    }
}

impl Error {
    pub fn new(err: impl StdError + Send + Sync + 'static) -> Self {
        Self(err.into())
    }

    pub fn msg<M>(message: M) -> Self
    where
        M: fmt::Display + fmt::Debug + Send + Sync + 'static,
    {
        Self(anyhow::Error::msg(message))
    }

    pub fn into_boxed_err(self) -> Box<dyn StdError + Send + Sync + 'static> {
        self.0.reallocate_into_boxed_dyn_error_without_backtrace()
    }
}
