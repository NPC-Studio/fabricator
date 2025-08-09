use std::{error::Error as StdError, fmt, ops, string::String as StdString, sync::Arc};

use gc_arena::{Collect, Gc};
use thiserror::Error;

use crate::{interpreter::Context, userdata::UserData, value::Value};

/// An error raised directly from FML which contains a `Value`.
///
/// Any [`Value`] can be raised as an error and it will be contained here.
#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct ScriptError<'gc>(pub Value<'gc>);

impl<'gc> fmt::Display for ScriptError<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'gc> From<Value<'gc>> for ScriptError<'gc> {
    fn from(error: Value<'gc>) -> Self {
        ScriptError(error)
    }
}

impl<'gc> ScriptError<'gc> {
    pub fn to_extern(self) -> ExternScriptError {
        self.into()
    }
}

/// A [`ScriptError`] that is not bound to the GC context.
///
/// All primitive values (undefined, booleans, integers, floats) are represented here exactly.
/// Strings are converted into normal Rust strings. All other Gc types are stored in their *raw
/// pointer* form.
#[derive(Debug, Clone, Error)]
pub enum ExternScriptError {
    #[error("nil")]
    Undefined,
    #[error("{0}")]
    Boolean(bool),
    #[error("{0}")]
    Integer(i64),
    #[error("{0}")]
    Float(f64),
    #[error("{0}")]
    String(StdString),
    #[error("<object {0:p}>")]
    Object(*const ()),
    #[error("<array {0:p}>")]
    Array(*const ()),
    #[error("<closure {0:p}>")]
    Closure(*const ()),
    #[error("<callback {0:p}>")]
    Callback(*const ()),
    #[error("<userdata {0:p}>")]
    UserData(*const ()),
}

// SAFETY: The pointers in `ExternScriptError` are not actually dereferenced at all, they are purely
// informational.
unsafe impl Send for ExternScriptError {}
unsafe impl Sync for ExternScriptError {}

impl<'gc> From<ScriptError<'gc>> for ExternScriptError {
    fn from(error: ScriptError<'gc>) -> Self {
        match error.0 {
            Value::Undefined => ExternScriptError::Undefined,
            Value::Boolean(b) => ExternScriptError::Boolean(b),
            Value::Integer(i) => ExternScriptError::Integer(i),
            Value::Float(n) => ExternScriptError::Float(n),
            Value::String(s) => ExternScriptError::String(s.to_string()),
            Value::Object(o) => ExternScriptError::Object(Gc::as_ptr(o.into_inner()) as *const ()),
            Value::Array(a) => ExternScriptError::Array(Gc::as_ptr(a.into_inner()) as *const ()),
            Value::Closure(c) => {
                ExternScriptError::Closure(Gc::as_ptr(c.into_inner()) as *const ())
            }
            Value::Callback(c) => {
                ExternScriptError::Callback(Gc::as_ptr(c.into_inner()) as *const ())
            }
            Value::UserData(u) => {
                ExternScriptError::UserData(Gc::as_ptr(u.into_inner()) as *const ())
            }
        }
    }
}

/// A shareable, dynamically typed wrapper around a normal Rust error.
///
/// Rust errors can be caught and re-raised through FML which allows for unrestricted sharing, so
/// this type contains its error inside an `Arc` pointer to allow for this.
#[derive(Debug, Clone, Collect)]
#[collect(require_static)]
pub struct RuntimeError(pub Arc<dyn StdError + Send + Sync + 'static>);

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<E: Into<Box<dyn StdError + Send + Sync + 'static>>> From<E> for RuntimeError {
    fn from(err: E) -> Self {
        Self::new(err)
    }
}

impl ops::Deref for RuntimeError {
    type Target = dyn StdError + Send + Sync + 'static;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl AsRef<dyn StdError + Send + Sync + 'static> for RuntimeError {
    fn as_ref(&self) -> &(dyn StdError + Send + Sync + 'static) {
        self.0.as_ref()
    }
}

impl RuntimeError {
    pub fn new(err: impl Into<Box<dyn StdError + Send + Sync + 'static>>) -> Self {
        Self(err.into().into())
    }
}

/// Any error that can be raised from executing a script.
///
/// This can be either a [`ScriptError`] containing a [`Value`], or a [`RuntimeError`] containing a
/// Rust error.
#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub enum Error<'gc> {
    Script(ScriptError<'gc>),
    Runtime(RuntimeError),
}

impl<'gc> fmt::Display for Error<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Script(err) => write!(f, "script error: {err}"),
            Error::Runtime(err) => write!(f, "runtime error: {err:#}"),
        }
    }
}

impl<'gc> From<Value<'gc>> for Error<'gc> {
    fn from(value: Value<'gc>) -> Self {
        Self::from_value(value)
    }
}

impl<'gc> From<ScriptError<'gc>> for Error<'gc> {
    fn from(error: ScriptError<'gc>) -> Self {
        Self::Script(error)
    }
}

impl<'gc> From<RuntimeError> for Error<'gc> {
    fn from(error: RuntimeError) -> Self {
        Self::Runtime(error)
    }
}

impl<'gc, E: Into<Box<dyn StdError + Send + Sync + 'static>>> From<E> for Error<'gc> {
    fn from(err: E) -> Self {
        Self::Runtime(RuntimeError::new(err))
    }
}

impl<'gc> Error<'gc> {
    /// Turn a [`Value`] into an `Error`.
    ///
    /// If the provided value is a [`UserData`] object which holds a [`RuntimeError`], then this
    /// conversion will clone the held `RuntimeError` and properly return an [`Error::Runtime`]
    /// variant. This is how Rust errors are properly transported through scripts: a `RuntimeError`
    /// which is turned into a `Value` with [`Error::to_value`] will always turn back into a
    /// `RuntimeError` error with [`Error::from_value`].
    ///
    /// If the given value is *any other* kind of script value, then this will return a
    /// [`ScriptError`] instead.
    pub fn from_value(value: Value<'gc>) -> Self {
        if let Value::UserData(ud) = value {
            if let Ok(err) = ud.downcast_static::<RuntimeError>() {
                return Error::Runtime(err.clone());
            }
        }

        Error::Script(value.into())
    }

    /// Convert an `Error` into a script value.
    ///
    /// For script errors, this simply returns the original [`Value`] directly.
    ///
    /// For Rust errors, this will return a [`UserData`] value which holds a [`RuntimeError`].
    pub fn to_value(&self, ctx: Context<'gc>) -> Value<'gc> {
        match self {
            Error::Script(err) => err.0,
            Error::Runtime(err) => UserData::new_static(&ctx, err.clone()).into(),
        }
    }
}

/// An [`enum@Error`] that is not bound to the GC context.
#[derive(Debug, Clone)]
pub enum ExternError {
    Script(ExternScriptError),
    Runtime(RuntimeError),
}

impl fmt::Display for ExternError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExternError::Script(err) => write!(f, "script error: {err}"),
            ExternError::Runtime(err) => write!(f, "runtime error: {err:#}"),
        }
    }
}

impl StdError for ExternError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            ExternError::Script(err) => Some(err),
            ExternError::Runtime(err) => Some(err.as_ref()),
        }
    }
}

impl From<ExternScriptError> for ExternError {
    fn from(error: ExternScriptError) -> Self {
        Self::Script(error)
    }
}

impl From<RuntimeError> for ExternError {
    fn from(error: RuntimeError) -> Self {
        Self::Runtime(error)
    }
}

impl<'gc> From<Error<'gc>> for ExternError {
    fn from(err: Error<'gc>) -> Self {
        match err {
            Error::Script(err) => err.to_extern().into(),
            Error::Runtime(e) => e.into(),
        }
    }
}
