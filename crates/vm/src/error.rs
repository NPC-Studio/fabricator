use std::{error::Error as StdError, fmt, ops, sync::Arc};

use gc_arena::{Collect, Gc};
use thiserror::Error;

use crate::{interpreter::Context, string::SharedStr, userdata::UserData, value::Value};

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
    pub fn new(value: Value<'gc>) -> Self {
        Self(value)
    }

    pub fn to_value(self) -> Value<'gc> {
        self.0
    }

    pub fn to_extern(self) -> ExternScriptError {
        self.into()
    }
}

/// A raw pointer to a Gc object that can be exported outside of the Gc context.
///
/// This is safe to use and construct, but dereferencing the held pointer in any way is wildly
/// unsafe.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct RawGc(pub *const ());

impl fmt::Display for RawGc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:p}", self.0)
    }
}

impl RawGc {
    pub fn new<'gc, T>(gc: Gc<'gc, T>) -> Self {
        Self(Gc::as_ptr(gc) as *const ())
    }
}

// SAFETY: The pointer held in `RawGc` needs to be held in error types which must be `Send` and is
// mostly just informative. If it is dereferenced, it is entirely up to the user to make this sound.
unsafe impl Send for RawGc {}
unsafe impl Sync for RawGc {}

/// An external representation of a [`Value`], useful for errors.
///
/// All primitive values (undefined, booleans, integers, floats) are represented here exactly.
/// Strings are cheaply cloned from an internal shared string. All other Gc types are stored as
/// `RawGc`.
#[derive(Clone)]
pub enum ExternValue {
    Undefined,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(SharedStr),
    Object(RawGc),
    Array(RawGc),
    Closure(RawGc),
    Callback(RawGc),
    UserData(RawGc),
}

impl fmt::Debug for ExternValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExternValue::Undefined => write!(f, "`undefined`"),
            ExternValue::Boolean(b) => write!(f, "`{b}`"),
            ExternValue::Integer(i) => write!(f, "`{i}`"),
            ExternValue::Float(n) => write!(f, "`{n}`"),
            ExternValue::String(s) => write!(f, "{s:?}"),
            ExternValue::Object(object) => write!(f, "<object {object}>"),
            ExternValue::Array(array) => write!(f, "<array {array}>"),
            ExternValue::Closure(closure) => {
                write!(f, "<closure {closure}>")
            }
            ExternValue::Callback(callback) => {
                write!(f, "<callback {callback}>")
            }
            ExternValue::UserData(user_data) => {
                write!(f, "<user_data {user_data}>")
            }
        }
    }
}

impl fmt::Display for ExternValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExternValue::Undefined => write!(f, "undefined"),
            ExternValue::Boolean(b) => write!(f, "{b}"),
            ExternValue::Integer(i) => write!(f, "{i}"),
            ExternValue::Float(n) => write!(f, "{n}"),
            ExternValue::String(s) => write!(f, "{s}"),
            ExternValue::Object(object) => {
                write!(f, "<object {object}>")
            }
            ExternValue::Array(array) => write!(f, "<array {array}>"),
            ExternValue::Closure(closure) => {
                write!(f, "<closure {closure}>")
            }
            ExternValue::Callback(callback) => {
                write!(f, "<callback {callback}>")
            }
            ExternValue::UserData(user_data) => {
                write!(f, "<user_data {user_data}>")
            }
        }
    }
}

impl<'gc> From<Value<'gc>> for ExternValue {
    fn from(value: Value<'gc>) -> Self {
        match value {
            Value::Undefined => ExternValue::Undefined,
            Value::Boolean(b) => ExternValue::Boolean(b),
            Value::Integer(i) => ExternValue::Integer(i),
            Value::Float(n) => ExternValue::Float(n),
            Value::String(s) => ExternValue::String(s.as_shared().clone()),
            Value::Object(o) => ExternValue::Object(RawGc::new(o.into_inner())),
            Value::Array(a) => ExternValue::Array(RawGc::new(a.into_inner())),
            Value::Closure(c) => ExternValue::Closure(RawGc::new(c.into_inner())),
            Value::Callback(c) => ExternValue::Callback(RawGc::new(c.into_inner())),
            Value::UserData(u) => ExternValue::UserData(RawGc::new(u.into_inner())),
        }
    }
}

/// A [`ScriptError`] that is not bound to the GC context and holds an [`ExternValue`].
#[derive(Debug, Clone, Error)]
#[error("{0}")]
pub struct ExternScriptError(pub ExternValue);

impl<'gc> From<ScriptError<'gc>> for ExternScriptError {
    fn from(error: ScriptError<'gc>) -> Self {
        ExternScriptError(error.to_value().into())
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
            Error::Script(err) => write!(f, "script error: {err:#}"),
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

    pub fn into_extern(self) -> ExternError {
        match self {
            Error::Script(script_error) => ExternError::Script(script_error.to_extern()),
            Error::Runtime(runtime_error) => ExternError::Runtime(runtime_error),
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
            ExternError::Runtime(err) => write!(f, "runtime error: {err}"),
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
