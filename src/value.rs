use std::string::String as StdString;

use gc_arena::{Collect, Gc};

use crate::{callback::Callback, closure::Closure, constant::Constant};

pub type String<'gc> = Gc<'gc, StdString>;

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub enum Function<'gc> {
    Closure(Closure<'gc>),
    Callback(Callback<'gc>),
}

#[derive(Debug, Copy, Clone, PartialEq, Collect)]
#[collect(no_drop)]
pub enum Value<'gc> {
    Undefined,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String<'gc>),
    Closure(Closure<'gc>),
    Callback(Callback<'gc>),
}

impl<'gc> Default for Value<'gc> {
    fn default() -> Self {
        Value::Undefined
    }
}

impl<'gc> Value<'gc> {
    #[inline]
    pub fn to_bool(self) -> bool {
        match self {
            Value::Undefined => false,
            Value::Boolean(b) => b,
            Value::Integer(i) => i > 0,
            Value::Float(f) => f > 0.5,
            _ => true,
        }
    }

    #[inline]
    pub fn from_constant(c: Constant<String<'gc>>) -> Self {
        match c {
            Constant::Undefined => Value::Undefined,
            Constant::Boolean(b) => Value::Boolean(b),
            Constant::Integer(i) => Value::Integer(i),
            Constant::Float(f) => Value::Float(f),
            Constant::String(s) => Value::String(s),
        }
    }

    #[inline]
    pub fn to_constant(self) -> Option<Constant<String<'gc>>> {
        match self {
            Value::Undefined => Some(Constant::Undefined),
            Value::Boolean(b) => Some(Constant::Boolean(b)),
            Value::Integer(i) => Some(Constant::Integer(i)),
            Value::Float(f) => Some(Constant::Float(f)),
            Value::String(s) => Some(Constant::String(s)),
            _ => None,
        }
    }

    #[inline]
    pub fn to_function(self) -> Option<Function<'gc>> {
        match self {
            Value::Closure(closure) => Some(Function::Closure(closure)),
            Value::Callback(callback) => Some(Function::Callback(callback)),
            _ => None,
        }
    }

    #[inline]
    pub fn add(self, other: Value<'gc>) -> Option<Value<'gc>> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a.wrapping_add(b))),
            (Value::Integer(a), Value::Float(b)) => Some(Value::Float(a as f64 + b)),
            (Value::Float(a), Value::Integer(b)) => Some(Value::Float(a + b as f64)),
            (Value::Float(a), Value::Float(b)) => Some(Value::Float(a + b)),
            _ => None,
        }
    }

    #[inline]
    pub fn sub(self, other: Value<'gc>) -> Option<Value<'gc>> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a.wrapping_sub(b))),
            (Value::Integer(a), Value::Float(b)) => Some(Value::Float(a as f64 - b)),
            (Value::Float(a), Value::Integer(b)) => Some(Value::Float(a - b as f64)),
            (Value::Float(a), Value::Float(b)) => Some(Value::Float(a - b)),
            _ => None,
        }
    }

    #[inline]
    pub fn equal(self, other: Value<'gc>) -> Option<bool> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Some(a == b),
            (Value::Integer(a), Value::Float(b)) => Some((a as f64) == b),
            (Value::Float(a), Value::Integer(b)) => Some(a == b as f64),
            (Value::Float(a), Value::Float(b)) => Some(a == b),
            _ => None,
        }
    }

    #[inline]
    pub fn less_than(self, other: Value<'gc>) -> Option<bool> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Some(a < b),
            (Value::Integer(a), Value::Float(b)) => Some((a as f64) < b),
            (Value::Float(a), Value::Integer(b)) => Some(a < b as f64),
            (Value::Float(a), Value::Float(b)) => Some(a < b),
            _ => None,
        }
    }

    #[inline]
    pub fn less_equal(self, other: Value<'gc>) -> Option<bool> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Some(a <= b),
            (Value::Integer(a), Value::Float(b)) => Some((a as f64) <= b),
            (Value::Float(a), Value::Integer(b)) => Some(a <= b as f64),
            (Value::Float(a), Value::Float(b)) => Some(a <= b),
            _ => None,
        }
    }
}
