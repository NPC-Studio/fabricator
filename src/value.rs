use gc_arena::{Collect, Mutation};

use crate::{callback::Callback, closure::Closure, object::Object, string::String};

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub enum Function<'gc> {
    Closure(Closure<'gc>),
    Callback(Callback<'gc>),
}

impl<'gc> Function<'gc> {
    pub fn rebind(self, mc: &Mutation<'gc>, this: Option<Object<'gc>>) -> Self {
        match self {
            Function::Closure(closure) => Function::Closure(closure.rebind(mc, this)),
            Function::Callback(callback) => Function::Callback(callback.rebind(mc, this)),
        }
    }
}

impl<'gc> Into<Value<'gc>> for Function<'gc> {
    fn into(self) -> Value<'gc> {
        match self {
            Function::Closure(closure) => Value::Closure(closure),
            Function::Callback(callback) => Value::Callback(callback),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Collect)]
#[collect(no_drop)]
pub enum Value<'gc> {
    Undefined,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String<'gc>),
    Object(Object<'gc>),
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
