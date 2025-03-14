use std::string::String as StdString;

use gc_arena::{Collect, Gc};

use crate::{callback::Callback, constant::Constant};

pub type String<'gc> = Gc<'gc, StdString>;

#[derive(Debug, Copy, Clone, PartialEq, Collect)]
#[collect(no_drop)]
pub enum Value<'gc> {
    Undefined,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String<'gc>),
    Callback(Callback<'gc>),
}

impl<'gc> Default for Value<'gc> {
    fn default() -> Self {
        Value::Undefined
    }
}

impl<'gc> Value<'gc> {
    pub fn to_bool(self) -> bool {
        match self {
            Value::Undefined => false,
            Value::Boolean(b) => b,
            _ => true,
        }
    }

    pub fn add(self, other: Value<'gc>) -> Option<Value<'gc>> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a.wrapping_add(b))),
            (Value::Integer(a), Value::Float(b)) => Some(Value::Float(a as f64 + b)),
            (Value::Float(a), Value::Integer(b)) => Some(Value::Float(a + b as f64)),
            (Value::Float(a), Value::Float(b)) => Some(Value::Float(a + b)),
            _ => None,
        }
    }

    pub fn sub(self, other: Value<'gc>) -> Option<Value<'gc>> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a.wrapping_sub(b))),
            (Value::Integer(a), Value::Float(b)) => Some(Value::Float(a as f64 - b)),
            (Value::Float(a), Value::Integer(b)) => Some(Value::Float(a - b as f64)),
            (Value::Float(a), Value::Float(b)) => Some(Value::Float(a - b)),
            _ => None,
        }
    }

    pub fn less_than(self, other: Value<'gc>) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a < b,
            (Value::Integer(a), Value::Float(b)) => (a as f64) < b,
            (Value::Float(a), Value::Integer(b)) => a < b as f64,
            (Value::Float(a), Value::Float(b)) => a < b,
            _ => false,
        }
    }

    pub fn less_equal(self, other: Value<'gc>) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a <= b,
            (Value::Integer(a), Value::Float(b)) => (a as f64) <= b,
            (Value::Float(a), Value::Integer(b)) => a <= b as f64,
            (Value::Float(a), Value::Float(b)) => a <= b,
            _ => false,
        }
    }
}

impl<'gc> From<Constant<String<'gc>>> for Value<'gc> {
    fn from(c: Constant<String<'gc>>) -> Self {
        match c {
            Constant::Undefined => Value::Undefined,
            Constant::Boolean(b) => Value::Boolean(b),
            Constant::Integer(i) => Value::Integer(i),
            Constant::Float(f) => Value::Float(f),
            Constant::String(s) => Value::String(s),
        }
    }
}
