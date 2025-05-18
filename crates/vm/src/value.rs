use gc_arena::{Collect, Mutation};

use crate::{
    array::Array, callback::Callback, closure::Closure, object::Object, string::String,
    userdata::UserData,
};

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub enum Function<'gc> {
    Closure(Closure<'gc>),
    Callback(Callback<'gc>),
}

impl<'gc> Function<'gc> {
    pub fn rebind(self, mc: &Mutation<'gc>, this: Value<'gc>) -> Self {
        match self {
            Function::Closure(closure) => Function::Closure(closure.rebind(mc, this)),
            Function::Callback(callback) => Function::Callback(callback.rebind(mc, this)),
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
    Array(Array<'gc>),
    Closure(Closure<'gc>),
    Callback(Callback<'gc>),
    UserData(UserData<'gc>),
}

impl<'gc> Default for Value<'gc> {
    fn default() -> Self {
        Value::Undefined
    }
}

impl<'gc> From<bool> for Value<'gc> {
    fn from(b: bool) -> Self {
        Value::Boolean(b)
    }
}

impl<'gc> From<i64> for Value<'gc> {
    fn from(i: i64) -> Self {
        Value::Integer(i)
    }
}

impl<'gc> From<f64> for Value<'gc> {
    fn from(f: f64) -> Self {
        Value::Float(f)
    }
}

impl<'gc> From<String<'gc>> for Value<'gc> {
    fn from(s: String<'gc>) -> Self {
        Value::String(s)
    }
}

impl<'gc> From<Object<'gc>> for Value<'gc> {
    fn from(o: Object<'gc>) -> Self {
        Value::Object(o)
    }
}

impl<'gc> From<Array<'gc>> for Value<'gc> {
    fn from(a: Array<'gc>) -> Self {
        Value::Array(a)
    }
}

impl<'gc> From<Closure<'gc>> for Value<'gc> {
    fn from(c: Closure<'gc>) -> Self {
        Value::Closure(c)
    }
}

impl<'gc> From<Callback<'gc>> for Value<'gc> {
    fn from(c: Callback<'gc>) -> Self {
        Value::Callback(c)
    }
}

impl<'gc> From<UserData<'gc>> for Value<'gc> {
    fn from(u: UserData<'gc>) -> Self {
        Value::UserData(u)
    }
}

impl<'gc> From<Function<'gc>> for Value<'gc> {
    fn from(func: Function<'gc>) -> Self {
        match func {
            Function::Closure(closure) => Value::Closure(closure),
            Function::Callback(callback) => Value::Callback(callback),
        }
    }
}

impl<'gc> Value<'gc> {
    pub fn type_name(self) -> &'static str {
        match self {
            Value::Undefined => "undefined",
            Value::Boolean(_) => "boolean",
            Value::Integer(_) => "integer",
            Value::Float(_) => "float",
            Value::String(_) => "string",
            Value::Object(_) => "object",
            Value::Array(_) => "array",
            Value::Closure(_) => "closure",
            Value::Callback(_) => "callback",
            Value::UserData(_) => "userdata",
        }
    }

    #[inline]
    pub fn is_undefined(self) -> bool {
        matches!(self, Value::Undefined)
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
    pub fn to_integer(self) -> Option<i64> {
        match self {
            Value::Boolean(b) => Some(if b { 1 } else { 0 }),
            Value::Integer(i) => Some(i),
            Value::Float(f) => Some(f.round() as i64),
            _ => None,
        }
    }

    #[inline]
    pub fn to_float(self) -> Option<f64> {
        match self {
            Value::Boolean(b) => Some(if b { 1.0 } else { 0.0 }),
            Value::Integer(i) => Some(i as f64),
            Value::Float(f) => Some(f),
            _ => None,
        }
    }

    #[inline]
    pub fn negate(self) -> Option<Value<'gc>> {
        match self {
            Value::Integer(i) => Some(Value::Integer(-i)),
            Value::Float(f) => Some(Value::Float(-f)),
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
    pub fn mult(self, other: Value<'gc>) -> Option<Value<'gc>> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a.wrapping_mul(b))),
            (Value::Integer(a), Value::Float(b)) => Some(Value::Float(a as f64 * b)),
            (Value::Float(a), Value::Integer(b)) => Some(Value::Float(a * b as f64)),
            (Value::Float(a), Value::Float(b)) => Some(Value::Float(a * b)),
            _ => None,
        }
    }

    #[inline]
    pub fn div(self, other: Value<'gc>) -> Option<Value<'gc>> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a.wrapping_div(b))),
            (Value::Integer(a), Value::Float(b)) => Some(Value::Float(a as f64 / b)),
            (Value::Float(a), Value::Integer(b)) => Some(Value::Float(a / b as f64)),
            (Value::Float(a), Value::Float(b)) => Some(Value::Float(a / b)),
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
