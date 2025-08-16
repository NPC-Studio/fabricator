use std::fmt;

use gc_arena::{Collect, Gc, Mutation};

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

impl<'gc> From<Closure<'gc>> for Function<'gc> {
    fn from(closure: Closure<'gc>) -> Self {
        Self::Closure(closure)
    }
}

impl<'gc> From<Callback<'gc>> for Function<'gc> {
    fn from(callback: Callback<'gc>) -> Self {
        Self::Callback(callback)
    }
}

#[derive(Copy, Clone, PartialEq, Default, Collect)]
#[collect(no_drop)]
pub enum Value<'gc> {
    #[default]
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

impl<'gc> fmt::Debug for Value<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Undefined => write!(f, "Value::Undefined"),
            Value::Boolean(b) => write!(f, "Value::Boolean({b})"),
            Value::Integer(i) => write!(f, "Value::Integer({i})"),
            Value::Float(n) => write!(f, "Value::Float({n})"),
            Value::String(s) => write!(f, "Value::String({s})"),
            Value::Object(object) => {
                write!(f, "Value::Object{:p})", Gc::as_ptr(object.into_inner()))
            }
            Value::Array(array) => write!(f, "Value::Array({:p})", Gc::as_ptr(array.into_inner())),
            Value::Closure(closure) => {
                write!(f, "Value::Closure({:p})", Gc::as_ptr(closure.into_inner()))
            }
            Value::Callback(callback) => {
                write!(
                    f,
                    "Value::Callback({:p})",
                    Gc::as_ptr(callback.into_inner())
                )
            }
            Value::UserData(user_data) => {
                write!(
                    f,
                    "Value::UserData({:p})",
                    Gc::as_ptr(user_data.into_inner())
                )
            }
        }
    }
}

impl<'gc> fmt::Display for Value<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Undefined => write!(f, "undefined"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Integer(i) => write!(f, "{i}"),
            Value::Float(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "{s:?}"),
            Value::Object(object) => write!(f, "<object {:p}>", Gc::as_ptr(object.into_inner())),
            Value::Array(array) => write!(f, "<array {:p}>", Gc::as_ptr(array.into_inner())),
            Value::Closure(closure) => {
                write!(f, "<closure {:p}>", Gc::as_ptr(closure.into_inner()))
            }
            Value::Callback(callback) => {
                write!(f, "<callback {:p}>", Gc::as_ptr(callback.into_inner()))
            }
            Value::UserData(user_data) => {
                write!(f, "<user_data {:p}>", Gc::as_ptr(user_data.into_inner()))
            }
        }
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
    pub fn to_function(self) -> Option<Function<'gc>> {
        match self {
            Value::Closure(closure) => Some(Function::Closure(closure)),
            Value::Callback(callback) => Some(Function::Callback(callback)),
            _ => None,
        }
    }

    #[inline]
    pub fn negate(self) -> Option<Value<'gc>> {
        match self {
            Value::Boolean(b) => Some(Value::Integer(if b { -1 } else { 0 })),
            Value::Integer(i) => Some(Value::Integer(-i)),
            Value::Float(f) => Some(Value::Float(-f)),
            _ => None,
        }
    }

    #[inline]
    pub fn add(self, other: Value<'gc>) -> Option<Value<'gc>> {
        if let (Some(a), Some(b)) = (self.to_integer(), other.to_integer()) {
            Some(Value::Integer(a.wrapping_add(b)))
        } else if let (Some(a), Some(b)) = (self.to_float(), other.to_float()) {
            Some(Value::Float(a + b))
        } else {
            None
        }
    }

    #[inline]
    pub fn sub(self, other: Value<'gc>) -> Option<Value<'gc>> {
        if let (Some(a), Some(b)) = (self.to_integer(), other.to_integer()) {
            Some(Value::Integer(a.wrapping_sub(b)))
        } else if let (Some(a), Some(b)) = (self.to_float(), other.to_float()) {
            Some(Value::Float(a - b))
        } else {
            None
        }
    }

    #[inline]
    pub fn mult(self, other: Value<'gc>) -> Option<Value<'gc>> {
        if let (Some(a), Some(b)) = (self.to_integer(), other.to_integer()) {
            Some(Value::Integer(a.wrapping_mul(b)))
        } else if let (Some(a), Some(b)) = (self.to_float(), other.to_float()) {
            Some(Value::Float(a * b))
        } else {
            None
        }
    }

    #[inline]
    pub fn div(self, other: Value<'gc>) -> Option<Value<'gc>> {
        if let (Some(a), Some(b)) = (self.to_float(), other.to_float()) {
            Some(Value::Float(a / b))
        } else {
            None
        }
    }

    #[inline]
    pub fn rem(self, other: Value<'gc>) -> Option<Value<'gc>> {
        if let (Some(a), Some(b)) = (self.to_integer(), other.to_integer()) {
            Some(Value::Integer(a.wrapping_rem(b)))
        } else if let (Some(a), Some(b)) = (self.to_float(), other.to_float()) {
            Some(Value::Float(a % b))
        } else {
            None
        }
    }

    #[inline]
    pub fn idiv(self, other: Value<'gc>) -> Option<i64> {
        let self_int = if let Some(i) = self.to_integer() {
            i
        } else if let Some(f) = self.to_float() {
            f.round() as i64
        } else {
            return None;
        };

        let other_int = if let Some(i) = other.to_integer() {
            i
        } else if let Some(f) = other.to_float() {
            f.round() as i64
        } else {
            return None;
        };

        Some(self_int.wrapping_div(other_int))
    }

    #[inline]
    pub fn equal(self, other: Value<'gc>) -> bool {
        match (self, other) {
            (Value::Undefined, Value::Undefined) => true,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Object(a), Value::Object(b)) => a == b,
            (Value::Array(a), Value::Array(b)) => a == b,
            (Value::Closure(a), Value::Closure(b)) => a == b,
            (Value::Callback(a), Value::Callback(b)) => a == b,
            (Value::UserData(a), Value::UserData(b)) => a == b,
            _ => {
                if let (Some(a), Some(b)) = (self.to_integer(), other.to_integer()) {
                    a == b
                } else if let (Some(a), Some(b)) = (self.to_float(), other.to_float()) {
                    a == b
                } else {
                    false
                }
            }
        }
    }

    #[inline]
    pub fn less_than(self, other: Value<'gc>) -> Option<bool> {
        if let (Some(a), Some(b)) = (self.to_integer(), other.to_integer()) {
            Some(a < b)
        } else if let (Some(a), Some(b)) = (self.to_float(), other.to_float()) {
            Some(a < b)
        } else {
            None
        }
    }

    #[inline]
    pub fn less_equal(self, other: Value<'gc>) -> Option<bool> {
        if let (Some(a), Some(b)) = (self.to_integer(), other.to_integer()) {
            Some(a <= b)
        } else if let (Some(a), Some(b)) = (self.to_float(), other.to_float()) {
            Some(a <= b)
        } else {
            None
        }
    }

    #[inline]
    pub fn and(&self, other: Value<'gc>) -> bool {
        self.to_bool() && other.to_bool()
    }

    #[inline]
    pub fn or(&self, other: Value<'gc>) -> bool {
        self.to_bool() || other.to_bool()
    }

    #[inline]
    pub fn xor(&self, other: Value<'gc>) -> bool {
        self.to_bool() ^ other.to_bool()
    }

    #[inline]
    pub fn bit_negate(&self) -> Option<i64> {
        Some(!self.to_integer()?)
    }

    #[inline]
    pub fn bit_and(&self, other: Value<'gc>) -> Option<i64> {
        Some(self.to_integer()? & other.to_integer()?)
    }

    #[inline]
    pub fn bit_or(&self, other: Value<'gc>) -> Option<i64> {
        Some(self.to_integer()? | other.to_integer()?)
    }

    #[inline]
    pub fn bit_xor(&self, other: Value<'gc>) -> Option<i64> {
        Some(self.to_integer()? ^ other.to_integer()?)
    }

    #[inline]
    pub fn bit_shift_left(&self, other: Value<'gc>) -> Option<i64> {
        Some(self.to_integer()? << other.to_integer()?)
    }

    #[inline]
    pub fn bit_shift_right(&self, other: Value<'gc>) -> Option<i64> {
        Some(self.to_integer()? >> other.to_integer()?)
    }

    #[inline]
    pub fn null_coalesce(self, other: Value<'gc>) -> Value<'gc> {
        if self.is_undefined() { other } else { self }
    }
}
