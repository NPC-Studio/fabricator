use std::fmt;

use gc_arena::{Collect, Mutation};

use crate::{
    array::Array, callback::Callback, closure::Closure, interpreter::Context, object::Object,
    string::String, userdata::UserData,
};

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub enum Function<'gc> {
    Closure(Closure<'gc>),
    Callback(Callback<'gc>),
}

impl<'gc> Function<'gc> {
    pub fn this(self) -> Value<'gc> {
        match self {
            Function::Closure(closure) => closure.this(),
            Function::Callback(callback) => callback.this(),
        }
    }

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

/// The representation for all values in FML.
///
/// There are three levels of conversion methods provided on `Value` to convert them to concrete
/// types.
///   1) `as_xxx` conversions are perfect conversions that return exact values, there is one per
///       variant type (including `is_undefined`).
///   2) `cast_xxx` conversions are generally reasonable conversions to do implicitly. They include
///      things that match normal GML semantics like evaluating value truthiness, casting bools to
///      numbers, and casting between numeric types.
///   3) `coerce_xxx` conversions are expensive and should not usually be done implicitly except in
///      specific circumstances. They include things like coercing scalar values into strings and
///      userdata type coercions.
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
            Value::Undefined => write!(f, "`undefined`"),
            Value::Boolean(b) => write!(f, "`{b}`"),
            Value::Integer(i) => write!(f, "`{i}`"),
            Value::Float(n) => write!(f, "`{n}`"),
            Value::String(s) => write!(f, "`{:?}`", s.as_str()),
            Value::Object(object) => write!(f, "<object {:p}>", object.into_inner()),
            Value::Array(array) => write!(f, "<array {:p}>", array.into_inner()),
            Value::Closure(closure) => write!(f, "<closure {:p}>", closure.into_inner()),
            Value::Callback(callback) => write!(f, "<callback {:p}>", callback.into_inner()),
            Value::UserData(user_data) => write!(f, "<user_data {:p}>", user_data.into_inner()),
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
            Value::Object(object) => write!(f, "<object {:p}>", object.into_inner()),
            Value::Array(array) => write!(f, "<array {:p}>", array.into_inner()),
            Value::Closure(closure) => write!(f, "<closure {:p}>", closure.into_inner()),
            Value::Callback(callback) => write!(f, "<callback {:p}>", callback.into_inner()),
            Value::UserData(user_data) => write!(f, "<user_data {:p}>", user_data.into_inner()),
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
    #[must_use]
    pub fn is_undefined(self) -> bool {
        matches!(self, Value::Undefined)
    }

    #[inline]
    pub fn as_bool(self) -> Option<bool> {
        match self {
            Value::Boolean(b) => Some(b),
            _ => None,
        }
    }

    #[inline]
    #[must_use]
    pub fn as_integer(self) -> Option<i64> {
        match self {
            Value::Integer(i) => Some(i),
            _ => None,
        }
    }

    #[inline]
    #[must_use]
    pub fn as_float(self) -> Option<f64> {
        match self {
            Value::Float(f) => Some(f),
            _ => None,
        }
    }

    #[inline]
    #[must_use]
    pub fn as_string(self) -> Option<String<'gc>> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    #[inline]
    #[must_use]
    pub fn as_object(self) -> Option<Object<'gc>> {
        match self {
            Value::Object(s) => Some(s),
            _ => None,
        }
    }

    #[inline]
    #[must_use]
    pub fn as_array(self) -> Option<Array<'gc>> {
        match self {
            Value::Array(s) => Some(s),
            _ => None,
        }
    }

    #[inline]
    #[must_use]
    pub fn as_closure(self) -> Option<Closure<'gc>> {
        match self {
            Value::Closure(c) => Some(c),
            _ => None,
        }
    }

    #[inline]
    #[must_use]
    pub fn as_callback(self) -> Option<Callback<'gc>> {
        match self {
            Value::Callback(c) => Some(c),
            _ => None,
        }
    }

    #[inline]
    #[must_use]
    pub fn as_function(self) -> Option<Function<'gc>> {
        match self {
            Value::Closure(closure) => Some(Function::Closure(closure)),
            Value::Callback(callback) => Some(Function::Callback(callback)),
            _ => None,
        }
    }

    #[inline]
    #[must_use]
    pub fn as_userdata(self) -> Option<UserData<'gc>> {
        match self {
            Value::UserData(userdata) => Some(userdata),
            _ => None,
        }
    }

    /// Interpret any value as a boolean.
    ///
    /// Boolean values are returned as themselves, `Value::Undefined` returns false, integers and
    /// floats return true if they are greater than 0.5 and false otherwise, and all other value
    /// types return true.
    #[inline]
    #[must_use]
    pub fn cast_bool(self) -> bool {
        match self {
            Value::Undefined => false,
            Value::Boolean(b) => b,
            Value::Integer(i) => i > 0,
            Value::Float(f) => f > 0.5,
            _ => true,
        }
    }

    /// Interpret numeric values as integers.
    ///
    /// Integers are returned as themselves, booleans return 0 or 1, and floats are rounded to the
    /// nearest integer.
    #[inline]
    #[must_use]
    pub fn cast_integer(self) -> Option<i64> {
        match self {
            Value::Boolean(b) => Some(if b { 1 } else { 0 }),
            Value::Integer(i) => Some(i),
            Value::Float(f) => Some(f.round() as i64),
            _ => None,
        }
    }

    /// Interpret numeric values as floats.
    ///
    /// Floats are returned as themselves, booleans return 0.0 or 1.0, and integers are converted
    /// to floats.
    #[inline]
    #[must_use]
    pub fn cast_float(self) -> Option<f64> {
        match self {
            Value::Boolean(b) => Some(if b { 1.0 } else { 0.0 }),
            Value::Integer(i) => Some(i as f64),
            Value::Float(f) => Some(f),
            _ => None,
        }
    }

    /// Coerce values into strings.
    ///
    /// Strings are returned as themselves, booleans return either `"true"` or `"false"`, numeric
    /// values are printed, and userdata values are coerced if they implement string coercion.
    #[inline]
    #[must_use]
    pub fn coerce_string(self, ctx: Context<'gc>) -> Option<String<'gc>> {
        match self {
            Value::Boolean(b) => Some(ctx.intern(if b { "true" } else { "false" })),
            Value::Integer(i) => Some(ctx.intern(&i.to_string())),
            Value::Float(f) => Some(ctx.intern(&f.to_string())),
            Value::String(s) => Some(s),
            Value::UserData(u) => u.coerce_string(ctx),
            _ => None,
        }
    }

    /// Coerce values into integers.
    ///
    /// This is similar to [`Value::cast_integer`] with the addition of parsing strings as integers
    /// if possible, and coercing userdata to integers if they implement integer coersion.
    #[inline]
    #[must_use]
    pub fn coerce_integer(self, ctx: Context<'gc>) -> Option<i64> {
        if let Some(i) = self.cast_integer() {
            Some(i)
        } else {
            match self {
                Value::String(s) => s.parse().ok(),
                Value::UserData(u) => u.coerce_integer(ctx),
                _ => None,
            }
        }
    }

    /// Coerce values into floats.
    ///
    /// This is similar to [`Value::cast_float`] with the addition of parsing strings as floats if
    /// possible, and coercing userdata to floats if they implement float coersion.
    #[inline]
    #[must_use]
    pub fn coerce_float(self, ctx: Context<'gc>) -> Option<f64> {
        if let Some(i) = self.cast_float() {
            Some(i)
        } else {
            match self {
                Value::String(s) => s.parse().ok(),
                Value::UserData(u) => u.coerce_float(ctx),
                _ => None,
            }
        }
    }

    /// If both values are numeric, return the two values added together. If both values are
    /// strings, appends them.
    #[inline]
    #[must_use]
    pub fn add_or_append(self, ctx: Context<'gc>, other: Value<'gc>) -> Option<Value<'gc>> {
        if let Some(r) = self.add(other) {
            Some(r)
        } else if let (Value::String(a), Value::String(b)) = (self, other) {
            Some(ctx.intern(&format!("{a}{b}")).into())
        } else {
            None
        }
    }

    #[inline]
    #[must_use]
    pub fn negate(self) -> Option<Value<'gc>> {
        match self {
            Value::Boolean(b) => Some(Value::Integer(if b { -1 } else { 0 })),
            Value::Integer(i) => Some(Value::Integer(-i)),
            Value::Float(f) => Some(Value::Float(-f)),
            _ => None,
        }
    }

    #[inline]
    #[must_use]
    pub fn add(self, other: Value<'gc>) -> Option<Value<'gc>> {
        if let (Some(a), Some(b)) = (self.as_integer(), other.as_integer()) {
            Some(Value::Integer(a.wrapping_add(b)))
        } else if let (Some(a), Some(b)) = (self.cast_float(), other.cast_float()) {
            Some(Value::Float(a + b))
        } else {
            None
        }
    }

    #[inline]
    #[must_use]
    pub fn sub(self, other: Value<'gc>) -> Option<Value<'gc>> {
        if let (Some(a), Some(b)) = (self.as_integer(), other.as_integer()) {
            Some(Value::Integer(a.wrapping_sub(b)))
        } else if let (Some(a), Some(b)) = (self.cast_float(), other.cast_float()) {
            Some(Value::Float(a - b))
        } else {
            None
        }
    }

    #[inline]
    #[must_use]
    pub fn mult(self, other: Value<'gc>) -> Option<Value<'gc>> {
        if let (Some(a), Some(b)) = (self.as_integer(), other.as_integer()) {
            Some(Value::Integer(a.wrapping_mul(b)))
        } else if let (Some(a), Some(b)) = (self.cast_float(), other.cast_float()) {
            Some(Value::Float(a * b))
        } else {
            None
        }
    }

    #[inline]
    #[must_use]
    pub fn div(self, other: Value<'gc>) -> Option<Value<'gc>> {
        if let (Some(a), Some(b)) = (self.cast_float(), other.cast_float()) {
            Some(Value::Float(a / b))
        } else {
            None
        }
    }

    #[inline]
    #[must_use]
    pub fn rem(self, other: Value<'gc>) -> Option<Value<'gc>> {
        if let (Some(a), Some(b)) = (self.as_integer(), other.as_integer()) {
            Some(Value::Integer(a.wrapping_rem(b)))
        } else if let (Some(a), Some(b)) = (self.cast_float(), other.cast_float()) {
            Some(Value::Float(a % b))
        } else {
            None
        }
    }

    #[inline]
    #[must_use]
    pub fn idiv(self, other: Value<'gc>) -> Option<i64> {
        let self_int = if let Some(i) = self.as_integer() {
            i
        } else if let Some(f) = self.cast_float() {
            f.round() as i64
        } else {
            return None;
        };

        let other_int = if let Some(i) = other.as_integer() {
            i
        } else if let Some(f) = other.cast_float() {
            f.round() as i64
        } else {
            return None;
        };

        Some(self_int.wrapping_div(other_int))
    }

    #[inline]
    #[must_use]
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
                if let (Some(a), Some(b)) = (self.as_integer(), other.as_integer()) {
                    a == b
                } else if let (Some(a), Some(b)) = (self.cast_float(), other.cast_float()) {
                    a == b
                } else {
                    false
                }
            }
        }
    }

    #[inline]
    #[must_use]
    pub fn less_than(self, other: Value<'gc>) -> Option<bool> {
        if let (Some(a), Some(b)) = (self.as_integer(), other.as_integer()) {
            Some(a < b)
        } else if let (Some(a), Some(b)) = (self.cast_float(), other.cast_float()) {
            Some(a < b)
        } else {
            None
        }
    }

    #[inline]
    #[must_use]
    pub fn less_equal(self, other: Value<'gc>) -> Option<bool> {
        if let (Some(a), Some(b)) = (self.as_integer(), other.as_integer()) {
            Some(a <= b)
        } else if let (Some(a), Some(b)) = (self.cast_float(), other.cast_float()) {
            Some(a <= b)
        } else {
            None
        }
    }

    #[inline]
    #[must_use]
    pub fn and(&self, other: Value<'gc>) -> bool {
        self.cast_bool() && other.cast_bool()
    }

    #[inline]
    #[must_use]
    pub fn or(&self, other: Value<'gc>) -> bool {
        self.cast_bool() || other.cast_bool()
    }

    #[inline]
    #[must_use]
    pub fn xor(&self, other: Value<'gc>) -> bool {
        self.cast_bool() ^ other.cast_bool()
    }

    #[inline]
    #[must_use]
    pub fn bit_negate(&self) -> Option<i64> {
        Some(!self.cast_integer()?)
    }

    #[inline]
    #[must_use]
    pub fn bit_and(&self, other: Value<'gc>) -> Option<i64> {
        Some(self.cast_integer()? & other.cast_integer()?)
    }

    #[inline]
    #[must_use]
    pub fn bit_or(&self, other: Value<'gc>) -> Option<i64> {
        Some(self.cast_integer()? | other.cast_integer()?)
    }

    #[inline]
    #[must_use]
    pub fn bit_xor(&self, other: Value<'gc>) -> Option<i64> {
        Some(self.cast_integer()? ^ other.cast_integer()?)
    }

    #[inline]
    #[must_use]
    pub fn bit_shift_left(&self, other: Value<'gc>) -> Option<i64> {
        Some(self.cast_integer()? << other.cast_integer()?)
    }

    #[inline]
    #[must_use]
    pub fn bit_shift_right(&self, other: Value<'gc>) -> Option<i64> {
        Some(self.cast_integer()? >> other.cast_integer()?)
    }

    #[inline]
    #[must_use]
    pub fn null_coalesce(self, other: Value<'gc>) -> Value<'gc> {
        if self.is_undefined() { other } else { self }
    }
}
