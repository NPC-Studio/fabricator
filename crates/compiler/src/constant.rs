use std::hash::{Hash, Hasher};

use gc_arena::Collect;

#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub enum Constant<S> {
    Undefined,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(S),
}

impl<S: PartialEq> PartialEq for Constant<S> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Constant::Undefined, Constant::Undefined) => true,
            (Constant::Boolean(a), Constant::Boolean(b)) => a == b,
            (Constant::Integer(a), Constant::Integer(b)) => a == b,
            (Constant::Float(a), Constant::Float(b)) => a.to_bits() == b.to_bits(),
            (Constant::String(a), Constant::String(b)) => a == b,
            _ => false,
        }
    }
}

impl<S: Eq> Eq for Constant<S> {}

impl<S: Hash> Hash for Constant<S> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Constant::Undefined => {
                0u8.hash(state);
            }
            Constant::Boolean(b) => {
                1u8.hash(state);
                b.hash(state);
            }
            Constant::Integer(i) => {
                2u8.hash(state);
                i.hash(state);
            }
            Constant::Float(f) => {
                3u8.hash(state);
                f.to_bits().hash(state);
            }
            Constant::String(s) => {
                4u8.hash(state);
                s.hash(state);
            }
        }
    }
}

impl<S> From<bool> for Constant<S> {
    fn from(b: bool) -> Self {
        Constant::Boolean(b)
    }
}

impl<S> From<i64> for Constant<S> {
    fn from(i: i64) -> Self {
        Constant::Integer(i)
    }
}

impl<S> From<f64> for Constant<S> {
    fn from(f: f64) -> Self {
        Constant::Float(f)
    }
}

impl<S> Constant<S> {
    #[inline]
    pub fn is_undefined(&self) -> bool {
        matches!(&self, Constant::Undefined)
    }

    #[inline]
    pub fn to_bool(&self) -> bool {
        match *self {
            Constant::Undefined => false,
            Constant::Boolean(b) => b,
            Constant::Integer(i) => i > 0,
            Constant::Float(f) => f > 0.5,
            _ => true,
        }
    }

    #[inline]
    pub fn to_integer(&self) -> Option<i64> {
        match *self {
            Constant::Boolean(b) => Some(if b { 1 } else { 0 }),
            Constant::Integer(i) => Some(i),
            _ => None,
        }
    }

    #[inline]
    pub fn to_float(&self) -> Option<f64> {
        match *self {
            Constant::Boolean(b) => Some(if b { 1.0 } else { 0.0 }),
            Constant::Integer(i) => Some(i as f64),
            Constant::Float(f) => Some(f),
            _ => None,
        }
    }

    #[inline]
    pub fn negate(&self) -> Option<Constant<S>> {
        match *self {
            Constant::Boolean(b) => Some(Constant::Integer(if b { -1 } else { 0 })),
            Constant::Integer(i) => Some(Constant::Integer(i.wrapping_neg())),
            Constant::Float(f) => Some(Constant::Float(-f)),
            _ => None,
        }
    }

    #[inline]
    pub fn add(&self, other: &Constant<S>) -> Option<Constant<S>> {
        if let (Some(a), Some(b)) = (self.to_integer(), other.to_integer()) {
            Some(Constant::Integer(a.wrapping_add(b)))
        } else if let (Some(a), Some(b)) = (self.to_float(), other.to_float()) {
            Some(Constant::Float(a + b))
        } else {
            None
        }
    }

    #[inline]
    pub fn sub(&self, other: &Constant<S>) -> Option<Constant<S>> {
        if let (Some(a), Some(b)) = (self.to_integer(), other.to_integer()) {
            Some(Constant::Integer(a.wrapping_sub(b)))
        } else if let (Some(a), Some(b)) = (self.to_float(), other.to_float()) {
            Some(Constant::Float(a - b))
        } else {
            None
        }
    }

    #[inline]
    pub fn mult(&self, other: &Constant<S>) -> Option<Constant<S>> {
        if let (Some(a), Some(b)) = (self.to_integer(), other.to_integer()) {
            Some(Constant::Integer(a.wrapping_mul(b)))
        } else if let (Some(a), Some(b)) = (self.to_float(), other.to_float()) {
            Some(Constant::Float(a * b))
        } else {
            None
        }
    }

    #[inline]
    pub fn div(&self, other: &Constant<S>) -> Option<Constant<S>> {
        if let (Some(a), Some(b)) = (self.to_float(), other.to_float()) {
            Some(Constant::Float(a / b))
        } else {
            None
        }
    }

    #[inline]
    pub fn rem(&self, other: &Constant<S>) -> Option<Constant<S>> {
        if let (Some(a), Some(b)) = (self.to_integer(), other.to_integer()) {
            Some(Constant::Integer(a.wrapping_rem(b)))
        } else if let (Some(a), Some(b)) = (self.to_float(), other.to_float()) {
            Some(Constant::Float(a % b))
        } else {
            None
        }
    }

    #[inline]
    pub fn idiv(&self, other: &Constant<S>) -> Option<i64> {
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
    pub fn equal(&self, other: &Constant<S>) -> bool
    where
        S: Eq,
    {
        match (self, other) {
            (Constant::Undefined, Constant::Undefined) => true,
            (Constant::String(a), Constant::String(b)) => a == b,
            (a, b) => {
                if let (Some(a), Some(b)) = (a.to_integer(), b.to_integer()) {
                    a == b
                } else if let (Some(a), Some(b)) = (a.to_float(), b.to_float()) {
                    a == b
                } else {
                    false
                }
            }
        }
    }

    #[inline]
    pub fn less_than(&self, other: &Constant<S>) -> Option<bool> {
        if let (Some(a), Some(b)) = (self.to_integer(), other.to_integer()) {
            Some(a < b)
        } else if let (Some(a), Some(b)) = (self.to_float(), other.to_float()) {
            Some(a < b)
        } else {
            None
        }
    }

    #[inline]
    pub fn less_equal(&self, other: &Constant<S>) -> Option<bool> {
        if let (Some(a), Some(b)) = (self.to_integer(), other.to_integer()) {
            Some(a <= b)
        } else if let (Some(a), Some(b)) = (self.to_float(), other.to_float()) {
            Some(a <= b)
        } else {
            None
        }
    }

    #[inline]
    pub fn and(&self, other: &Constant<S>) -> bool {
        self.to_bool() && other.to_bool()
    }

    #[inline]
    pub fn or(&self, other: &Constant<S>) -> bool {
        self.to_bool() || other.to_bool()
    }

    #[inline]
    pub fn xor(&self, other: &Constant<S>) -> bool {
        self.to_bool() ^ other.to_bool()
    }

    #[inline]
    pub fn bit_negate(&self) -> Option<i64> {
        Some(!self.to_integer()?)
    }

    #[inline]
    pub fn bit_and(&self, other: &Constant<S>) -> Option<i64> {
        Some(self.to_integer()? & other.to_integer()?)
    }

    #[inline]
    pub fn bit_or(&self, other: &Constant<S>) -> Option<i64> {
        Some(self.to_integer()? | other.to_integer()?)
    }

    #[inline]
    pub fn bit_xor(&self, other: &Constant<S>) -> Option<i64> {
        Some(self.to_integer()? ^ other.to_integer()?)
    }

    #[inline]
    pub fn bit_shift_left(&self, other: &Constant<S>) -> Option<i64> {
        Some(self.to_integer()? << other.to_integer()?)
    }

    #[inline]
    pub fn bit_shift_right(&self, other: &Constant<S>) -> Option<i64> {
        Some(self.to_integer()? >> other.to_integer()?)
    }

    #[inline]
    pub fn null_coalesce<'a>(&'a self, other: &'a Constant<S>) -> &'a Constant<S> {
        if self.is_undefined() { other } else { self }
    }

    pub fn as_string_ref(&self) -> Constant<&S> {
        match self {
            Constant::Undefined => Constant::Undefined,
            Constant::Boolean(b) => Constant::Boolean(*b),
            Constant::Integer(i) => Constant::Integer(*i),
            Constant::Float(f) => Constant::Float(*f),
            Constant::String(s) => Constant::String(s),
        }
    }

    pub fn map_string<S2>(self, map: impl Fn(S) -> S2) -> Constant<S2> {
        match self {
            Constant::Undefined => Constant::Undefined,
            Constant::Boolean(b) => Constant::Boolean(b),
            Constant::Integer(i) => Constant::Integer(i),
            Constant::Float(f) => Constant::Float(f),
            Constant::String(s) => Constant::String(map(s)),
        }
    }
}

impl<S: AsRef<str>> Constant<S> {
    pub fn as_str(&self) -> Constant<&str> {
        match self {
            Constant::Undefined => Constant::Undefined,
            Constant::Boolean(b) => Constant::Boolean(*b),
            Constant::Integer(i) => Constant::Integer(*i),
            Constant::Float(f) => Constant::Float(*f),
            Constant::String(s) => Constant::String(s.as_ref()),
        }
    }
}
