use std::hash::{Hash, Hasher};

#[derive(Debug, Copy, Clone)]
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

impl<S> Constant<S> {
    #[inline]
    pub fn add(self, other: Constant<S>) -> Option<Constant<S>> {
        match (self, other) {
            (Constant::Integer(a), Constant::Integer(b)) => {
                Some(Constant::Integer(a.wrapping_add(b)))
            }
            (Constant::Integer(a), Constant::Float(b)) => Some(Constant::Float(a as f64 + b)),
            (Constant::Float(a), Constant::Integer(b)) => Some(Constant::Float(a + b as f64)),
            (Constant::Float(a), Constant::Float(b)) => Some(Constant::Float(a + b)),
            _ => None,
        }
    }

    #[inline]
    pub fn sub(self, other: Constant<S>) -> Option<Constant<S>> {
        match (self, other) {
            (Constant::Integer(a), Constant::Integer(b)) => {
                Some(Constant::Integer(a.wrapping_sub(b)))
            }
            (Constant::Integer(a), Constant::Float(b)) => Some(Constant::Float(a as f64 - b)),
            (Constant::Float(a), Constant::Integer(b)) => Some(Constant::Float(a - b as f64)),
            (Constant::Float(a), Constant::Float(b)) => Some(Constant::Float(a - b)),
            _ => None,
        }
    }

    #[inline]
    pub fn less_than(self, other: Constant<S>) -> Option<bool> {
        match (self, other) {
            (Constant::Integer(a), Constant::Integer(b)) => Some(a < b),
            (Constant::Integer(a), Constant::Float(b)) => Some((a as f64) < b),
            (Constant::Float(a), Constant::Integer(b)) => Some(a < b as f64),
            (Constant::Float(a), Constant::Float(b)) => Some(a < b),
            _ => None,
        }
    }

    #[inline]
    pub fn less_equal(self, other: Constant<S>) -> Option<bool> {
        match (self, other) {
            (Constant::Integer(a), Constant::Integer(b)) => Some(a <= b),
            (Constant::Integer(a), Constant::Float(b)) => Some((a as f64) <= b),
            (Constant::Float(a), Constant::Integer(b)) => Some(a <= b as f64),
            (Constant::Float(a), Constant::Float(b)) => Some(a <= b),
            _ => None,
        }
    }
}
