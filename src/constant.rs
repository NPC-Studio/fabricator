use gc_arena::Collect;

#[derive(Debug, Copy, Clone, PartialEq, Collect)]
#[collect(no_drop)]
pub enum Constant<S> {
    Undefined,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(S),
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
