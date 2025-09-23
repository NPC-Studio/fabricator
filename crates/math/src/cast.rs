#[must_use]
pub fn cast<T: num::NumCast>(a: impl num::NumCast) -> T {
    num::cast::<_, T>(a).expect("numerical cast failed")
}

#[must_use]
pub fn try_cast<T: num::NumCast>(a: impl num::NumCast) -> Option<T> {
    num::cast::<_, T>(a)
}
