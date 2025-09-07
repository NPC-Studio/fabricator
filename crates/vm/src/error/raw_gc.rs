use std::fmt;

use gc_arena::Gc;

/// A raw pointer to a Gc object that can be exported outside of the Gc context.
///
/// This is safe to use and construct, but dereferencing the held pointer in any way is wildly
/// unsafe.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct RawGc(pub *const ());

impl fmt::Display for RawGc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:p}", self.0)
    }
}

impl RawGc {
    pub fn new<'gc, T>(gc: Gc<'gc, T>) -> Self {
        Self(Gc::as_ptr(gc) as *const ())
    }
}

// SAFETY: The pointer held in `RawGc` needs to be held in error types which must be `Send` and is
// mostly just informative. If it is dereferenced, it is entirely up to the user to make this sound.
unsafe impl Send for RawGc {}
unsafe impl Sync for RawGc {}
