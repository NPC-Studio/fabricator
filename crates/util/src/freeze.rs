use std::{cell::RefCell, marker::PhantomData, mem};

use thiserror::Error;

pub trait Freeze<'f> {
    type Frozen: 'f;
}

pub struct DynFreeze<T: ?Sized>(PhantomData<T>);

impl<'f, T: ?Sized + for<'a> Freeze<'a>> Freeze<'f> for DynFreeze<T> {
    type Frozen = <T as Freeze<'f>>::Frozen;
}

#[macro_export]
#[doc(hidden)]
macro_rules! __freeze_Freeze {
    ($f:lifetime => $frozen:ty) => {
        $crate::freeze::DynFreeze::<
            dyn for<$f> $crate::freeze::Freeze<$f, Frozen = $frozen>,
        >
    };
    ($frozen:ty) => {
        $crate::freeze::Freeze!['freeze => $frozen]
    };
}

#[doc(inline)]
pub use crate::__freeze_Freeze as Freeze;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Error)]
pub enum AccessError {
    #[error("frozen value accessed outside of enclosing freeze scope")]
    Expired,
    #[error("already borrowed incompatibly")]
    BadBorrow,
}

/// Safely erase a lifetime from a value and store it in a scoped handle.
///
/// Works by providing only limited access to the held value within an enclosing call to
/// [`FreezeCell::with`] or [`FreezeCell::with_mut`].
pub struct FreezeCell<F: for<'f> Freeze<'f>> {
    cell: RefCell<Option<<F as Freeze<'static>>::Frozen>>,
}

impl<F: for<'f> Freeze<'f>> Default for FreezeCell<F> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<F: for<'f> Freeze<'f>> FreezeCell<F> {
    #[inline]
    pub const fn new() -> FreezeCell<F> {
        FreezeCell {
            cell: RefCell::new(None),
        }
    }

    /// Set a value for the duration of the provided closure.
    ///
    /// It is explicitly allowed to call this method recursively from the provided callback, and the
    /// previous set value will be restored once the inner callback finishes.
    ///
    /// It is however NOT permitted to call this method from within [`FreezeCell::with`] or
    /// [`FreezeCell::with_mut`].
    ///
    /// # Panics
    ///
    /// Calling this method from a call to [`FreezeCell::with`] or [`FreezeCell::with_mut`] will
    /// panic.
    #[inline]
    pub fn freeze<'f, R>(&self, v: <F as Freeze<'f>>::Frozen, f: impl FnOnce() -> R) -> R {
        // SAFETY: Safety depends on a few things...
        //
        // 1) We turn non-'static values into a 'static ones, outside code should never be able to
        //    observe the held 'static value, because it lies about the true lifetime.
        //
        // 2) The only way to interact with the held 'static value is through `FreezeCell::with` and
        //    `FreezeCell::with_mut`, both of which require a callback that works with the frozen
        //    type for *any* lifetime. This interaction is safe because the callbacks must work for
        //    any lifetime, so they must work with the lifetime we have erased.
        //
        // 3) The 'static `FreezeCell<F>` handles must have their values unset before the body
        //    of this function ends because we only know they live for at least the body of this
        //    function, and we use a drop guard for this.
        let next = unsafe {
            mem::transmute::<<F as Freeze<'f>>::Frozen, <F as Freeze<'static>>::Frozen>(v)
        };

        let prev = self
            .cell
            .try_borrow_mut()
            .expect("`FreezeCell::freeze` cannot be called inside `FreezeCell::with[_mut]`")
            .replace(next);

        struct Guard<'a, F: for<'f> Freeze<'f>> {
            cell: &'a RefCell<Option<<F as Freeze<'static>>::Frozen>>,
            prev: Option<<F as Freeze<'static>>::Frozen>,
        }

        impl<F: for<'f> Freeze<'f>> Drop for Guard<'_, F> {
            #[inline]
            fn drop(&mut self) {
                if let Ok(mut cell) = self.cell.try_borrow_mut() {
                    *cell = self.prev.take();
                } else {
                    // If the value is locked, then there is a live reference to it somewhere in the
                    // body of a `Fozen::with[_mut]` call. We can no longer guarantee our invariants
                    // and are forced to abort.
                    //
                    // This should be impossible to trigger safely.
                    eprintln!("freeze lock held during guard drop, aborting!");
                    std::process::abort()
                }
            }
        }

        let _guard = Guard::<F> {
            cell: &self.cell,
            prev,
        };

        f()
    }

    /// Access the stored value.
    #[inline]
    pub fn with<R>(
        &self,
        f: impl for<'f> FnOnce(&<F as Freeze<'f>>::Frozen) -> R,
    ) -> Result<R, AccessError> {
        let val = self.cell.try_borrow().map_err(|_| AccessError::BadBorrow)?;
        let val = val.as_ref().ok_or(AccessError::Expired)?;
        Ok(f(val))
    }

    /// Access the stored value mutably.
    #[inline]
    pub fn with_mut<R>(
        &self,
        f: impl for<'f> FnOnce(&mut <F as Freeze<'f>>::Frozen) -> R,
    ) -> Result<R, AccessError> {
        let mut val = self
            .cell
            .try_borrow_mut()
            .map_err(|_| AccessError::BadBorrow)?;
        let val = val.as_mut().ok_or(AccessError::Expired)?;
        Ok(f(val))
    }
}

/// A builder type that makes it easier to freeze values inside several [`FreezeCell`]s at once.
///
/// This can be used to avoid the rightward drift that results from making several individual nested
/// calls to [`FreezeCell::freeze`], but is otherwise identical.
pub struct FreezeMany<T = ()>(T);

impl FreezeMany<()> {
    #[inline]
    pub fn new() -> Self {
        FreezeMany(())
    }
}

impl<T> FreezeMany<T> {
    /// Freeze the given value in the provided [`FreezeCell`] during the call to
    /// [`FreezeMany::in_scope`].
    #[inline]
    pub fn freeze<'h, 'f, F: for<'a> Freeze<'a>>(
        self,
        cell: &'h FreezeCell<F>,
        value: <F as Freeze<'f>>::Frozen,
    ) -> FreezeMany<(FreezeOne<'h, 'f, F>, T)> {
        FreezeMany((FreezeOne { cell, value }, self.0))
    }
}

impl<A: SetFrozen, B: SetFrozen> FreezeMany<(A, B)> {
    /// Freeze every value provided via [`FreezeMany::freeze`] for the duration of the provided
    /// closure.
    #[inline]
    pub fn in_scope<R>(self, f: impl FnOnce() -> R) -> R {
        self.0.set(f)
    }
}

pub trait SetFrozen {
    fn set<R>(self, f: impl FnOnce() -> R) -> R;
}

pub struct FreezeOne<'h, 'f, F: for<'a> Freeze<'a>> {
    cell: &'h FreezeCell<F>,
    value: <F as Freeze<'f>>::Frozen,
}

impl<'h, 'f, F: for<'a> Freeze<'a>> SetFrozen for FreezeOne<'h, 'f, F> {
    #[inline]
    fn set<R>(self, f: impl FnOnce() -> R) -> R {
        let Self { cell, value } = self;
        cell.freeze(value, f)
    }
}

impl SetFrozen for () {
    #[inline]
    fn set<R>(self, f: impl FnOnce() -> R) -> R {
        f()
    }
}

impl<A: SetFrozen, B: SetFrozen> SetFrozen for (A, B) {
    #[inline]
    fn set<R>(self, f: impl FnOnce() -> R) -> R {
        let (a, b) = self;
        a.set(move || b.set(f))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_freeze_works() {
        struct F<'a>(&'a i32);

        let f = FreezeCell::<Freeze![F<'freeze>]>::new();

        f.freeze(F(&4), || {
            f.with(|f| {
                assert_eq!(*f.0, 4);
            })
            .unwrap();
        });
    }

    #[test]
    fn test_freeze_expires() {
        struct F<'a>(&'a i32);

        let f = FreezeCell::<Freeze![F<'freeze>]>::new();
        assert_eq!(
            f.with(|f| {
                assert_eq!(*f.0, 4);
            }),
            Err(AccessError::Expired)
        );
    }

    #[test]
    fn test_freeze_many() {
        struct FA<'a>(&'a i32);
        struct FB<'a>(&'a i32);

        let fa = FreezeCell::<Freeze![FA<'freeze>]>::new();
        let fb = FreezeCell::<Freeze![FB<'freeze>]>::new();

        FreezeMany::new()
            .freeze(&fa, FA(&1))
            .freeze(&fb, FB(&2))
            .in_scope(|| {
                fa.with(|fa| assert_eq!(*fa.0, 1)).unwrap();
                fb.with(|fb| assert_eq!(*fb.0, 2)).unwrap();
            })
    }
}
