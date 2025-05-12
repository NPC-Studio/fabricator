use std::{cell::RefCell, marker::PhantomData, mem};

pub trait Freeze<'f> {
    type Frozen: 'f;
}

pub struct DynFreeze<T: ?Sized>(PhantomData<T>);

impl<'f, T: ?Sized + for<'a> Freeze<'a>> Freeze<'f> for DynFreeze<T> {
    type Frozen = <T as Freeze<'f>>::Frozen;
}

#[macro_export]
#[doc(hidden)]
macro_rules! __scripting_Freeze {
    ($f:lifetime => $frozen:ty) => {
        $crate::freeze::DynFreeze::<
            dyn for<$f> $crate::freeze::Freeze<$f, Frozen = $frozen>,
        >
    };
    ($frozen:ty) => {
        $crate::freeze::Freeze!['freeze => $frozen]
    };
}

use thiserror::Error;

pub use crate::__scripting_Freeze as Freeze;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Error)]
pub enum AccessError {
    #[error("frozen value accessed outside of enclosing scope")]
    Expired,
    #[error("already borrowed incompatibly")]
    BadBorrow,
}

/// Safely erase a lifetime from a value and store it in a scoped handle.
///
/// Works by providing only limited access to the held value within an enclosing call to
/// `Frozen:scope`.
pub struct Frozen<F: for<'f> Freeze<'f>> {
    cell: RefCell<Option<<F as Freeze<'static>>::Frozen>>,
}

impl<F: for<'f> Freeze<'f>> Default for Frozen<F> {
    fn default() -> Self {
        Self::new()
    }
}

impl<F: for<'f> Freeze<'f>> Frozen<F> {
    pub const fn new() -> Frozen<F> {
        Frozen {
            cell: RefCell::new(None),
        }
    }

    /// Set a value for the duration of the provided closure.
    ///
    /// It is explicitly allowed to call this method recursively from the provided callback, and the
    /// previous set value will be restored once the inner callback finishes.
    ///
    /// It is however NOT permitted to call this method from within [`Frozen::with`] or
    /// [`Frozen::with_mut`].
    ///
    /// # Panics
    ///
    /// Calling this method from a call to [`Frozen::with`] or [`Frozen::with_mut`] will panic.
    pub fn set<'f, R>(&self, v: <F as Freeze<'f>>::Frozen, f: impl FnOnce() -> R) -> R {
        // SAFETY: Safety depends on a few things...
        //
        // 1) We turn non-'static values into a 'static ones, outside code should never be able to
        //    observe the held 'static value, because it lies about the true lifetime.
        //
        // 2) The only way to interact with the held 'static value is through `Frozen::[try_]with`
        //    and `Frozen::[try_]with_mut`, both of which require a callback that works with the
        //    frozen type for *any* lifetime. This interaction is safe because the callbacks must
        //    work for any lifetime, so they must work with the lifetime we have erased.
        //
        // 3) The 'static `Frozen<F>` handles must have their values unset before the body of
        //    this function ends because we only know they live for at least the body of this
        //    function, and we use drop guards for this.
        let next = unsafe {
            mem::transmute::<<F as Freeze<'f>>::Frozen, <F as Freeze<'static>>::Frozen>(v)
        };

        let prev = self.cell.replace(Some(next));

        struct Guard<'a, F: for<'f> Freeze<'f>> {
            cell: &'a RefCell<Option<<F as Freeze<'static>>::Frozen>>,
            prev: Option<<F as Freeze<'static>>::Frozen>,
        }

        impl<F: for<'f> Freeze<'f>> Drop for Guard<'_, F> {
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
    pub fn with<R>(
        &self,
        f: impl for<'f> FnOnce(&<F as Freeze<'f>>::Frozen) -> R,
    ) -> Result<R, AccessError> {
        let val = self.cell.try_borrow().map_err(|_| AccessError::BadBorrow)?;
        let val = val.as_ref().ok_or(AccessError::Expired)?;
        Ok(f(val))
    }

    /// Access the stored value mutably.
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_freeze_works() {
        struct F<'a>(&'a i32);

        let f = Frozen::<Freeze![F<'freeze>]>::new();

        let i = 4;
        f.set(F(&i), || {
            f.with(|f| {
                assert_eq!(*f.0, 4);
            })
            .unwrap();
        });
    }

    #[test]
    fn test_freeze_expires() {
        struct F<'a>(&'a i32);

        let f = Frozen::<Freeze![F<'freeze>]>::new();
        assert_eq!(
            f.with(|f| {
                assert_eq!(*f.0, 4);
            }),
            Err(AccessError::Expired)
        );
    }
}
