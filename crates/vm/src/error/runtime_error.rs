use std::{error::Error as StdError, fmt, ops, ptr, sync::Arc};

use gc_arena::Collect;

/// A shareable, dynamically typed wrapper around a normal Rust error.
///
/// Rust errors can be caught and re-raised through FML which allows for unrestricted sharing, so
/// this type contains its error inside an `Arc` pointer to allow for this.
#[derive(Clone, Collect)]
#[collect(require_static)]
pub struct RuntimeError(pub Arc<ThinError>);

impl fmt::Debug for RuntimeError {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self.0, f)
    }
}

impl fmt::Display for RuntimeError {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&**self.0, f)
    }
}

impl<E: StdError + Send + Sync + 'static> From<E> for RuntimeError {
    #[inline]
    fn from(err: E) -> Self {
        Self::new(err)
    }
}

impl ops::Deref for RuntimeError {
    type Target = dyn StdError + Send + Sync + 'static;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &**self.0
    }
}

impl AsRef<dyn StdError + Send + Sync + 'static> for RuntimeError {
    #[inline]
    fn as_ref(&self) -> &(dyn StdError + Send + Sync + 'static) {
        &**self.0
    }
}

impl RuntimeError {
    pub fn new<E: StdError + Send + Sync + 'static>(err: E) -> Self {
        #[repr(C)]
        struct HeaderError<E> {
            header: ThinError,
            error: E,
        }

        unsafe fn error_ref<E: StdError + Send + Sync + 'static>(
            ptr: *const ThinError,
        ) -> *const (dyn StdError + Send + Sync + 'static) {
            let ptr = ptr as *const HeaderError<E>;
            unsafe { &(*ptr).error as *const (dyn StdError + Send + Sync + 'static) }
        }

        let he = Arc::new(HeaderError {
            header: ThinError {
                error_ref: error_ref::<E>,
            },
            error: err,
        });

        // SAFETY: `HeaderError` starts with `RuntimeErrorInner` and is `#[repr(C)]`.
        RuntimeError(unsafe { Arc::from_raw(Arc::into_raw(he) as *const ThinError) })
    }

    pub fn from_boxed(boxed_err: Box<dyn StdError + Send + Sync + 'static>) -> Self {
        struct BoxErr(Box<dyn StdError + Send + Sync + 'static>);

        impl fmt::Debug for BoxErr {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.0.fmt(f)
            }
        }

        impl fmt::Display for BoxErr {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.0.fmt(f)
            }
        }

        impl StdError for BoxErr {
            fn source(&self) -> Option<&(dyn StdError + 'static)> {
                self.0.source()
            }
        }

        Self::new(BoxErr(boxed_err.into()))
    }

    pub fn msg<M: fmt::Display + fmt::Debug + Send + Sync + 'static>(message: M) -> Self {
        struct MsgErr<M>(M);

        impl<M: fmt::Debug> fmt::Debug for MsgErr<M> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.0.fmt(f)
            }
        }

        impl<M: fmt::Display> fmt::Display for MsgErr<M> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.0.fmt(f)
            }
        }

        impl<M: fmt::Display + fmt::Debug> StdError for MsgErr<M> {}

        Self::new(MsgErr(message))
    }

    #[inline]
    pub fn is<T: StdError + Send + Sync + 'static>(&self) -> bool {
        self.as_ref().is::<T>()
    }

    #[inline]
    pub fn downcast_ref<T: StdError + Send + Sync + 'static>(&self) -> Option<&T> {
        self.as_ref().downcast_ref()
    }

    /// Convert this `RuntimeError` into a cloneable type that directly implements [`StdError`].
    ///
    /// This conversion is free and only changes the wrapper type around the inner `ThinError`.
    #[inline]
    pub fn into_stderr(self) -> SharedError {
        SharedError(self.0)
    }
}

#[derive(Clone, Collect)]
#[collect(require_static)]
pub struct SharedError(pub Arc<ThinError>);

impl fmt::Debug for SharedError {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self.0, f)
    }
}

impl fmt::Display for SharedError {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&**self.0, f)
    }
}

impl StdError for SharedError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        self.0.source()
    }
}

impl SharedError {
    pub fn into_runtime_err(self) -> RuntimeError {
        RuntimeError(self.0)
    }
}

/// Performance is extremely sensitive to the size of `RuntimeError`, so we represent it as a single
/// pointer with an inline VTable header.
pub struct ThinError {
    error_ref: unsafe fn(*const ThinError) -> *const (dyn StdError + Send + Sync + 'static),
}

impl ops::Deref for ThinError {
    type Target = dyn StdError + Send + Sync + 'static;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl AsRef<dyn StdError + Send + Sync + 'static> for ThinError {
    #[inline]
    fn as_ref(&self) -> &(dyn StdError + Send + Sync + 'static) {
        unsafe { &*(self.error_ref)(ptr::from_ref(self)) }
    }
}
