use std::fmt;

use gc_arena::{Collect, Gc, Mutation};

use crate::{error::Error, stack::Stack};

pub trait CallbackFn<'gc>: Collect<'gc> {
    fn call(&self, mc: &Mutation<'gc>, stack: Stack<'gc, '_>) -> Result<(), Error>;
}

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Callback<'gc>(Gc<'gc, CallbackInner<'gc>>);

// We represent a callback as a single pointer with an inline VTable header.
pub struct CallbackInner<'gc> {
    call: unsafe fn(*const CallbackInner<'gc>, &Mutation<'gc>, Stack<'gc, '_>) -> Result<(), Error>,
}

impl<'gc> Callback<'gc> {
    pub fn new<C: CallbackFn<'gc> + 'gc>(mc: &Mutation<'gc>, callback: C) -> Self {
        #[repr(C)]
        struct HeaderCallback<'gc, C> {
            header: CallbackInner<'gc>,
            callback: C,
        }

        // SAFETY: We can't auto-implement `Collect` due to the function pointer lifetimes, but
        // function pointers can't hold any data.
        unsafe impl<'gc, C: Collect<'gc>> Collect<'gc> for HeaderCallback<'gc, C> {
            const NEEDS_TRACE: bool = C::NEEDS_TRACE;

            fn trace<T: gc_arena::collect::Trace<'gc>>(&self, cc: &mut T) {
                self.callback.trace(cc)
            }
        }

        let hc = Gc::new(
            mc,
            HeaderCallback {
                header: CallbackInner {
                    call: |ptr, mc, stack| unsafe {
                        let hc = ptr as *const HeaderCallback<C>;
                        ((*hc).callback).call(mc, stack)
                    },
                },
                callback,
            },
        );

        Self(unsafe { Gc::cast::<CallbackInner>(hc) })
    }

    pub fn call(self, mc: &Mutation<'gc>, stack: Stack<'gc, '_>) -> Result<(), Error> {
        unsafe { (self.0.call)(Gc::as_ptr(self.0), mc, stack) }
    }

    /// Create a callback from a Rust function.
    ///
    /// The function must be `'static` because Rust closures cannot implement `Collect`. If you need
    /// to associate GC data with this function, use [`Callback::from_fn_with`].
    pub fn from_fn<F>(mc: &Mutation<'gc>, call: F) -> Callback<'gc>
    where
        F: 'static + Fn(&Mutation<'gc>, Stack<'gc, '_>) -> Result<(), Error>,
    {
        Self::from_fn_with(mc, (), move |_, mc, stack| call(mc, stack))
    }

    /// Create a callback from a Rust function together with a GC object.
    pub fn from_fn_with<R, F>(mc: &Mutation<'gc>, root: R, call: F) -> Callback<'gc>
    where
        R: 'gc + Collect<'gc>,
        F: 'static + Fn(&R, &Mutation<'gc>, Stack<'gc, '_>) -> Result<(), Error>,
    {
        #[derive(Collect)]
        #[collect(no_drop)]
        struct RootCallback<R, F> {
            root: R,
            #[collect(require_static)]
            call: F,
        }

        impl<'gc, R, F> CallbackFn<'gc> for RootCallback<R, F>
        where
            R: 'gc + Collect<'gc>,
            F: 'static + Fn(&R, &Mutation<'gc>, Stack<'gc, '_>) -> Result<(), Error>,
        {
            fn call(&self, mc: &Mutation<'gc>, stack: Stack<'gc, '_>) -> Result<(), Error> {
                (self.call)(&self.root, mc, stack)
            }
        }

        Callback::new(mc, RootCallback { root, call })
    }

    pub fn from_inner(inner: Gc<'gc, CallbackInner<'gc>>) -> Self {
        Self(inner)
    }

    pub fn into_inner(self) -> Gc<'gc, CallbackInner<'gc>> {
        self.0
    }
}

impl<'gc> fmt::Debug for Callback<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_tuple("Callback")
            .field(&Gc::as_ptr(self.0))
            .finish()
    }
}

impl<'gc> PartialEq for Callback<'gc> {
    fn eq(&self, other: &Callback<'gc>) -> bool {
        Gc::ptr_eq(self.0, other.0)
    }
}

impl<'gc> Eq for Callback<'gc> {}
