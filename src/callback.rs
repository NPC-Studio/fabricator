use std::fmt;

use gc_arena::{barrier, Collect, Gc, Lock, Mutation};

use crate::{context::Context, error::Error, object::Object, stack::Stack};

pub trait CallbackFn<'gc>: Collect<'gc> {
    fn call(
        &self,
        ctx: Context<'gc>,
        this: Object<'gc>,
        stack: Stack<'gc, '_>,
    ) -> Result<(), Error>;
}

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Callback<'gc>(Gc<'gc, CallbackInner<'gc>>);

// We represent a callback as a single pointer with an inline VTable header.
pub struct CallbackInner<'gc> {
    call: unsafe fn(
        *const CallbackInner<'gc>,
        Context<'gc>,
        Object<'gc>,
        Stack<'gc, '_>,
    ) -> Result<(), Error>,
    this: Lock<Option<Object<'gc>>>,
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
                self.header.this.trace(cc);
                self.callback.trace(cc)
            }
        }

        let hc = Gc::new(
            mc,
            HeaderCallback {
                header: CallbackInner {
                    call: |ptr, mc, this, stack| unsafe {
                        let hc = ptr as *const HeaderCallback<C>;
                        ((*hc).callback).call(mc, this, stack)
                    },
                    this: Lock::new(None),
                },
                callback,
            },
        );

        Self(unsafe { Gc::cast::<CallbackInner>(hc) })
    }

    /// Call the contained callback.
    ///
    /// The provided `this` object will be used if no `this` object is bound to the callback.
    pub fn call_with(
        self,
        ctx: Context<'gc>,
        this: Object<'gc>,
        stack: Stack<'gc, '_>,
    ) -> Result<(), Error> {
        unsafe {
            (self.0.call)(
                Gc::as_ptr(self.0),
                ctx,
                self.0.this.get().unwrap_or(this),
                stack,
            )
        }
    }

    /// Call the contained callback.
    ///
    /// If no `this` object is bound to the callback, then the `this` object will be set as
    /// `ctx.globals()`.
    pub fn call(self, ctx: Context<'gc>, stack: Stack<'gc, '_>) -> Result<(), Error> {
        self.call_with(ctx, ctx.globals(), stack)
    }

    /// Bind an object to this callback so that this object will always be used as the `this`
    /// object.
    ///
    /// If `None` is provided, then the bound `this` object will be removed.
    ///
    /// Returns the previously bound `this` object, if one was set.
    pub fn bind(self, mc: &Mutation<'gc>, this: Option<Object<'gc>>) -> Option<Object<'gc>> {
        barrier::field!(Gc::write(mc, self.0), CallbackInner, this)
            .unlock()
            .replace(this)
    }

    /// Returns the currently bound `this` object, if one is set.
    pub fn this(self) -> Option<Object<'gc>> {
        self.0.this.get()
    }

    /// Create a callback from a Rust function.
    ///
    /// The function must be `'static` because Rust closures cannot implement `Collect`. If you need
    /// to associate GC data with this function, use [`Callback::from_fn_with`].
    pub fn from_fn<F>(mc: &Mutation<'gc>, call: F) -> Callback<'gc>
    where
        F: 'static + Fn(Context<'gc>, Object<'gc>, Stack<'gc, '_>) -> Result<(), Error>,
    {
        Self::from_fn_with(mc, (), move |_, ctx, this, stack| call(ctx, this, stack))
    }

    /// Create a callback from a Rust function together with a GC object.
    pub fn from_fn_with<R, F>(mc: &Mutation<'gc>, root: R, call: F) -> Callback<'gc>
    where
        R: 'gc + Collect<'gc>,
        F: 'static + Fn(&R, Context<'gc>, Object<'gc>, Stack<'gc, '_>) -> Result<(), Error>,
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
            F: 'static + Fn(&R, Context<'gc>, Object<'gc>, Stack<'gc, '_>) -> Result<(), Error>,
        {
            fn call(
                &self,
                ctx: Context<'gc>,
                this: Object<'gc>,
                stack: Stack<'gc, '_>,
            ) -> Result<(), Error> {
                (self.call)(&self.root, ctx, this, stack)
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
