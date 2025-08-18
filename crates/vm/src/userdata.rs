use std::{
    fmt,
    hash::{Hash, Hasher},
};

use gc_arena::{Collect, Gc, Mutation, Rootable, Static, arena::Root, barrier, lock};
use thiserror::Error;

use crate::{
    any::{Any, AnyInner},
    error::Error,
    interpreter::Context,
    string::String,
    value::Value,
};

#[derive(Debug, Copy, Clone, Error)]
#[error("UserData type mismatch")]
pub struct BadUserDataType;

pub trait UserDataMethods<'gc> {
    fn get_field(
        &self,
        ctx: Context<'gc>,
        ud: UserData<'gc>,
        key: String<'gc>,
    ) -> Result<Value<'gc>, Error<'gc>>;

    fn set_field(
        &self,
        ctx: Context<'gc>,
        ud: UserData<'gc>,
        key: String<'gc>,
        value: Value<'gc>,
    ) -> Result<(), Error<'gc>>;

    fn get_index(
        &self,
        ctx: Context<'gc>,
        ud: UserData<'gc>,
        indexes: &[Value<'gc>],
    ) -> Result<Value<'gc>, Error<'gc>>;

    fn set_index(
        &self,
        ctx: Context<'gc>,
        ud: UserData<'gc>,
        indexes: &[Value<'gc>],
        value: Value<'gc>,
    ) -> Result<(), Error<'gc>>;

    /// Return an iterator function and state value for iteration in a `with` loop.
    ///
    /// Should return two values according to the normal iterator protocol: the iterator function,
    /// and the initial iteration state.
    ///
    /// Every time through the loop, the iterator function will be called with the state as its
    /// parameter. The iterator function should return the new state value followed by all iter
    /// results for that iteration (for a `with` loop, there should only be one iteration result,
    /// which is a `self` value for that iteration). As soon as the state value becomes `undefined`,
    /// the iterator function will no longer be called and no additional results will be yielded. If
    /// the initial call of this method returns an `undefined` state value on the first call, then
    /// this implies an empty result and no iteration will be performed.
    fn iter(&self, ctx: Context<'gc>, ud: UserData<'gc>) -> Result<(), Error<'gc>>;
}

/// Meta-data for a `UserData` type.
#[derive(Clone, Default, Collect)]
#[collect(no_drop)]
pub struct UserDataMeta<'gc> {
    pub methods: lock::Lock<Option<Gc<'gc, dyn UserDataMethods<'gc>>>>,
}

impl<'gc> fmt::Debug for UserDataMeta<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("UserDataMeta")
            .field("methods", &self.methods.get().map(Gc::as_ptr))
            .finish()
    }
}

pub type UserDataInner<'gc> = AnyInner<UserDataMeta<'gc>>;

/// A garbage collected pointer to a user-defined type.
///
/// `UserData` fully integrate with the garbage collector and may be any (custom) garbage collected
/// type. Downcasting and mutating garbage collected types is however more complicated than normal
/// non-garbage collected types, so there are two parallel APIs provided here: one for any garbage
/// collected type and one only for `'static` types that is provided as a convenience over the more
/// general interface for arbitrary GC types.
///
/// There is no automatic mechanism to provide internal mutability on the held value. If the held
/// value needs to be internally mutable and is `'static`, consider normal mechanisms for Rust
/// internal mutability like [`std::cell::RefCell`]. If the type is a GC type and needs to be
/// internally mutable, use the mechanisms in `gc-arena` for this like [`gc_arena::lock::RefLock`]
/// instead.
///
/// Internally, `UserData` simply wraps an [`Any`] type with some optional metadata. For more
/// advanced cases where you simply need to store a user defined GC type with safe downcasting, use
/// [`Any`] directly.
#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct UserData<'gc>(Any<'gc, UserDataMeta<'gc>>);

impl<'gc> PartialEq for UserData<'gc> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<'gc> Eq for UserData<'gc> {}

impl<'gc> Hash for UserData<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<'gc> UserData<'gc> {
    /// Create a new `UserData` from any GC value.
    ///
    /// In order to provide safe downcasting, an `R` type must be provided that implements
    /// [`trait@Rootable`]. Usually this type is constructed with the [`macro@Rootable`] macro.
    ///
    /// Downcasting GC types requires that you provide the **same** `R` type to [`UserData::is`] or
    /// [`UserData::downcast`], as the type is identified not by the `TypeId` of the value itself,
    /// but the `TypeId` of the `Rootable` impl (and it must be this way, for soundness). If you
    /// always use the [`macro@Rootable`] macro rather than a custom `Rootable` impl as the `R`
    /// type, then this happens automatically.
    pub fn new<R>(mc: &Mutation<'gc>, val: Root<'gc, R>) -> Self
    where
        R: for<'a> Rootable<'a> + 'static,
        Root<'gc, R>: Sized + Collect<'gc>,
    {
        UserData(Any::new::<R>(mc, val))
    }

    /// Create a new `UserData` type from a non-GC value.
    ///
    /// This equivalent to calling [`UserData::new`] with the given value wrapped in the [`Static`]
    /// wrapper provided by `gc-arena` and the `R` type set to `Static<T>`.
    ///
    /// This is provided as a convenience as an easier API than dealing with the [`trait@Rootable`]
    /// trait. In order to downcast values created with this method, you can use the *static*
    /// variants of `UserData` methods as a further convenience, like [`UserData::downcast_static`].
    pub fn new_static<T: 'static>(mc: &Mutation<'gc>, val: T) -> Self {
        Self::new::<Static<T>>(mc, Static(val))
    }

    pub fn from_inner(inner: Gc<'gc, UserDataInner<'gc>>) -> Self {
        Self(Any::from_inner(inner))
    }

    pub fn into_inner(self) -> Gc<'gc, UserDataInner<'gc>> {
        self.0.into_inner()
    }

    /// Check if a `UserData` was created with the type `R` passed to [`UserData::new`].
    ///
    /// `UserData` is identified by the `TypeId` of the [`trait@Rootable`] impl, NOT the type
    /// itself. We must do things this way, because GC types are non-'static (so you cannot obtain
    /// their `TypeId` in the first place).
    pub fn is<R>(self) -> bool
    where
        R: for<'a> Rootable<'a> + 'static,
    {
        self.0.is::<R>()
    }

    /// Check if a `UserData` is of type `T` created with [`UserData::new_static`].
    ///
    /// This is equivalent to calling `this.is::<Static<T>>()`.
    pub fn is_static<T: 'static>(self) -> bool {
        self.is::<Static<T>>()
    }

    /// Downcast a GC `UserData` and get a reference to it.
    ///
    /// If [`UserData::is`] returns true for the provided type `R`, then this will return a
    /// reference to the held type, otherwise it will return `Err(BadUserDataType)`.
    pub fn downcast<R>(self) -> Result<&'gc Root<'gc, R>, BadUserDataType>
    where
        R: for<'b> Rootable<'b> + 'static,
        Root<'gc, R>: Sized,
    {
        self.0.downcast::<R>().ok_or(BadUserDataType)
    }

    /// Downcast the `UserData` and get a reference to it wrapped in [`barrier::Write`].
    ///
    /// If the type matches, this also triggers a write barrier on the held `Gc` pointer, allowing
    /// you to safely mutate the held GC value through mechanisms provided by `gc_arena`.
    ///
    /// This is ONLY ever useful if the held type is a non-'static GC type, and usually only useful
    /// when the type is something like [`gc_arena::lock::RefLock`], where this would allow you to
    /// call [`gc_arena::barrier::Write::unlock`] on it to get safe mutability.
    pub fn downcast_write<R>(
        self,
        mc: &Mutation<'gc>,
    ) -> Result<&'gc barrier::Write<Root<'gc, R>>, BadUserDataType>
    where
        R: for<'b> Rootable<'b> + 'static,
        Root<'gc, R>: Sized,
    {
        self.0.downcast_write::<R>(mc).ok_or(BadUserDataType)
    }

    /// Downcast a `'static` `UserData` and get a reference to it.
    ///
    /// If [`UserData::is_static`] returns true for the provided type `T`, then this will return a
    /// reference to the held type, otherwise it will return `Err(BadUserDataType)`.
    ///
    /// This is equivalent to calling `this.downcast::<Static<T>>()` (except this returns a
    /// reference to the inner [`Static::0`] field instead).
    pub fn downcast_static<T: 'static>(self) -> Result<&'gc T, BadUserDataType> {
        self.downcast::<Static<T>>().map(|r| &r.0)
    }

    pub fn methods(self) -> Option<Gc<'gc, dyn UserDataMethods<'gc>>> {
        self.0.metadata().methods.get()
    }

    pub fn set_methods(
        self,
        mc: &Mutation<'gc>,
        methods: Option<Gc<'gc, dyn UserDataMethods<'gc>>>,
    ) {
        barrier::unlock!(self.0.write_metadata(mc), UserDataMeta, methods).set(methods);
    }
}
