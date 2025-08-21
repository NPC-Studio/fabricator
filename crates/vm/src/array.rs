use gc_arena::{Collect, Gc, Mutation, RefLock};

use crate::value::Value;

pub type ArrayInner<'gc> = RefLock<Vec<Value<'gc>>>;

#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Array<'gc>(Gc<'gc, ArrayInner<'gc>>);

impl<'gc> PartialEq for Array<'gc> {
    fn eq(&self, other: &Self) -> bool {
        Gc::ptr_eq(self.0, other.0)
    }
}

impl<'gc> Eq for Array<'gc> {}

impl<'gc> Array<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Self {
        Self::with_capacity(mc, 0)
    }

    pub fn with_capacity(mc: &Mutation<'gc>, capacity: usize) -> Self {
        Self(Gc::new(mc, RefLock::new(Vec::with_capacity(capacity))))
    }

    pub fn from_iter(mc: &Mutation<'gc>, iter: impl IntoIterator<Item = Value<'gc>>) -> Self {
        Self(Gc::new(mc, RefLock::new(Vec::from_iter(iter))))
    }

    #[inline]
    pub fn from_inner(inner: Gc<'gc, ArrayInner<'gc>>) -> Self {
        Self(inner)
    }

    #[inline]
    pub fn into_inner(self) -> Gc<'gc, ArrayInner<'gc>> {
        self.0
    }

    pub fn len(self) -> usize {
        self.0.borrow().len()
    }

    pub fn resize(self, mc: &Mutation<'gc>, new_len: usize, value: Value<'gc>) {
        self.0.borrow_mut(mc).resize(new_len, value);
    }

    pub fn is_empty(self) -> bool {
        self.0.borrow().is_empty()
    }

    pub fn get(self, index: usize) -> Value<'gc> {
        self.0.borrow().get(index).copied().unwrap_or_default()
    }

    pub fn set(self, mc: &Mutation<'gc>, index: usize, value: Value<'gc>) {
        let mut this = self.0.borrow_mut(mc);
        if index >= this.len() {
            this.resize(index + 1, Value::Undefined);
        }
        this[index] = value;
    }
}
