use std::collections::HashMap;

use gc_arena::{Collect, Gc, Mutation, RefLock};

use crate::{string::String, value::Value};

#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Object<'gc>(Gc<'gc, RefLock<HashMap<String<'gc>, Value<'gc>>>>);

impl<'gc> PartialEq for Object<'gc> {
    fn eq(&self, other: &Self) -> bool {
        Gc::ptr_eq(self.0, other.0)
    }
}

impl<'gc> Eq for Object<'gc> {}

impl<'gc> Object<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Self {
        Self(Gc::new(mc, Default::default()))
    }

    pub fn get(self, key: String<'gc>) -> Option<Value<'gc>> {
        self.0.borrow().get(&key).copied()
    }

    pub fn set(
        self,
        mc: &Mutation<'gc>,
        key: String<'gc>,
        value: Value<'gc>,
    ) -> Option<Value<'gc>> {
        self.0.borrow_mut(mc).insert(key, value)
    }
}
