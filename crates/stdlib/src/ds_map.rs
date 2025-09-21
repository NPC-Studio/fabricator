use std::{
    cell::{Ref, RefMut},
    sync::atomic,
};

use fabricator_vm as vm;
use gc_arena::{Collect, Gc, Mutation, RefLock, Rootable, barrier};
use rustc_hash::FxHashMap;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Collect)]
#[collect(no_drop)]
pub enum MapKey<'gc> {
    Undefined,
    Boolean(bool),
    Integer(i64),
    Float(u64),
    String(vm::String<'gc>),
    Object(vm::Object<'gc>),
    Array(vm::Array<'gc>),
    Closure(vm::Closure<'gc>),
    Callback(vm::Callback<'gc>),
    UserData(vm::UserData<'gc>),
}

impl<'gc> MapKey<'gc> {
    #[inline]
    pub fn new(value: vm::Value<'gc>) -> Self {
        match value {
            vm::Value::Undefined => MapKey::Undefined,
            vm::Value::Boolean(b) => MapKey::Boolean(b),
            vm::Value::Integer(i) => MapKey::Integer(i),
            vm::Value::Float(f) => {
                let i = f as i64;
                if i as f64 == f {
                    MapKey::Integer(i)
                } else {
                    MapKey::Float(f.to_bits())
                }
            }
            vm::Value::String(s) => MapKey::String(s),
            vm::Value::Object(o) => MapKey::Object(o),
            vm::Value::Array(a) => MapKey::Array(a),
            vm::Value::Closure(c) => MapKey::Closure(c),
            vm::Value::Callback(c) => MapKey::Callback(c),
            vm::Value::UserData(u) => MapKey::UserData(u),
        }
    }
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct DsMap<'gc> {
    inner: RefLock<FxHashMap<MapKey<'gc>, vm::Value<'gc>>>,
    counter: i64,
}

impl<'gc> DsMap<'gc> {
    pub fn new() -> Self {
        static COUNTER: atomic::AtomicI64 = atomic::AtomicI64::new(0);
        let counter = COUNTER.fetch_add(1, atomic::Ordering::Relaxed);

        Self {
            inner: Default::default(),
            counter,
        }
    }

    pub fn into_userdata(self, ctx: vm::Context<'gc>) -> vm::UserData<'gc> {
        #[derive(Collect)]
        #[collect(require_static)]
        struct DsMapMethods;

        impl<'gc> vm::UserDataMethods<'gc> for DsMapMethods {
            fn get_index(
                &self,
                ud: vm::UserData<'gc>,
                _ctx: vm::Context<'gc>,
                indexes: &[vm::Value<'gc>],
            ) -> Result<vm::Value<'gc>, vm::RuntimeError> {
                if indexes.len() != 1 {
                    return Err(vm::RuntimeError::msg("expected 1 index for ds_map"));
                }
                let key = MapKey::new(indexes[0]);
                Ok(DsMap::downcast(ud)
                    .unwrap()
                    .inner
                    .borrow()
                    .get(&key)
                    .copied()
                    .unwrap_or_default())
            }

            fn set_index(
                &self,
                ud: vm::UserData<'gc>,
                ctx: vm::Context<'gc>,
                indexes: &[vm::Value<'gc>],
                value: vm::Value<'gc>,
            ) -> Result<(), vm::RuntimeError> {
                if indexes.len() != 1 {
                    return Err(vm::RuntimeError::msg("expected 1 index for ds_map"));
                }
                let ds_map = DsMap::downcast_write(&ctx, ud).unwrap();
                let key = MapKey::new(indexes[0]);
                let inner = barrier::field!(ds_map, DsMap, inner);
                let mut map = inner.unlock().borrow_mut();
                map.insert(key, value);
                Ok(())
            }

            fn coerce_integer(&self, ud: vm::UserData<'gc>, _ctx: vm::Context<'gc>) -> Option<i64> {
                Some(ud.downcast_static::<DsMap>().unwrap().counter)
            }
        }

        #[derive(Collect)]
        #[collect(no_drop)]
        struct DsMapMethodsSingleton<'gc>(Gc<'gc, dyn vm::UserDataMethods<'gc>>);

        impl<'gc> vm::Singleton<'gc> for DsMapMethodsSingleton<'gc> {
            fn create(ctx: vm::Context<'gc>) -> Self {
                let methods = Gc::new(&ctx, DsMapMethods);
                DsMapMethodsSingleton(gc_arena::unsize!(methods => dyn vm::UserDataMethods<'gc>))
            }
        }

        let methods = ctx.singleton::<Rootable![DsMapMethodsSingleton<'_>]>().0;
        let ud = vm::UserData::new::<Rootable![DsMap<'_>]>(&ctx, self);
        ud.set_methods(&ctx, Some(methods));
        ud
    }

    pub fn downcast(
        ud: vm::UserData<'gc>,
    ) -> Result<&'gc DsMap<'gc>, vm::user_data::BadUserDataType> {
        ud.downcast::<Rootable![DsMap<'_>]>()
    }

    pub fn downcast_write(
        mc: &Mutation<'gc>,
        ud: vm::UserData<'gc>,
    ) -> Result<&'gc barrier::Write<DsMap<'gc>>, vm::user_data::BadUserDataType> {
        ud.downcast_write::<Rootable![DsMap<'_>]>(mc)
    }

    pub fn borrow(&self) -> Ref<'_, FxHashMap<MapKey<'gc>, vm::Value<'gc>>> {
        self.inner.borrow()
    }

    pub fn borrow_mut(
        this: &barrier::Write<Self>,
    ) -> RefMut<'_, FxHashMap<MapKey<'gc>, vm::Value<'gc>>> {
        let inner = barrier::field!(this, DsMap, inner);
        inner.unlock().borrow_mut()
    }
}

pub fn ds_map_lib<'gc>(ctx: vm::Context<'gc>, lib: &mut vm::MagicSet<'gc>) {
    let ds_map_create = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        exec.stack().replace(ctx, DsMap::new().into_userdata(ctx));
        Ok(())
    });
    lib.insert(
        ctx.intern("ds_map_create"),
        vm::MagicConstant::new_ptr(&ctx, ds_map_create),
    );
}
