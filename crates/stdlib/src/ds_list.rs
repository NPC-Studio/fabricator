use std::{
    cell::{Ref, RefMut},
    sync::atomic,
};

use fabricator_vm as vm;
use gc_arena::{Collect, Gc, Mutation, RefLock, Rootable, barrier};

#[derive(Collect)]
#[collect(no_drop)]
pub struct DsList<'gc> {
    inner: RefLock<Vec<vm::Value<'gc>>>,
    counter: i64,
}

impl<'gc> DsList<'gc> {
    pub fn new() -> Self {
        static COUNTER: atomic::AtomicI64 = atomic::AtomicI64::new(0);
        let counter = COUNTER.fetch_add(1, atomic::Ordering::Relaxed);

        Self {
            inner: RefLock::new(Vec::new()),
            counter,
        }
    }

    pub fn into_userdata(self, ctx: vm::Context<'gc>) -> vm::UserData<'gc> {
        #[derive(Collect)]
        #[collect(require_static)]
        struct DsListMethods;

        impl<'gc> vm::UserDataMethods<'gc> for DsListMethods {
            fn get_index(
                &self,
                ud: vm::UserData<'gc>,
                ctx: vm::Context<'gc>,
                indexes: &[vm::Value<'gc>],
            ) -> Result<vm::Value<'gc>, vm::RuntimeError> {
                if indexes.len() != 1 {
                    return Err(vm::RuntimeError::msg("expected 1 index for ds_list"));
                }
                let i: usize = vm::FromValue::from_value(ctx, indexes[0])?;
                Ok(DsList::downcast(ud)
                    .unwrap()
                    .inner
                    .borrow()
                    .get(i)
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
                    return Err(vm::RuntimeError::msg("expected 1 index for ds_list"));
                }
                let i: usize = vm::FromValue::from_value(ctx, indexes[0])?;
                let ds_list = DsList::downcast_write(&ctx, ud).unwrap();
                let inner = barrier::field!(ds_list, DsList, inner);
                let mut vec = inner.unlock().borrow_mut();
                if i >= vec.len() {
                    vec.resize(i + 1, vm::Value::Undefined);
                }
                vec[i] = value;
                Ok(())
            }

            fn coerce_integer(&self, ud: vm::UserData<'gc>, _ctx: vm::Context<'gc>) -> Option<i64> {
                Some(DsList::downcast(ud).unwrap().counter)
            }
        }

        #[derive(Collect)]
        #[collect(no_drop)]
        struct DsListMethodsSingleton<'gc>(Gc<'gc, dyn vm::UserDataMethods<'gc>>);

        impl<'gc> vm::Singleton<'gc> for DsListMethodsSingleton<'gc> {
            fn create(ctx: vm::Context<'gc>) -> Self {
                let methods = Gc::new(&ctx, DsListMethods);
                DsListMethodsSingleton(gc_arena::unsize!(methods => dyn vm::UserDataMethods<'gc>))
            }
        }

        let methods = ctx.singleton::<Rootable![DsListMethodsSingleton<'_>]>().0;
        let ud = vm::UserData::new::<Rootable![DsList<'_>]>(&ctx, self);
        ud.set_methods(&ctx, Some(methods));
        ud
    }

    pub fn downcast(
        ud: vm::UserData<'gc>,
    ) -> Result<&'gc DsList<'gc>, vm::user_data::BadUserDataType> {
        ud.downcast::<Rootable![DsList<'_>]>()
    }

    pub fn downcast_write(
        mc: &Mutation<'gc>,
        ud: vm::UserData<'gc>,
    ) -> Result<&'gc barrier::Write<DsList<'gc>>, vm::user_data::BadUserDataType> {
        ud.downcast_write::<Rootable![DsList<'_>]>(mc)
    }

    pub fn borrow(&self) -> Ref<'_, Vec<vm::Value<'gc>>> {
        self.inner.borrow()
    }

    pub fn borrow_mut(this: &barrier::Write<Self>) -> RefMut<'_, Vec<vm::Value<'gc>>> {
        let inner = barrier::field!(this, DsList, inner);
        inner.unlock().borrow_mut()
    }
}

pub fn ds_list_lib<'gc>(ctx: vm::Context<'gc>, lib: &mut vm::MagicSet<'gc>) {
    let ds_list_create = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        exec.stack().replace(ctx, DsList::new().into_userdata(ctx));
        Ok(())
    });
    lib.insert(
        ctx.intern("ds_list_create"),
        vm::MagicConstant::new_ptr(&ctx, ds_list_create),
    );

    let ds_list_add = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let ds_list: vm::UserData = exec.stack().from_index(ctx, 0)?;
        let ds_list = DsList::downcast_write(&ctx, ds_list)?;
        let mut vec = DsList::borrow_mut(ds_list);
        vec.extend_from_slice(&exec.stack()[1..]);
        exec.stack().clear();
        Ok(())
    });
    lib.insert(
        ctx.intern("ds_list_add"),
        vm::MagicConstant::new_ptr(&ctx, ds_list_add),
    );

    let ds_list_find_index = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (ds_list, value): (vm::UserData, vm::Value) = exec.stack().consume(ctx)?;
        let ds_list = DsList::downcast_write(&ctx, ds_list)?;
        let index = ds_list
            .inner
            .borrow()
            .iter()
            .position(|&v| v == value)
            .map(|i| i as isize)
            .unwrap_or(-1);
        exec.stack().replace(ctx, index);
        Ok(())
    });
    lib.insert(
        ctx.intern("ds_list_find_index"),
        vm::MagicConstant::new_ptr(&ctx, ds_list_find_index),
    );

    let ds_list_delete = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (ds_list, index): (vm::UserData, usize) = exec.stack().consume(ctx)?;
        let ds_list = DsList::downcast_write(&ctx, ds_list)?;
        let mut vec = DsList::borrow_mut(ds_list);
        if index >= vec.len() {
            return Err(vm::RuntimeError::msg(format!(
                "index {index} out of range of ds_list with length {}",
                vec.len()
            )));
        }
        vec.remove(index);
        Ok(())
    });
    lib.insert(
        ctx.intern("ds_list_delete"),
        vm::MagicConstant::new_ptr(&ctx, ds_list_delete),
    );

    let ds_list_size = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let ds_list: vm::UserData = exec.stack().from_index(ctx, 0)?;
        let ds_list = DsList::downcast(ds_list)?;
        exec.stack()
            .replace(ctx, ds_list.inner.borrow().len() as isize);
        Ok(())
    });
    lib.insert(
        ctx.intern("ds_list_size"),
        vm::MagicConstant::new_ptr(&ctx, ds_list_size),
    );

    let ds_list_clear = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let ds_list: vm::UserData = exec.stack().from_index(ctx, 0)?;
        let ds_list = DsList::downcast_write(&ctx, ds_list)?;
        DsList::borrow_mut(ds_list).clear();

        exec.stack().clear();
        Ok(())
    });
    lib.insert(
        ctx.intern("ds_list_clear"),
        vm::MagicConstant::new_ptr(&ctx, ds_list_clear),
    );
}
