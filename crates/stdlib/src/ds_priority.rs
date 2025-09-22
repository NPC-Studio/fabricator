use std::{
    cell::{Ref, RefMut},
    cmp,
    collections::BinaryHeap,
    sync::atomic,
};

use fabricator_vm as vm;
use gc_arena::{Collect, Gc, Mutation, RefLock, Rootable, barrier};

#[derive(Collect)]
#[collect(no_drop)]
pub struct DsPriority<'gc> {
    inner: RefLock<BinaryHeap<Entry<'gc>>>,
    counter: i64,
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct Entry<'gc> {
    pub priority: f64,
    pub value: vm::Value<'gc>,
}

impl<'gc> PartialEq for Entry<'gc> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other).is_eq()
    }
}

impl<'gc> Eq for Entry<'gc> {}

impl<'gc> PartialOrd for Entry<'gc> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'gc> Ord for Entry<'gc> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.priority.total_cmp(&other.priority)
    }
}

impl<'gc> DsPriority<'gc> {
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
        struct DsPriorityMethods;

        impl<'gc> vm::UserDataMethods<'gc> for DsPriorityMethods {
            fn coerce_integer(&self, ud: vm::UserData<'gc>, _ctx: vm::Context<'gc>) -> Option<i64> {
                Some(DsPriority::downcast(ud).unwrap().counter)
            }
        }

        #[derive(Collect)]
        #[collect(no_drop)]
        struct DsPriorityMethodsSingleton<'gc>(Gc<'gc, dyn vm::UserDataMethods<'gc>>);

        impl<'gc> vm::Singleton<'gc> for DsPriorityMethodsSingleton<'gc> {
            fn create(ctx: vm::Context<'gc>) -> Self {
                let methods = Gc::new(&ctx, DsPriorityMethods);
                DsPriorityMethodsSingleton(
                    gc_arena::unsize!(methods => dyn vm::UserDataMethods<'gc>),
                )
            }
        }

        let methods = ctx
            .singleton::<Rootable![DsPriorityMethodsSingleton<'_>]>()
            .0;
        let ud = vm::UserData::new::<Rootable![DsPriority<'_>]>(&ctx, self);
        ud.set_methods(&ctx, Some(methods));
        ud
    }

    pub fn downcast(
        ud: vm::UserData<'gc>,
    ) -> Result<&'gc DsPriority<'gc>, vm::user_data::BadUserDataType> {
        ud.downcast::<Rootable![DsPriority<'_>]>()
    }

    pub fn downcast_write(
        mc: &Mutation<'gc>,
        ud: vm::UserData<'gc>,
    ) -> Result<&'gc barrier::Write<DsPriority<'gc>>, vm::user_data::BadUserDataType> {
        ud.downcast_write::<Rootable![DsPriority<'_>]>(mc)
    }

    pub fn borrow(&self) -> Ref<'_, BinaryHeap<Entry<'gc>>> {
        self.inner.borrow()
    }

    pub fn borrow_mut(this: &barrier::Write<Self>) -> RefMut<'_, BinaryHeap<Entry<'gc>>> {
        let inner = barrier::field!(this, DsPriority, inner);
        inner.unlock().borrow_mut()
    }
}

pub fn ds_priority_lib<'gc>(ctx: vm::Context<'gc>, lib: &mut vm::MagicSet<'gc>) {
    let ds_priority_create = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        exec.stack()
            .replace(ctx, DsPriority::new().into_userdata(ctx));
        Ok(())
    });
    lib.insert(
        ctx.intern("ds_priority_create"),
        vm::MagicConstant::new_ptr(&ctx, ds_priority_create),
    );
}
