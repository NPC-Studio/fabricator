use fabricator_vm as vm;
use gc_arena::{Collect, Gc, Mutation};

pub fn create_magic_ro<'gc>(
    mc: &Mutation<'gc>,
    read: impl Fn(vm::Context<'gc>) -> Result<vm::Value<'gc>, vm::Error> + 'static,
) -> Gc<'gc, dyn vm::Magic<'gc>> {
    #[derive(Collect)]
    #[collect(require_static)]
    struct Magic<R> {
        read: R,
    }

    impl<'gc, R> vm::Magic<'gc> for Magic<R>
    where
        R: Fn(vm::Context<'gc>) -> Result<vm::Value<'gc>, vm::Error>,
    {
        fn get(&self, ctx: vm::Context<'gc>) -> Result<vm::Value<'gc>, vm::Error> {
            (self.read)(ctx)
        }
    }

    gc_arena::unsize!(Gc::new(mc, Magic { read }) => dyn vm::Magic)
}

pub fn create_magic_rw<'gc>(
    mc: &Mutation<'gc>,
    read: impl Fn(vm::Context<'gc>) -> Result<vm::Value<'gc>, vm::Error> + 'static,
    write: impl Fn(vm::Context<'gc>, vm::Value<'gc>) -> Result<(), vm::Error> + 'static,
) -> Gc<'gc, dyn vm::Magic<'gc>> {
    #[derive(Collect)]
    #[collect(require_static)]
    struct Magic<R, W> {
        read: R,
        write: W,
    }

    impl<'gc, R, W> vm::Magic<'gc> for Magic<R, W>
    where
        R: Fn(vm::Context<'gc>) -> Result<vm::Value<'gc>, vm::Error>,
        W: Fn(vm::Context<'gc>, vm::Value<'gc>) -> Result<(), vm::Error>,
    {
        fn get(&self, ctx: vm::Context<'gc>) -> Result<vm::Value<'gc>, vm::Error> {
            (self.read)(ctx)
        }

        fn set(
            &self,
            ctx: fabricator_vm::Context<'gc>,
            value: fabricator_vm::Value<'gc>,
        ) -> Result<(), fabricator_vm::Error> {
            (self.write)(ctx, value)
        }

        fn read_only(&self) -> bool {
            false
        }
    }

    gc_arena::unsize!(Gc::new(mc, Magic { read, write }) => dyn vm::Magic)
}
