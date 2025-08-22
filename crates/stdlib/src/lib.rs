pub mod array;
pub mod core;
pub mod math;
pub mod testing;

use fabricator_vm::{self as vm};
use gc_arena::{Collect, Gc, Rootable};

use crate::{array::array_lib, core::core_lib, math::math_lib, testing::testing_lib};

pub trait StdlibContext<'gc> {
    fn stdlib(self) -> Gc<'gc, vm::MagicSet<'gc>>;
    fn testing_stdlib(self) -> Gc<'gc, vm::MagicSet<'gc>>;
}

impl<'gc> StdlibContext<'gc> for vm::Context<'gc> {
    fn stdlib(self) -> Gc<'gc, vm::MagicSet<'gc>> {
        #[derive(Collect)]
        #[collect(no_drop)]
        struct StdlibSingleton<'gc>(Gc<'gc, vm::MagicSet<'gc>>);

        impl<'gc> vm::Singleton<'gc> for StdlibSingleton<'gc> {
            fn create(ctx: vm::Context<'gc>) -> Self {
                let mut stdlib = vm::BuiltIns::new(&ctx).magic_set(ctx);

                core_lib(ctx, &mut stdlib);
                math_lib(ctx, &mut stdlib);
                array_lib(ctx, &mut stdlib);

                Self(Gc::new(&ctx, stdlib))
            }
        }

        self.registry()
            .singleton::<Rootable![StdlibSingleton<'_>]>(self)
            .0
    }

    fn testing_stdlib(self) -> Gc<'gc, vm::MagicSet<'gc>> {
        #[derive(Collect)]
        #[collect(no_drop)]
        struct TestingStdlibSingleton<'gc>(Gc<'gc, vm::MagicSet<'gc>>);

        impl<'gc> vm::Singleton<'gc> for TestingStdlibSingleton<'gc> {
            fn create(ctx: vm::Context<'gc>) -> Self {
                let mut lib = vm::MagicSet::new();

                lib.merge(&ctx.stdlib());
                testing_lib(ctx, &mut lib);

                Self(Gc::new(&ctx, lib))
            }
        }

        self.registry()
            .singleton::<Rootable![TestingStdlibSingleton<'_>]>(self)
            .0
    }
}
