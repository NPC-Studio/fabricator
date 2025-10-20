pub mod array;
pub mod buffer;
pub mod core;
pub mod ds_grid;
pub mod ds_list;
pub mod ds_map;
pub mod ds_priority;
pub mod json;
pub mod math;
pub mod string;
mod util;

pub use self::util::Pointer;

use fabricator_vm as vm;
use gc_arena::{Collect, Gc, Rootable};

use crate::{
    array::array_lib, buffer::buffer_lib, core::core_lib, ds_grid::ds_grid_lib,
    ds_list::ds_list_lib, ds_map::ds_map_lib, ds_priority::ds_priority_lib, json::json_lib,
    math::math_lib, string::string_lib,
};

pub trait StdlibContext<'gc> {
    fn stdlib(self) -> Gc<'gc, vm::MagicSet<'gc>>;
}

impl<'gc> StdlibContext<'gc> for vm::Context<'gc> {
    fn stdlib(self) -> Gc<'gc, vm::MagicSet<'gc>> {
        #[derive(Collect)]
        #[collect(no_drop)]
        struct StdlibSingleton<'gc>(Gc<'gc, vm::MagicSet<'gc>>);

        impl<'gc> vm::Singleton<'gc> for StdlibSingleton<'gc> {
            fn create(ctx: vm::Context<'gc>) -> Self {
                let mut stdlib = vm::MagicSet::builtins(ctx);

                core_lib(ctx, &mut stdlib);
                string_lib(ctx, &mut stdlib);
                math_lib(ctx, &mut stdlib);
                array_lib(ctx, &mut stdlib);
                buffer_lib(ctx, &mut stdlib);
                json_lib(ctx, &mut stdlib);
                ds_list_lib(ctx, &mut stdlib);
                ds_grid_lib(ctx, &mut stdlib);
                ds_map_lib(ctx, &mut stdlib);
                ds_priority_lib(ctx, &mut stdlib);

                Self(Gc::new(&ctx, stdlib))
            }
        }

        self.singleton::<Rootable![StdlibSingleton<'_>]>().0
    }
}
