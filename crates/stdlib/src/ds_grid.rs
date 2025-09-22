use std::sync::atomic;

use fabricator_vm as vm;
use gc_arena::{Collect, Gc, Lock, Mutation, Rootable, barrier};
use thiserror::Error;

#[derive(Debug, Copy, Clone, Error)]
#[error("index [{x}, {y}] out of range of grid size {width}x{height}")]
pub struct OutOfGridRangeError {
    x: usize,
    y: usize,
    width: usize,
    height: usize,
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct DsGrid<'gc> {
    array: Box<[Lock<vm::Value<'gc>>]>,
    width: usize,
    height: usize,
    counter: i64,
}

impl<'gc> DsGrid<'gc> {
    pub fn new(width: usize, height: usize) -> Self {
        static COUNTER: atomic::AtomicI64 = atomic::AtomicI64::new(0);
        let counter = COUNTER.fetch_add(1, atomic::Ordering::Relaxed);

        Self {
            array: vec![Lock::new(vm::Value::Undefined); width.checked_mul(height).unwrap()]
                .into_boxed_slice(),
            counter,
            width,
            height,
        }
    }

    pub fn into_userdata(self, ctx: vm::Context<'gc>) -> vm::UserData<'gc> {
        #[derive(Collect)]
        #[collect(require_static)]
        struct DsGridMethods;

        impl<'gc> vm::UserDataMethods<'gc> for DsGridMethods {
            fn get_index(
                &self,
                ud: vm::UserData<'gc>,
                ctx: vm::Context<'gc>,
                indexes: &[vm::Value<'gc>],
            ) -> Result<vm::Value<'gc>, vm::RuntimeError> {
                if indexes.len() != 2 {
                    return Err(vm::RuntimeError::msg("expected 2 indexes for ds_grid"));
                }

                let x: usize = vm::FromValue::from_value(ctx, indexes[0])?;
                let y: usize = vm::FromValue::from_value(ctx, indexes[1])?;

                Ok(DsGrid::downcast(ud).unwrap().get(x, y)?)
            }

            fn set_index(
                &self,
                ud: vm::UserData<'gc>,
                ctx: vm::Context<'gc>,
                indexes: &[vm::Value<'gc>],
                value: vm::Value<'gc>,
            ) -> Result<(), vm::RuntimeError> {
                if indexes.len() != 2 {
                    return Err(vm::RuntimeError::msg("expected 2 indexes for ds_grid"));
                }

                let x: usize = vm::FromValue::from_value(ctx, indexes[0])?;
                let y: usize = vm::FromValue::from_value(ctx, indexes[1])?;

                DsGrid::set(DsGrid::downcast_write(&ctx, ud).unwrap(), x, y, value)?;
                Ok(())
            }

            fn coerce_integer(&self, ud: vm::UserData<'gc>, _ctx: vm::Context<'gc>) -> Option<i64> {
                Some(DsGrid::downcast(ud).unwrap().counter)
            }
        }

        #[derive(Collect)]
        #[collect(no_drop)]
        struct DsGridMethodsSingleton<'gc>(Gc<'gc, dyn vm::UserDataMethods<'gc>>);

        impl<'gc> vm::Singleton<'gc> for DsGridMethodsSingleton<'gc> {
            fn create(ctx: vm::Context<'gc>) -> Self {
                let methods = Gc::new(&ctx, DsGridMethods);
                DsGridMethodsSingleton(gc_arena::unsize!(methods => dyn vm::UserDataMethods<'gc>))
            }
        }

        let methods = ctx.singleton::<Rootable![DsGridMethodsSingleton<'_>]>().0;
        let ud = vm::UserData::new::<Rootable![DsGrid<'_>]>(&ctx, self);
        ud.set_methods(&ctx, Some(methods));
        ud
    }

    pub fn downcast(
        ud: vm::UserData<'gc>,
    ) -> Result<&'gc DsGrid<'gc>, vm::user_data::BadUserDataType> {
        ud.downcast::<Rootable![DsGrid<'_>]>()
    }

    pub fn downcast_write(
        mc: &Mutation<'gc>,
        ud: vm::UserData<'gc>,
    ) -> Result<&'gc barrier::Write<DsGrid<'gc>>, vm::user_data::BadUserDataType> {
        ud.downcast_write::<Rootable![DsGrid<'_>]>(mc)
    }

    pub fn get(&self, x: usize, y: usize) -> Result<vm::Value<'gc>, OutOfGridRangeError> {
        if x < self.width && y < self.height {
            Ok(self.array[y * self.width + x].get())
        } else {
            Err(OutOfGridRangeError {
                x,
                y,
                width: self.width,
                height: self.height,
            })
        }
    }

    pub fn set(
        this: &barrier::Write<Self>,
        x: usize,
        y: usize,
        value: vm::Value<'gc>,
    ) -> Result<(), OutOfGridRangeError> {
        if x < this.width && y < this.height {
            let array = barrier::field!(this, DsGrid, array).as_deref();
            array[y * this.width + x].unlock().set(value);
            Ok(())
        } else {
            Err(OutOfGridRangeError {
                x,
                y,
                width: this.width,
                height: this.height,
            })
        }
    }
}

pub fn ds_grid_lib<'gc>(ctx: vm::Context<'gc>, lib: &mut vm::MagicSet<'gc>) {
    let ds_grid_create = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (width, height): (usize, usize) = exec.stack().consume(ctx)?;
        exec.stack()
            .replace(ctx, DsGrid::new(width, height).into_userdata(ctx));
        Ok(())
    });
    lib.insert(
        ctx.intern("ds_grid_create"),
        vm::MagicConstant::new_ptr(&ctx, ds_grid_create),
    );

    let ds_grid_set_region = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (grid, xmin, ymin, xmax, ymax, value): (
            vm::UserData,
            usize,
            usize,
            usize,
            usize,
            vm::Value,
        ) = exec.stack().consume(ctx)?;
        let grid = DsGrid::downcast_write(&ctx, grid)?;

        if xmin > xmax || ymin > ymax {
            return Err(vm::RuntimeError::msg(format!(
                "grid region [{xmin}, {ymin}, {xmax}, {ymax}] is invalid"
            )));
        }

        if xmin >= grid.width || xmax >= grid.width || ymin >= grid.height || ymax >= grid.height {
            return Err(vm::RuntimeError::msg(format!(
                "grid region [{xmin}, {ymin}, {xmax}, {ymax}] is out of range of size {}x{}",
                grid.width, grid.height
            )));
        }

        for x in xmin..=xmax {
            for y in ymin..=ymax {
                DsGrid::set(grid, x, y, value)?;
            }
        }
        Ok(())
    });
    lib.insert(
        ctx.intern("ds_grid_set_region"),
        vm::MagicConstant::new_ptr(&ctx, ds_grid_set_region),
    );

    let ds_grid_clear = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (grid, value): (vm::UserData, vm::Value) = exec.stack().consume(ctx)?;
        let grid = DsGrid::downcast_write(&ctx, grid)?;
        let array = barrier::field!(grid, DsGrid, array).as_deref();
        for i in 0..array.len() {
            array[i].unlock().set(value);
        }
        Ok(())
    });
    lib.insert(
        ctx.intern("ds_grid_clear"),
        vm::MagicConstant::new_ptr(&ctx, ds_grid_clear),
    );

    let ds_grid_width = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let grid: vm::UserData = exec.stack().consume(ctx)?;
        exec.stack()
            .replace(ctx, DsGrid::downcast(grid)?.width as isize);
        Ok(())
    });
    lib.insert(
        ctx.intern("ds_grid_width"),
        vm::MagicConstant::new_ptr(&ctx, ds_grid_width),
    );

    let ds_grid_height = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let grid: vm::UserData = exec.stack().consume(ctx)?;
        exec.stack()
            .replace(ctx, DsGrid::downcast(grid)?.height as isize);
        Ok(())
    });
    lib.insert(
        ctx.intern("ds_grid_height"),
        vm::MagicConstant::new_ptr(&ctx, ds_grid_height),
    );
}
