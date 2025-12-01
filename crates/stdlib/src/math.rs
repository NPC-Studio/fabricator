use std::{cell::RefCell, f64};

use fabricator_vm as vm;
use rand::{Rng, SeedableRng, rngs::SmallRng, seq::SliceRandom as _};

use crate::util::resolve_array_range;

pub fn math_lib<'gc>(ctx: vm::Context<'gc>, lib: &mut vm::MagicSet<'gc>) {
    lib.insert(
        ctx.intern("NaN"),
        vm::MagicConstant::new_ptr(&ctx, f64::NAN),
    );

    lib.insert(
        ctx.intern("infinity"),
        vm::MagicConstant::new_ptr(&ctx, f64::INFINITY),
    );

    lib.insert(
        ctx.intern("pi"),
        vm::MagicConstant::new_ptr(&ctx, f64::consts::PI),
    );

    let cos = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: f64 = exec.stack().consume(ctx)?;
        exec.stack().replace(ctx, arg.cos());
        Ok(())
    });
    lib.insert(ctx.intern("cos"), vm::MagicConstant::new_ptr(&ctx, cos));

    let sin = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: f64 = exec.stack().consume(ctx)?;
        exec.stack().replace(ctx, arg.sin());
        Ok(())
    });
    lib.insert(ctx.intern("sin"), vm::MagicConstant::new_ptr(&ctx, sin));

    let abs = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: f64 = exec.stack().consume(ctx)?;
        exec.stack().replace(ctx, arg.abs());
        Ok(())
    });
    lib.insert(ctx.intern("abs"), vm::MagicConstant::new_ptr(&ctx, abs));

    let sqrt = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: f64 = exec.stack().consume(ctx)?;
        exec.stack().replace(ctx, arg.sqrt());
        Ok(())
    });
    lib.insert(ctx.intern("sqrt"), vm::MagicConstant::new_ptr(&ctx, sqrt));

    let sqr = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: f64 = exec.stack().consume(ctx)?;
        exec.stack().replace(ctx, arg * arg);
        Ok(())
    });
    lib.insert(ctx.intern("sqr"), vm::MagicConstant::new_ptr(&ctx, sqr));

    let power = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (arg, exp): (f64, f64) = exec.stack().consume(ctx)?;
        exec.stack().replace(ctx, arg.powf(exp));
        Ok(())
    });
    lib.insert(ctx.intern("power"), vm::MagicConstant::new_ptr(&ctx, power));

    let round = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: f64 = exec.stack().consume(ctx)?;
        exec.stack().replace(ctx, arg.round_ties_even());
        Ok(())
    });
    lib.insert(ctx.intern("round"), vm::MagicConstant::new_ptr(&ctx, round));

    let floor = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: f64 = exec.stack().consume(ctx)?;
        exec.stack().replace(ctx, arg.floor());
        Ok(())
    });
    lib.insert(ctx.intern("floor"), vm::MagicConstant::new_ptr(&ctx, floor));

    let ceil = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: f64 = exec.stack().consume(ctx)?;
        exec.stack().replace(ctx, arg.ceil());
        Ok(())
    });
    lib.insert(ctx.intern("ceil"), vm::MagicConstant::new_ptr(&ctx, ceil));

    let sign = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: f64 = exec.stack().consume(ctx)?;
        let output = if arg > 0.0 {
            1.0
        } else if arg < 0.0 {
            -1.0
        } else {
            0.0
        };
        
        exec.stack().replace(ctx, output);
        Ok(())
    });
    lib.insert(ctx.intern("sign"), vm::MagicConstant::new_ptr(&ctx, sign));

    let min = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let mut min: f64 = exec.stack().from_index(ctx, 0)?;
        for i in 1..exec.stack().len() {
            min = min.min(exec.stack().from_index::<f64>(ctx, i)?);
        }
        exec.stack().replace(ctx, min);
        Ok(())
    });
    lib.insert(ctx.intern("min"), vm::MagicConstant::new_ptr(&ctx, min));

    let max = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let mut max: f64 = exec.stack().from_index(ctx, 0)?;
        for i in 1..exec.stack().len() {
            max = max.max(exec.stack().from_index::<f64>(ctx, i)?);
        }
        exec.stack().replace(ctx, max);
        Ok(())
    });
    lib.insert(ctx.intern("max"), vm::MagicConstant::new_ptr(&ctx, max));

    let clamp = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (val, min, max): (f64, f64, f64) = exec.stack().consume(ctx)?;
        if min > max {
            return Err(vm::RuntimeError::msg(format!(
                "{} > {} in `clamp`",
                min, max
            )));
        }
        exec.stack().replace(ctx, val.max(min).min(max));
        Ok(())
    });
    lib.insert(ctx.intern("clamp"), vm::MagicConstant::new_ptr(&ctx, clamp));

    struct Rng {
        rng: RefCell<SmallRng>,
    }

    impl Default for Rng {
        fn default() -> Self {
            Self {
                rng: RefCell::new(SmallRng::from_os_rng()),
            }
        }
    }

    type RngSingleton = gc_arena::Static<Rng>;

    let randomize = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        // NOTE: GMS2 only generates a u32 and FoM relies on this.
        let seed: u32 = rand::rng().random();
        *ctx.singleton::<RngSingleton>().rng.borrow_mut() = SmallRng::seed_from_u64(seed as u64);
        exec.stack().replace(ctx, seed as i64);
        Ok(())
    });
    lib.insert(
        ctx.intern("randomize"),
        vm::MagicConstant::new_ptr(&ctx, randomize),
    );

    let random_set_seed = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let seed: u32 = exec.stack().consume(ctx)?;
        *ctx.singleton::<RngSingleton>().rng.borrow_mut() = SmallRng::seed_from_u64(seed as u64);
        exec.stack().clear();
        Ok(())
    });
    lib.insert(
        ctx.intern("random_set_seed"),
        vm::MagicConstant::new_ptr(&ctx, random_set_seed),
    );

    let random = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let upper: f64 = exec.stack().consume(ctx)?;
        if upper < 0.0 {
            return Err(vm::RuntimeError::msg(format!(
                "`random` upper range {upper} cannot be <= 0.0"
            )));
        }
        let mut rng = ctx.singleton::<RngSingleton>().rng.borrow_mut();
        exec.stack().replace(ctx, rng.random_range(0.0..=upper));
        Ok(())
    });
    lib.insert(
        ctx.intern("random"),
        vm::MagicConstant::new_ptr(&ctx, random),
    );

    let irandom = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let upper: i64 = exec.stack().consume(ctx)?;
        if upper < 0 {
            return Err(vm::RuntimeError::msg(format!(
                "`irandom` upper range {upper} cannot be <= 0"
            )));
        }
        let mut rng = ctx.singleton::<RngSingleton>().rng.borrow_mut();
        exec.stack().replace(ctx, rng.random_range(0..=upper));
        Ok(())
    });
    lib.insert(
        ctx.intern("irandom"),
        vm::MagicConstant::new_ptr(&ctx, irandom),
    );

    let random_range = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (lower, upper): (f64, f64) = exec.stack().consume(ctx)?;
        if upper < lower {
            return Err(vm::RuntimeError::msg(format!(
                "`random_range`: invalid range [{lower}, {upper}]"
            )));
        }
        let mut rng = ctx.singleton::<RngSingleton>().rng.borrow_mut();
        exec.stack().replace(ctx, rng.random_range(lower..=upper));
        Ok(())
    });
    lib.insert(
        ctx.intern("random_range"),
        vm::MagicConstant::new_ptr(&ctx, random_range),
    );

    let irandom_range = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (lower, upper): (i64, i64) = exec.stack().consume(ctx)?;
        if upper < lower {
            return Err(vm::RuntimeError::msg(format!(
                "`irandom_range`: invalid range [{lower}, {upper}]"
            )));
        }
        let mut rng = ctx.singleton::<RngSingleton>().rng.borrow_mut();
        exec.stack().replace(ctx, rng.random_range(lower..=upper));
        Ok(())
    });
    lib.insert(
        ctx.intern("irandom_range"),
        vm::MagicConstant::new_ptr(&ctx, irandom_range),
    );

    let choose = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let mut stack = exec.stack();
        let mut rng = ctx.singleton::<RngSingleton>().rng.borrow_mut();
        let i = rng.random_range(0..stack.len());
        let v = stack[i];
        stack.replace(ctx, v);
        Ok(())
    });
    lib.insert(
        ctx.intern("choose"),
        vm::MagicConstant::new_ptr(&ctx, choose),
    );

    let array_shuffle = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (src, src_index, length): (vm::Array, Option<isize>, Option<isize>) =
            exec.stack().consume(ctx)?;
        let (src_range, is_reverse) = resolve_array_range(src.len(), src_index, length)?;

        let mut vals: Vec<_> = if is_reverse {
            src_range.rev().map(|i| src.get(i)).collect()
        } else {
            src_range.map(|i| src.get(i)).collect()
        };

        let mut rng = ctx.singleton::<RngSingleton>().rng.borrow_mut();
        vals.shuffle(&mut rng);

        exec.stack().replace(ctx, vm::Array::from_iter(&ctx, vals));
        Ok(())
    });
    lib.insert(
        ctx.intern("array_shuffle"),
        vm::MagicConstant::new_ptr(&ctx, array_shuffle),
    );

    let point_in_rectangle = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (px, py, xmin, ymin, xmax, ymax): (f64, f64, f64, f64, f64, f64) =
            exec.stack().consume(ctx)?;
        let inside = px >= xmin && px <= xmax && py >= ymin && py <= ymax;
        exec.stack().replace(ctx, inside);
        Ok(())
    });
    lib.insert(
        ctx.intern("point_in_rectangle"),
        vm::MagicConstant::new_ptr(&ctx, point_in_rectangle),
    );
}
