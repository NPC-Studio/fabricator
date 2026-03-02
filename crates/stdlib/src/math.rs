use std::{cell::RefCell, convert::Infallible, f64};

use fabricator_vm as vm;
use rand::{Rng as _, SeedableRng, rngs::SmallRng, seq::SliceRandom as _};

use crate::util::{MagicExt as _, resolve_array_range};

pub fn cos<'gc>(_ctx: vm::Context<'gc>, arg: f64) -> Result<f64, Infallible> {
    Ok(arg.cos())
}

pub fn sin<'gc>(_ctx: vm::Context<'gc>, arg: f64) -> Result<f64, Infallible> {
    Ok(arg.sin())
}

pub fn abs<'gc>(_ctx: vm::Context<'gc>, arg: f64) -> Result<f64, Infallible> {
    Ok(arg.abs())
}

pub fn sqrt<'gc>(_ctx: vm::Context<'gc>, arg: f64) -> Result<f64, Infallible> {
    Ok(arg.sqrt())
}

pub fn sqr<'gc>(_ctx: vm::Context<'gc>, arg: f64) -> Result<f64, Infallible> {
    Ok(arg * arg)
}

pub fn power<'gc>(_ctx: vm::Context<'gc>, (arg, exp): (f64, f64)) -> Result<f64, Infallible> {
    Ok(arg.powf(exp))
}

pub fn round<'gc>(_ctx: vm::Context<'gc>, arg: f64) -> Result<f64, Infallible> {
    Ok(arg.round_ties_even())
}

pub fn floor<'gc>(_ctx: vm::Context<'gc>, arg: f64) -> Result<f64, Infallible> {
    Ok(arg.floor())
}

pub fn ceil<'gc>(_ctx: vm::Context<'gc>, arg: f64) -> Result<f64, Infallible> {
    Ok(arg.ceil())
}

pub fn sign<'gc>(_ctx: vm::Context<'gc>, arg: f64) -> Result<f64, Infallible> {
    Ok(if arg > 0.0 {
        1.0
    } else if arg < 0.0 {
        -1.0
    } else {
        0.0
    })
}

pub fn min<'gc>(
    ctx: vm::Context<'gc>,
    mut exec: vm::Execution<'gc, '_>,
) -> Result<(), vm::RuntimeError> {
    let mut min: f64 = exec.stack().from_index(ctx, 0)?;
    for i in 1..exec.stack().len() {
        min = min.min(exec.stack().from_index::<f64>(ctx, i)?);
    }
    exec.stack().replace(ctx, min);
    Ok(())
}

pub fn max<'gc>(
    ctx: vm::Context<'gc>,
    mut exec: vm::Execution<'gc, '_>,
) -> Result<(), vm::RuntimeError> {
    let mut max: f64 = exec.stack().from_index(ctx, 0)?;
    for i in 1..exec.stack().len() {
        max = max.max(exec.stack().from_index::<f64>(ctx, i)?);
    }
    exec.stack().replace(ctx, max);
    Ok(())
}

pub fn clamp<'gc>(
    _ctx: vm::Context<'gc>,
    (val, min, max): (f64, f64, f64),
) -> Result<f64, vm::RuntimeError> {
    if min > max {
        return Err(vm::RuntimeError::msg(format!(
            "{} > {} in `clamp`",
            min, max
        )));
    }
    Ok(val.max(min).min(max))
}

pub struct Rng {
    rng: RefCell<SmallRng>,
}

impl Default for Rng {
    fn default() -> Self {
        Self {
            rng: RefCell::new(SmallRng::from_os_rng()),
        }
    }
}

pub type RngSingleton = gc_arena::Static<Rng>;

pub fn randomize<'gc>(ctx: vm::Context<'gc>, (): ()) -> Result<u32, Infallible> {
    // NOTE: GMS2 only generates a u32 and FoM relies on this.
    let seed: u32 = rand::rng().random();
    *ctx.singleton::<RngSingleton>().rng.borrow_mut() = SmallRng::seed_from_u64(seed as u64);
    Ok(seed)
}

pub fn random_set_seed<'gc>(ctx: vm::Context<'gc>, seed: u32) -> Result<(), Infallible> {
    *ctx.singleton::<RngSingleton>().rng.borrow_mut() = SmallRng::seed_from_u64(seed as u64);
    Ok(())
}

pub fn random<'gc>(ctx: vm::Context<'gc>, upper: f64) -> Result<f64, vm::RuntimeError> {
    if upper < 0.0 {
        return Err(vm::RuntimeError::msg(format!(
            "`random` upper range {upper} cannot be <= 0.0"
        )));
    }
    let mut rng = ctx.singleton::<RngSingleton>().rng.borrow_mut();
    Ok(rng.random_range(0.0..=upper))
}

pub fn irandom<'gc>(ctx: vm::Context<'gc>, upper: i64) -> Result<i64, vm::RuntimeError> {
    if upper < 0 {
        return Err(vm::RuntimeError::msg(format!(
            "`irandom` upper range {upper} cannot be <= 0"
        )));
    }
    let mut rng = ctx.singleton::<RngSingleton>().rng.borrow_mut();
    Ok(rng.random_range(0..=upper))
}

pub fn random_range<'gc>(
    ctx: vm::Context<'gc>,
    (lower, upper): (f64, f64),
) -> Result<f64, vm::RuntimeError> {
    if upper < lower {
        return Err(vm::RuntimeError::msg(format!(
            "`random_range`: invalid range [{lower}, {upper}]"
        )));
    }
    let mut rng = ctx.singleton::<RngSingleton>().rng.borrow_mut();
    Ok(rng.random_range(lower..=upper))
}

pub fn irandom_range<'gc>(
    ctx: vm::Context<'gc>,
    (lower, upper): (i64, i64),
) -> Result<i64, vm::RuntimeError> {
    if upper < lower {
        return Err(vm::RuntimeError::msg(format!(
            "`irandom_range`: invalid range [{lower}, {upper}]"
        )));
    }
    let mut rng = ctx.singleton::<RngSingleton>().rng.borrow_mut();
    Ok(rng.random_range(lower..=upper))
}

pub fn choose<'gc>(
    ctx: vm::Context<'gc>,
    mut exec: vm::Execution<'gc, '_>,
) -> Result<(), Infallible> {
    let mut stack = exec.stack();
    let mut rng = ctx.singleton::<RngSingleton>().rng.borrow_mut();
    let i = rng.random_range(0..stack.len());
    let v = stack[i];
    stack.replace(ctx, v);
    Ok(())
}

pub fn array_shuffle<'gc>(
    ctx: vm::Context<'gc>,
    (src, src_index, length): (vm::Array<'gc>, Option<isize>, Option<isize>),
) -> Result<vm::Array<'gc>, vm::RuntimeError> {
    let (src_range, is_reverse) = resolve_array_range(src.len(), src_index, length)?;

    let mut vals: Vec<_> = if is_reverse {
        src_range.rev().map(|i| src.get(i)).collect()
    } else {
        src_range.map(|i| src.get(i)).collect()
    };

    let mut rng = ctx.singleton::<RngSingleton>().rng.borrow_mut();
    vals.shuffle(&mut rng);

    Ok(vm::Array::from_iter(&ctx, vals))
}

pub fn point_in_rectangle<'gc>(
    _ctx: vm::Context<'gc>,
    (px, py, xmin, ymin, xmax, ymax): (f64, f64, f64, f64, f64, f64),
) -> Result<bool, Infallible> {
    Ok(px >= xmin && px <= xmax && py >= ymin && py <= ymax)
}

pub fn math_lib<'gc>(ctx: vm::Context<'gc>, lib: &mut vm::MagicSet<'gc>) {
    lib.insert_constant(ctx, "NaN", f64::NAN);
    lib.insert_constant(ctx, "infinity", f64::INFINITY);
    lib.insert_constant(ctx, "pi", f64::consts::PI);
    lib.insert_callback(ctx, "cos", cos);
    lib.insert_callback(ctx, "sin", sin);
    lib.insert_callback(ctx, "abs", abs);
    lib.insert_callback(ctx, "sqrt", sqrt);
    lib.insert_callback(ctx, "sqr", sqr);
    lib.insert_callback(ctx, "power", power);
    lib.insert_callback(ctx, "round", round);
    lib.insert_callback(ctx, "floor", floor);
    lib.insert_callback(ctx, "ceil", ceil);
    lib.insert_callback(ctx, "sign", sign);
    lib.insert_exec_callback(ctx, "min", min);
    lib.insert_exec_callback(ctx, "max", max);
    lib.insert_callback(ctx, "clamp", clamp);
    lib.insert_callback(ctx, "randomize", randomize);
    lib.insert_callback(ctx, "random_set_seed", random_set_seed);
    lib.insert_callback(ctx, "random", random);
    lib.insert_callback(ctx, "irandom", irandom);
    lib.insert_callback(ctx, "random_range", random_range);
    lib.insert_callback(ctx, "irandom_range", irandom_range);
    lib.insert_exec_callback(ctx, "choose", choose);
    lib.insert_callback(ctx, "array_shuffle", array_shuffle);
    lib.insert_callback(ctx, "point_in_rectangle", point_in_rectangle);
}
