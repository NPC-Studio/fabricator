use std::f64;

use fabricator_vm as vm;
use rand::Rng;

pub fn math_lib<'gc>(ctx: vm::Context<'gc>, lib: &mut vm::MagicSet<'gc>) {
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

    let round = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: f64 = exec.stack().consume(ctx)?;
        exec.stack().replace(ctx, arg.round());
        Ok(())
    });
    lib.insert(ctx.intern("round"), vm::MagicConstant::new_ptr(&ctx, round));

    let floor = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: f64 = exec.stack().consume(ctx)?;
        exec.stack().replace(ctx, arg.floor());
        Ok(())
    });
    lib.insert(ctx.intern("floor"), vm::MagicConstant::new_ptr(&ctx, floor));

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
            return Err(format!("{} > {} in `clamp`", min, max).into());
        }
        exec.stack().replace(ctx, val.max(min).min(max));
        Ok(())
    });
    lib.insert(ctx.intern("clamp"), vm::MagicConstant::new_ptr(&ctx, clamp));

    let random = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let upper: f64 = exec.stack().consume(ctx)?;
        if upper <= 0.0 {
            return Err(format!("`random` upper range {upper} cannot be <= 0.0").into());
        }
        exec.stack()
            .replace(ctx, rand::rng().random_range(0.0..=upper));
        Ok(())
    });
    lib.insert(
        ctx.intern("random"),
        vm::MagicConstant::new_ptr(&ctx, random),
    );

    let choose = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let mut stack = exec.stack();
        let i = rand::rng().random_range(0..stack.len());
        let v = stack[i];
        stack.replace(ctx, v);
        Ok(())
    });
    lib.insert(
        ctx.intern("choose"),
        vm::MagicConstant::new_ptr(&ctx, choose),
    );
}
