use std::f64;

use fabricator_vm as vm;

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
}
