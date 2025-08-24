use std::iter;

use fabricator_vm as vm;

pub fn array_lib<'gc>(ctx: vm::Context<'gc>, lib: &mut vm::MagicSet<'gc>) {
    let array_create = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let mut stack = exec.stack();
        let (length, value): (usize, vm::Value) = stack.consume(ctx)?;
        let array = vm::Array::from_iter(&ctx, iter::repeat_n(value, length));
        stack.replace(ctx, array);
        Ok(())
    });
    lib.insert(
        ctx.intern("array_create"),
        vm::MagicConstant::new_ptr(&ctx, array_create),
    );

    let array_create_ext = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (length, create): (usize, vm::Function) = exec.stack().consume(ctx)?;
        let array = vm::Array::with_capacity(&ctx, length);

        for i in 0..length {
            exec.stack().replace(ctx, i as i64);
            exec.call(ctx, create)?;
            array.set(&ctx, i, exec.stack().get(0));
        }

        exec.stack().replace(ctx, array);
        Ok(())
    });
    lib.insert(
        ctx.intern("array_create_ext"),
        vm::MagicConstant::new_ptr(&ctx, array_create_ext),
    );

    let array_length = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let mut stack = exec.stack();
        let array: vm::Array = stack.consume(ctx)?;
        stack.replace(ctx, array.len() as i64);
        Ok(())
    });
    lib.insert(
        ctx.intern("array_length"),
        vm::MagicConstant::new_ptr(&ctx, array_length),
    );
}
