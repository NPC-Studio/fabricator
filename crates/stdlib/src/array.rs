use std::iter;

use fabricator_vm::{
    self as vm,
    magic::{MagicConstant, MagicSet},
};

pub fn array_lib<'gc>(ctx: vm::Context<'gc>, lib: &mut MagicSet<'gc>) {
    let array_create = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let mut stack = exec.stack();
        let (length, value): (usize, vm::Value) = stack.consume(ctx)?;
        let array = vm::Array::from_iter(&ctx, iter::repeat_n(value, length));
        stack.replace(ctx, array);
        Ok(())
    });
    lib.insert(
        ctx.intern("array_create"),
        MagicConstant::new_ptr(&ctx, array_create.into()),
    );

    let array_length = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let mut stack = exec.stack();
        let array: vm::Array = stack.consume(ctx)?;
        stack.replace(ctx, array.len() as i64);
        Ok(())
    });
    lib.insert(
        ctx.intern("array_length"),
        MagicConstant::new_ptr(&ctx, array_length.into()),
    );
}
