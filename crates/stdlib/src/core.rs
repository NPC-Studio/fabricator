use fabricator_vm::{
    self as vm,
    magic::{MagicConstant, MagicSet},
};

pub fn core_lib<'gc>(ctx: vm::Context<'gc>, lib: &mut MagicSet<'gc>) {
    let typeof_ = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        // Return a *roughly* GML compatible set of type names.
        let type_name = match exec.stack().consume(ctx)? {
            vm::Value::Undefined => "undefined",
            vm::Value::Boolean(_) => "bool",
            vm::Value::Integer(_) => "int64",
            vm::Value::Float(_) => "number",
            vm::Value::String(_) => "string",
            vm::Value::Object(_) => "struct",
            vm::Value::Array(_) => "array",
            vm::Value::Closure(_) => "method",
            vm::Value::Callback(_) => "method",
            vm::Value::UserData(_) => "ptr",
        };

        exec.stack().replace(ctx, ctx.intern(type_name));
        Ok(())
    });
    lib.insert(
        ctx.intern("typeof"),
        MagicConstant::new_ptr(&ctx, typeof_.into()),
    );

    let int64 = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: vm::Value = exec.stack().consume(ctx)?;
        let int = if let Some(i) = arg.to_integer() {
            i
        } else if let vm::Value::String(i) = arg {
            i.parse()?
        } else {
            return Err(vm::TypeError {
                expected: "number or string",
                found: arg.type_name(),
            }
            .into());
        };
        exec.stack().replace(ctx, int);
        Ok(())
    });
    lib.insert(
        ctx.intern("int64"),
        MagicConstant::new_ptr(&ctx, int64.into()),
    );
}
