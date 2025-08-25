use fabricator_vm as vm;

pub fn core_lib<'gc>(ctx: vm::Context<'gc>, lib: &mut vm::MagicSet<'gc>) {
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
        vm::MagicConstant::new_ptr(&ctx, typeof_),
    );

    let int64 = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: vm::Value = exec.stack().consume(ctx)?;
        let int = if let Some(i) = arg.cast_integer() {
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
    lib.insert(ctx.intern("int64"), vm::MagicConstant::new_ptr(&ctx, int64));

    let is_numeric = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: vm::Value = exec.stack().consume(ctx)?;
        exec.stack().replace(ctx, arg.cast_float().is_some());
        Ok(())
    });
    lib.insert(
        ctx.intern("is_numeric"),
        vm::MagicConstant::new_ptr(&ctx, is_numeric),
    );

    let is_string = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: vm::Value = exec.stack().consume(ctx)?;
        exec.stack()
            .replace(ctx, matches!(arg, vm::Value::String(_)));
        Ok(())
    });
    lib.insert(
        ctx.intern("is_string"),
        vm::MagicConstant::new_ptr(&ctx, is_string),
    );

    let is_struct = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: vm::Value = exec.stack().consume(ctx)?;
        exec.stack()
            .replace(ctx, matches!(arg, vm::Value::Object(_)));
        Ok(())
    });
    lib.insert(
        ctx.intern("is_struct"),
        vm::MagicConstant::new_ptr(&ctx, is_struct),
    );

    let is_array = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: vm::Value = exec.stack().consume(ctx)?;
        exec.stack()
            .replace(ctx, matches!(arg, vm::Value::Array(_)));
        Ok(())
    });
    lib.insert(
        ctx.intern("is_array"),
        vm::MagicConstant::new_ptr(&ctx, is_array),
    );
}
