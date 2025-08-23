use std::env;

use fabricator_vm::{self as vm, magic::MagicConstant};

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
    lib.insert(ctx.intern("typeof"), MagicConstant::new_ptr(&ctx, typeof_));

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
    lib.insert(ctx.intern("int64"), MagicConstant::new_ptr(&ctx, int64));

    let is_string = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: vm::Value = exec.stack().consume(ctx)?;
        exec.stack()
            .replace(ctx, matches!(arg, vm::Value::String(_)));
        Ok(())
    });
    lib.insert(
        ctx.intern("is_string"),
        MagicConstant::new_ptr(&ctx, is_string),
    );

    let is_struct = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: vm::Value = exec.stack().consume(ctx)?;
        exec.stack()
            .replace(ctx, matches!(arg, vm::Value::Object(_)));
        Ok(())
    });
    lib.insert(
        ctx.intern("is_struct"),
        MagicConstant::new_ptr(&ctx, is_struct),
    );

    let is_array = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: vm::Value = exec.stack().consume(ctx)?;
        exec.stack()
            .replace(ctx, matches!(arg, vm::Value::Array(_)));
        Ok(())
    });
    lib.insert(
        ctx.intern("is_array"),
        MagicConstant::new_ptr(&ctx, is_array),
    );

    lib.insert(
        ctx.intern("os_type"),
        MagicConstant::new_ptr(&ctx, ctx.intern(env::consts::OS)),
    );
    lib.insert(
        ctx.intern("os_windows"),
        MagicConstant::new_ptr(&ctx, ctx.intern("windows")),
    );
    lib.insert(
        ctx.intern("os_macosx"),
        MagicConstant::new_ptr(&ctx, ctx.intern("macos")),
    );
    lib.insert(
        ctx.intern("os_linux"),
        MagicConstant::new_ptr(&ctx, ctx.intern("linux")),
    );
    lib.insert(
        ctx.intern("os_switch"),
        MagicConstant::new_ptr(&ctx, ctx.intern("switch")),
    );
    lib.insert(
        ctx.intern("os_ps4"),
        MagicConstant::new_ptr(&ctx, ctx.intern("ps4")),
    );
    lib.insert(
        ctx.intern("os_ps5"),
        MagicConstant::new_ptr(&ctx, ctx.intern("ps5")),
    );
    lib.insert(
        ctx.intern("os_gdk"),
        MagicConstant::new_ptr(&ctx, ctx.intern("gdk")),
    );
    lib.insert(
        ctx.intern("os_xboxseriesx"),
        MagicConstant::new_ptr(&ctx, ctx.intern("xboxseriesx")),
    );
}
