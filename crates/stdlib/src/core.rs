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

        exec.stack().replace(ctx, type_name);
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

    let debug_get_callstack = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let max_depth: Option<usize> = exec.stack().consume(ctx)?;
        let frame_depth = exec.frame_depth();
        let max_depth = if let Some(max_depth) = max_depth {
            max_depth.min(frame_depth)
        } else {
            frame_depth
        };

        let array = vm::Array::new(&ctx);
        for frame in 0..max_depth {
            let frame_desc = match exec.upper_frame(frame) {
                vm::BacktraceFrame::Closure(closure_frame) => ctx.intern(&format!(
                    "{}:{}",
                    closure_frame.chunk_name(),
                    closure_frame.line_number(),
                )),
                vm::BacktraceFrame::Callback(callback) => {
                    ctx.intern(&vm::Value::Callback(callback).to_string())
                }
            };
            array.push(&ctx, frame_desc);
        }

        exec.stack().push_back(array);
        Ok(())
    });
    lib.insert(
        ctx.intern("debug_get_callstack"),
        vm::MagicConstant::new_ptr(&ctx, debug_get_callstack),
    );

    let script_execute = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let func: vm::Function = exec.stack().from_front(ctx)?;
        exec.call(ctx, func)
    });
    lib.insert(
        ctx.intern("script_execute"),
        vm::MagicConstant::new_ptr(&ctx, script_execute),
    );

    let array_concat = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let array = vm::Array::new(&ctx);
        for i in 0..exec.stack().len() {
            let arr: vm::Array = exec.stack().from_index(ctx, i)?;
            array.extend(&ctx, arr.borrow().iter().copied());
        }
        exec.stack().replace(ctx, array);
        Ok(())
    });
    lib.insert(
        ctx.intern("array_concat"),
        vm::MagicConstant::new_ptr(&ctx, array_concat),
    );

    let struct_get_names = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let obj: vm::Object = exec.stack().consume(ctx)?;
        let keys = vm::Array::from_iter(&ctx, obj.borrow().map.keys().map(|&k| k.into()));
        exec.stack().replace(ctx, keys);
        Ok(())
    });
    lib.insert(
        ctx.intern("struct_get_names"),
        vm::MagicConstant::new_ptr(&ctx, struct_get_names),
    );

    let struct_exists = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (obj, key): (vm::Object, vm::String) = exec.stack().consume(ctx)?;
        exec.stack().replace(ctx, obj.get(key).is_some());
        Ok(())
    });
    lib.insert(
        ctx.intern("struct_exists"),
        vm::MagicConstant::new_ptr(&ctx, struct_exists),
    );
}
