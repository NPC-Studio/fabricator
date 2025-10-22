use fabricator_vm as vm;

use crate::util::{Pointer, resolve_array_range};

pub fn core_lib<'gc>(ctx: vm::Context<'gc>, lib: &mut vm::MagicSet<'gc>) {
    // `pointer_null` is not used by fabricator.
    lib.insert(
        ctx.intern("pointer_null"),
        vm::MagicConstant::new_ptr(&ctx, vm::UserData::new_static(&ctx, ())),
    );

    // `gml_pragma` is currently ignored by fabricator.
    lib.insert(
        ctx.intern("gml_pragma"),
        vm::MagicConstant::new_ptr(
            &ctx,
            vm::Callback::from_fn(&ctx, |_, mut exec| {
                exec.stack().clear();
                Ok(())
            }),
        ),
    );

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

    let bool = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: vm::Value = exec.stack().consume(ctx)?;
        exec.stack().replace(ctx, arg.cast_bool());
        Ok(())
    });
    lib.insert(ctx.intern("bool"), vm::MagicConstant::new_ptr(&ctx, bool));

    let int64 = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: vm::Value = exec.stack().consume(ctx)?;
        let int = if let Some(i) = arg.coerce_integer(ctx) {
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

    let real = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: vm::Value = exec.stack().consume(ctx)?;
        let Some(float) = arg.coerce_float(ctx) else {
            return Err(vm::TypeError {
                expected: "value coercible to float",
                found: arg.type_name(),
            }
            .into());
        };
        exec.stack().replace(ctx, float);
        Ok(())
    });
    lib.insert(ctx.intern("real"), vm::MagicConstant::new_ptr(&ctx, real));

    let is_numeric = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: vm::Value = exec.stack().consume(ctx)?;
        exec.stack().replace(ctx, arg.cast_float().is_some());
        Ok(())
    });
    lib.insert(
        ctx.intern("is_numeric"),
        vm::MagicConstant::new_ptr(&ctx, is_numeric),
    );

    let is_real = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: vm::Value = exec.stack().consume(ctx)?;
        exec.stack()
            .replace(ctx, arg.as_float().is_some() || arg.as_integer().is_some());
        Ok(())
    });
    lib.insert(
        ctx.intern("is_real"),
        vm::MagicConstant::new_ptr(&ctx, is_real),
    );

    let is_int64 = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: vm::Value = exec.stack().consume(ctx)?;
        exec.stack().replace(ctx, arg.as_integer().is_some());
        Ok(())
    });
    lib.insert(
        ctx.intern("is_int64"),
        vm::MagicConstant::new_ptr(&ctx, is_int64),
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

    lib.insert(
        ctx.intern("is_ptr"),
        vm::MagicConstant::new_ptr(
            &ctx,
            vm::Callback::from_fn(&ctx, |ctx, mut exec| {
                let arg: vm::Value = exec.stack().consume(ctx)?;
                exec.stack().replace(
                    ctx,
                    matches!(arg, vm::Value::UserData(u) if Pointer::is_pointer(u)),
                );

                Ok(())
            }),
        ),
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
        let mut func: vm::Function = exec.stack().from_front(ctx)?;
        // `script_execute` is documented as calling the provided function in the *calling context*,
        // even if it is a bound method.
        if !func.this().is_undefined() {
            // NOTE: This allocates, to avoid this we could add a feature to call closures and
            // callbacks while ignoring any bound `this`.
            func = func.rebind(&ctx, vm::Value::Undefined);
        }
        Ok(exec.call(ctx, func)?)
    });
    lib.insert(
        ctx.intern("script_execute"),
        vm::MagicConstant::new_ptr(&ctx, script_execute),
    );

    let script_execute_ext = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (mut func, args, offset, count): (
            vm::Function,
            Option<vm::Array>,
            Option<isize>,
            Option<isize>,
        ) = exec.stack().consume(ctx)?;
        if let Some(args) = args {
            let (range, reverse) = resolve_array_range(args.len(), offset, count)?;
            if reverse {
                for i in range.rev() {
                    exec.stack().push_back(args.get(i));
                }
            } else {
                for i in range {
                    exec.stack().push_back(args.get(i));
                }
            }
        }

        // `script_execute_ext` is documented as calling the provided function in the *calling
        // context*, even if it is a bound method.
        if !func.this().is_undefined() {
            // NOTE: This allocates, see the implementation of `script_execute`.
            func = func.rebind(&ctx, vm::Value::Undefined);
        }
        Ok(exec.call(ctx, func)?)
    });
    lib.insert(
        ctx.intern("script_execute_ext"),
        vm::MagicConstant::new_ptr(&ctx, script_execute_ext),
    );

    let method_call = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (func, args, offset, count): (
            vm::Function,
            Option<vm::Array>,
            Option<isize>,
            Option<isize>,
        ) = exec.stack().consume(ctx)?;
        if let Some(args) = args {
            let (range, reverse) = resolve_array_range(args.len(), offset, count)?;
            if reverse {
                for i in range.rev() {
                    exec.stack().push_back(args.get(i));
                }
            } else {
                for i in range {
                    exec.stack().push_back(args.get(i));
                }
            }
        }
        Ok(exec.call(ctx, func)?)
    });
    lib.insert(
        ctx.intern("method_call"),
        vm::MagicConstant::new_ptr(&ctx, method_call),
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

    let struct_remove = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (obj, key): (vm::Object, vm::Value) = exec.stack().consume(ctx)?;
        let key = key
            .coerce_string(ctx)
            .ok_or_else(|| vm::RuntimeError::msg("key not coercible to string"))?;
        obj.remove(&ctx, key);
        Ok(())
    });
    lib.insert(
        ctx.intern("struct_remove"),
        vm::MagicConstant::new_ptr(&ctx, struct_remove),
    );

    let struct_exists = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (obj, key): (vm::Object, vm::Value) = exec.stack().consume(ctx)?;
        let key = key
            .coerce_string(ctx)
            .ok_or_else(|| vm::RuntimeError::msg("key not coercible to string"))?;
        exec.stack().replace(ctx, obj.get(key).is_some());
        Ok(())
    });
    lib.insert(
        ctx.intern("struct_exists"),
        vm::MagicConstant::new_ptr(&ctx, struct_exists),
    );
}
