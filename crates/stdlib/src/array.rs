use std::{
    cmp::{self, Ordering},
    iter,
};

use fabricator_vm as vm;
use gc_arena::Gc;

use crate::util::{resolve_array_index, resolve_array_range};

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
            exec.stack().replace(ctx, i as isize);
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
        stack.replace(ctx, array.len() as isize);
        Ok(())
    });
    lib.insert(
        ctx.intern("array_length"),
        vm::MagicConstant::new_ptr(&ctx, array_length),
    );

    let array_delete = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (array, index, count): (vm::Array, isize, isize) = exec.stack().consume(ctx)?;
        let (range, _) = resolve_array_range(array.len(), Some(index), Some(count))?;
        array.borrow_mut(&ctx).drain(range);
        Ok(())
    });
    lib.insert(
        ctx.intern("array_delete"),
        vm::MagicConstant::new_ptr(&ctx, array_delete),
    );

    let array_get_index = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (array, value, offset, length): (vm::Array, vm::Value, Option<isize>, Option<isize>) =
            exec.stack().consume(ctx)?;

        let (range, is_reverse) = resolve_array_range(array.len(), offset, length)?;
        let array = array.borrow();
        let mut range_iter = array[range.clone()].iter();

        let idx = if is_reverse {
            range_iter
                .rev()
                .position(|&v| v == value)
                .map(|i| (range.end - 1 - i) as isize)
        } else {
            range_iter
                .position(|&v| v == value)
                .map(|i| (i + range.start) as isize)
        }
        .unwrap_or(-1);

        exec.stack().replace(ctx, idx);
        Ok(())
    });
    lib.insert(
        ctx.intern("array_get_index"),
        vm::MagicConstant::new_ptr(&ctx, array_get_index),
    );

    let array_push = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let array: vm::Array = exec.stack().from_index(ctx, 0)?;
        for &value in &exec.stack()[1..] {
            array.push(&ctx, value)
        }
        exec.stack().clear();
        Ok(())
    });
    lib.insert(
        ctx.intern("array_push"),
        vm::MagicConstant::new_ptr(&ctx, array_push),
    );

    let array_pop = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let array: vm::Array = exec.stack().consume(ctx)?;
        exec.stack().replace(ctx, array.pop(&ctx));
        Ok(())
    });
    lib.insert(
        ctx.intern("array_pop"),
        vm::MagicConstant::new_ptr(&ctx, array_pop),
    );

    let array_sort = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (array, comparator): (vm::Array, Option<vm::Value>) = exec.stack().consume(ctx)?;
        sort_array(
            ctx,
            exec.reborrow(),
            array,
            comparator.unwrap_or(true.into()),
        )?;
        Ok(())
    });
    lib.insert(
        ctx.intern("array_sort"),
        vm::MagicConstant::new_ptr(&ctx, array_sort),
    );

    let array_contains = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (array, value, index, count): (vm::Array, vm::Value, Option<isize>, Option<isize>) =
            exec.stack().consume(ctx)?;
        let (range, _) = resolve_array_range(array.len(), index, count)?;
        exec.stack()
            .replace(ctx, array.borrow_mut(&ctx)[range].contains(&value));
        Ok(())
    });
    lib.insert(
        ctx.intern("array_contains"),
        vm::MagicConstant::new_ptr(&ctx, array_contains),
    );

    let array_map = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (input, function, index, count): (
            vm::Array,
            vm::Function,
            Option<isize>,
            Option<isize>,
        ) = exec.stack().consume(ctx)?;
        let (range, _) = resolve_array_range(input.len(), index, count)?;
        let output = vm::Array::with_capacity(&ctx, range.len());
        for i in range {
            exec.stack().replace(ctx, (input.get(i), i as isize));
            exec.call(ctx, function)?;
            output.set(&ctx, i, exec.stack().get(0));
        }
        exec.stack().replace(ctx, output);
        Ok(())
    });
    lib.insert(
        ctx.intern("array_map"),
        vm::MagicConstant::new_ptr(&ctx, array_map),
    );

    let array_copy = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (dest, dest_index, src, src_index, length): (
            vm::Array,
            isize,
            vm::Array,
            isize,
            isize,
        ) = exec.stack().consume(ctx)?;
        let dest_index = resolve_array_index(dest.len(), Some(dest_index))?;
        let (src_range, is_reverse) =
            resolve_array_range(src.len(), Some(src_index), Some(length))?;

        if is_reverse {
            for (i, ind) in src_range.rev().enumerate() {
                dest.set(&ctx, dest_index + i, src.get(ind));
            }
        } else {
            for (i, ind) in src_range.enumerate() {
                dest.set(&ctx, dest_index + i, src.get(ind));
            }
        }
        Ok(())
    });
    lib.insert(
        ctx.intern("array_copy"),
        vm::MagicConstant::new_ptr(&ctx, array_copy),
    );

    let array_resize = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (array, new_len): (vm::Array, usize) = exec.stack().consume(ctx)?;
        array.borrow_mut(&ctx).resize(new_len, vm::Value::Undefined);
        Ok(())
    });
    lib.insert(
        ctx.intern("array_resize"),
        vm::MagicConstant::new_ptr(&ctx, array_resize),
    );
}

fn sort_array<'gc>(
    ctx: vm::Context<'gc>,
    mut exec: vm::Execution<'gc, '_>,
    array: vm::Array<'gc>,
    comparator: vm::Value<'gc>,
) -> Result<(), vm::RuntimeError> {
    #[derive(Copy, Clone)]
    enum SortBy<'gc> {
        Ascending,
        Descending,
        Custom(vm::Function<'gc>),
    }

    fn value_cmp<'gc>(lhs: vm::Value<'gc>, rhs: vm::Value<'gc>) -> cmp::Ordering {
        // The GMS2 documentation for `array_sort` barely covers what the "default sort order" is,
        // but we must make a total order for all values.

        #[derive(Copy, Clone)]
        struct TotalNum(f64);

        impl Ord for TotalNum {
            fn cmp(&self, other: &Self) -> Ordering {
                self.0.total_cmp(&other.0)
            }
        }

        impl PartialEq for TotalNum {
            fn eq(&self, other: &Self) -> bool {
                self.cmp(other).is_eq()
            }
        }

        impl Eq for TotalNum {}

        impl PartialOrd for TotalNum {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                Some(self.cmp(other))
            }
        }

        // We categorize values into four types and sort them independently. This does *not* treat
        // numbers "stringly", which seems to match the GMS2 documentation which states:
        //
        //   If the array contains a set of strings, then the strings will be sorted alphabetically
        //   based on the English alphabet when using the default ascending/descending sort type.
        //   All other data types will be sorted based on their numerical value, the exact values
        //   of which will depend on the data type itself.
        //
        // All strings will be sorted before all numerical scalars which will be sorted before all
        // heap values which will be sorted before any instances of `undefined`.
        #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
        enum SortValue<'a> {
            String(&'a str),
            Numeric(TotalNum),
            Pointer(*const ()),
            Undefined,
        }

        fn to_sort_value<'gc>(value: vm::Value<'gc>) -> SortValue<'gc> {
            match value {
                vm::Value::Undefined => SortValue::Undefined,
                vm::Value::Boolean(_) | vm::Value::Integer(_) | vm::Value::Float(_) => {
                    SortValue::Numeric(TotalNum(value.cast_float().unwrap()))
                }
                vm::Value::String(s) => SortValue::String(s.as_str()),
                vm::Value::Object(o) => SortValue::Pointer(Gc::as_ptr(o.into_inner()) as *const ()),
                vm::Value::Array(a) => SortValue::Pointer(Gc::as_ptr(a.into_inner()) as *const ()),
                vm::Value::Closure(c) => {
                    SortValue::Pointer(Gc::as_ptr(c.into_inner()) as *const ())
                }
                vm::Value::Callback(c) => {
                    SortValue::Pointer(Gc::as_ptr(c.into_inner()) as *const ())
                }
                vm::Value::UserData(u) => {
                    SortValue::Pointer(Gc::as_ptr(u.into_inner()) as *const ())
                }
            }
        }

        to_sort_value(lhs).cmp(&to_sort_value(rhs))
    }

    fn cmp_by<'gc>(
        ctx: vm::Context<'gc>,
        exec: &mut vm::Execution<'gc, '_>,
        sort_by: SortBy<'gc>,
        lhs: vm::Value<'gc>,
        rhs: vm::Value<'gc>,
    ) -> Result<cmp::Ordering, vm::RuntimeError> {
        Ok(match sort_by {
            SortBy::Ascending => value_cmp(lhs, rhs),
            SortBy::Descending => value_cmp(rhs, lhs),
            SortBy::Custom(func) => {
                exec.stack().replace(ctx, (lhs, rhs));
                exec.call(ctx, func)?;
                let n: f64 = exec.stack().consume(ctx)?;
                if n < 0.0 {
                    cmp::Ordering::Less
                } else if n > 0.0 {
                    cmp::Ordering::Greater
                } else if n == 0.0 {
                    cmp::Ordering::Equal
                } else {
                    return Err("numeric value returned by comparator is NaN".into());
                }
            }
        })
    }

    fn partition<'gc>(
        ctx: vm::Context<'gc>,
        exec: &mut vm::Execution<'gc, '_>,
        sort_by: SortBy<'gc>,
        array: &mut [vm::Value<'gc>],
        pivot: usize,
    ) -> Result<usize, vm::RuntimeError> {
        let last = array.len() - 1;
        array.swap(pivot, last);
        let mut i = 0;
        for j in 0..last {
            if cmp_by(ctx, exec, sort_by, array[j], array[last])?.is_lt() {
                array.swap(i, j);
                i += 1;
            }
        }
        array.swap(i, last);
        Ok(i)
    }

    fn quicksort<'gc>(
        ctx: vm::Context<'gc>,
        exec: &mut vm::Execution<'gc, '_>,
        sort_by: SortBy<'gc>,
        rng: &mut impl rand::Rng,
        array: &mut [vm::Value<'gc>],
    ) -> Result<(), vm::RuntimeError> {
        if array.len() <= 1 {
            return Ok(());
        }

        let pivot = partition(ctx, exec, sort_by, array, rng.random_range(0..array.len()))?;
        let (left, right) = array.split_at_mut(pivot);

        quicksort(ctx, exec, sort_by, rng, left)?;
        quicksort(ctx, exec, sort_by, rng, &mut right[1..])?;
        Ok(())
    }

    let sort_by = if let Some(func) = comparator.as_function() {
        SortBy::Custom(func)
    } else if comparator.cast_bool() {
        SortBy::Ascending
    } else {
        SortBy::Descending
    };

    quicksort(
        ctx,
        &mut exec,
        sort_by,
        &mut rand::rng(),
        &mut array.borrow_mut(&ctx),
    )
}
