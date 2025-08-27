use std::{
    cmp::{self, Ordering},
    error::Error,
    fmt, iter,
    ops::Range,
};

use fabricator_vm as vm;
use gc_arena::Gc;

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
                .map(|i| (range.end - 1 - i) as i64)
        } else {
            range_iter
                .position(|&v| v == value)
                .map(|i| (i + range.start) as i64)
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
}

#[derive(Debug, Copy, Clone)]
struct ArrayRangeError {
    index: isize,
    count: Option<isize>,
    array_len: usize,
}

impl fmt::Display for ArrayRangeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(count) = self.count {
            write!(
                f,
                "index {} and count {count} out of range of array with length {}",
                self.index, self.array_len
            )
        } else {
            write!(
                f,
                "index {} out of range of array with length {}",
                self.index, self.array_len
            )
        }
    }
}

impl Error for ArrayRangeError {}

fn resolve_array_range(
    array_len: usize,
    index: Option<isize>,
    count: Option<isize>,
) -> Result<(Range<usize>, bool), ArrayRangeError> {
    let index = index.unwrap_or(0);

    let mut err = ArrayRangeError {
        index,
        count,
        array_len,
    };

    let abs_index = if index < 0 {
        array_len.checked_add_signed(index).ok_or(err)?
    } else {
        index as usize
    };

    let count = count.unwrap_or((array_len - abs_index) as isize);
    err.count = Some(count);

    let (range, is_reverse) = if count < 0 {
        (
            abs_index.checked_add_signed(count + 1).ok_or(err)?
                ..abs_index.checked_add(1).ok_or(err)?,
            true,
        )
    } else {
        (
            abs_index..abs_index.checked_add_signed(count).ok_or(err)?,
            false,
        )
    };

    assert!(range.start <= range.end);
    if range.start > array_len || range.end > array_len {
        return Err(err);
    }

    Ok((range, is_reverse))
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
                exec.stack().replace(ctx, [lhs, rhs]);
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

    let sort_by = if let Some(func) = comparator.to_function() {
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
