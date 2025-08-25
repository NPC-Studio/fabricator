use std::{error::Error, fmt, iter, ops::Range};

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
