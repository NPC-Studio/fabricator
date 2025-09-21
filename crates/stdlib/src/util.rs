use std::{
    ops::{self, Range},
    ptr::NonNull,
};

use fabricator_vm as vm;
use gc_arena::Mutation;
use thiserror::Error;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Pointer(pub NonNull<u8>);

impl From<NonNull<u8>> for Pointer {
    fn from(value: NonNull<u8>) -> Self {
        Self(value)
    }
}

impl ops::Deref for Pointer {
    type Target = NonNull<u8>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Pointer {
    pub fn new(p: *mut u8) -> Option<Self> {
        Some(Self(NonNull::new(p)?))
    }

    pub fn into_userdata<'gc>(self, mc: &Mutation<'gc>) -> vm::UserData<'gc> {
        vm::UserData::new_static(mc, self)
    }

    pub fn is_pointer<'gc>(ud: vm::UserData<'gc>) -> bool {
        ud.is_static::<Pointer>()
    }

    pub fn downcast<'gc>(ud: vm::UserData<'gc>) -> Result<Pointer, vm::user_data::BadUserDataType> {
        Ok(*ud.downcast_static::<Pointer>()?)
    }

    pub fn into_inner(self) -> NonNull<u8> {
        self.0
    }
}

#[derive(Debug, Copy, Clone, Error)]
#[error("index {index} out of range of array with length {array_len}")]
pub struct ArrayIndexError {
    index: isize,
    array_len: usize,
}

#[derive(Debug, Copy, Clone, Error)]
#[error("index {index} and count {count} out of range of array with length {array_len}")]
pub struct ArrayRangeError {
    index: isize,
    count: isize,
    array_len: usize,
}

pub fn resolve_array_index(
    array_len: usize,
    index: Option<isize>,
) -> Result<usize, vm::RuntimeError> {
    let index = index.unwrap_or(0);

    Ok(if index < 0 {
        array_len
            .checked_add_signed(index)
            .ok_or(ArrayIndexError { index, array_len })?
    } else {
        index as usize
    })
}

pub fn resolve_array_range(
    array_len: usize,
    index: Option<isize>,
    count: Option<isize>,
) -> Result<(Range<usize>, bool), vm::RuntimeError> {
    let abs_index = resolve_array_index(array_len, index)?;

    let err = ArrayRangeError {
        index: index.unwrap_or(0),
        count: count.unwrap_or((array_len - abs_index) as isize),
        array_len,
    };

    let count = count.unwrap_or((array_len - abs_index) as isize);

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
        return Err(err.into());
    }

    Ok((range, is_reverse))
}
