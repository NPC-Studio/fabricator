use std::{
    ops::{self, Bound, RangeBounds},
    vec,
};

/// A mutable reference to only the *end* of some `Vec<T>`.
///
/// Preserves all values below the `bottom` of the slice. Users are allowed to grow and shrink the
/// end of the `Vec` as long as all values below `bottom` are preserved.
///
/// This can be used to provide a series of nested stacks while avoiding a separate allocation for
/// each stack.
#[derive(Debug)]
pub struct VecEndSlice<'a, T> {
    values: &'a mut Vec<T>,
    bottom: usize,
}

impl<'a, T> VecEndSlice<'a, T> {
    #[inline]
    pub fn new(values: &'a mut Vec<T>, bottom: usize) -> Self {
        assert!(
            values.len() >= bottom,
            "slice bottom {bottom} is greater than vec len {}",
            values.len()
        );
        Self { values, bottom }
    }

    #[inline]
    pub fn reborrow(&mut self) -> VecEndSlice<'_, T> {
        self.sub_slice(0)
    }

    #[inline]
    pub fn sub_slice(&mut self, bottom: usize) -> VecEndSlice<'_, T> {
        assert!(
            self.values.len() - self.bottom >= bottom,
            "sub-slice bottom {bottom} is greater than slice len {}",
            self.values.len() - self.bottom,
        );
        VecEndSlice {
            values: self.values,
            bottom: self.bottom + bottom,
        }
    }

    #[inline]
    pub fn push_back(&mut self, value: T) {
        self.values.push(value);
    }

    #[inline]
    pub fn pop_back(&mut self) -> Option<T> {
        if self.values.len() > self.bottom {
            Some(self.values.pop().unwrap())
        } else {
            None
        }
    }

    #[inline]
    pub fn clear(&mut self) {
        self.values.truncate(self.bottom);
    }

    #[inline]
    pub fn resize(&mut self, size: usize, value: T)
    where
        T: Clone,
    {
        self.values.resize(self.bottom + size, value);
    }

    #[inline]
    pub fn truncate(&mut self, size: usize) {
        self.values.truncate(self.bottom + size);
    }

    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        self.values.reserve(additional);
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.values.capacity() - self.bottom
    }

    #[inline]
    pub fn remove(&mut self, index: usize) -> T {
        self.values.remove(self.bottom + index)
    }

    #[inline]
    pub fn drain<R: RangeBounds<usize>>(&mut self, range: R) -> vec::Drain<'_, T> {
        let start = match range.start_bound().cloned() {
            Bound::Included(r) => Bound::Included(self.bottom + r),
            Bound::Excluded(r) => Bound::Excluded(self.bottom + r),
            Bound::Unbounded => Bound::Included(self.bottom),
        };
        let end = match range.end_bound().cloned() {
            Bound::Included(r) => Bound::Included(self.bottom + r),
            Bound::Excluded(r) => Bound::Excluded(self.bottom + r),
            Bound::Unbounded => Bound::Unbounded,
        };
        self.values.drain((start, end))
    }
}

impl<'a, T> ops::Deref for VecEndSlice<'a, T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.values[self.bottom..]
    }
}

impl<'a, T> ops::DerefMut for VecEndSlice<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.values[self.bottom..]
    }
}

impl<'a, T> Extend<T> for VecEndSlice<'a, T> {
    #[inline]
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.values.extend(iter);
    }
}

impl<'a, T: Copy> Extend<&'a T> for VecEndSlice<'a, T> {
    #[inline]
    fn extend<I: IntoIterator<Item = &'a T>>(&mut self, iter: I) {
        self.values.extend(iter);
    }
}
