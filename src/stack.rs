use std::{
    iter,
    ops::{Bound, Index, IndexMut, RangeBounds},
    slice::{self, SliceIndex},
    vec,
};

use crate::value::Value;

pub struct Stack<'gc, 'a> {
    values: &'a mut Vec<Value<'gc>>,
    bottom: usize,
}

impl<'gc, 'a> Stack<'gc, 'a> {
    pub fn new(values: &'a mut Vec<Value<'gc>>, bottom: usize) -> Self {
        assert!(values.len() >= bottom);
        Self { values, bottom }
    }

    pub fn reborrow(&mut self) -> Stack<'gc, '_> {
        self.sub_stack(0)
    }

    pub fn sub_stack(&mut self, bottom: usize) -> Stack<'gc, '_> {
        Stack {
            values: self.values,
            bottom: self.bottom + bottom,
        }
    }

    pub fn get(&self, i: usize) -> Value<'gc> {
        self.values
            .get(self.bottom + i)
            .copied()
            .unwrap_or_default()
    }

    pub fn push_back(&mut self, value: Value<'gc>) {
        self.values.push(value);
    }

    pub fn push_front(&mut self, value: Value<'gc>) {
        self.values.insert(self.bottom, value);
    }

    pub fn pop_back(&mut self) -> Option<Value<'gc>> {
        if self.values.len() > self.bottom {
            Some(self.values.pop().unwrap())
        } else {
            None
        }
    }

    pub fn pop_front(&mut self) -> Option<Value<'gc>> {
        if self.values.len() > self.bottom {
            Some(self.values.remove(self.bottom))
        } else {
            None
        }
    }

    pub fn remove(&mut self, i: usize) -> Option<Value<'gc>> {
        let index = self.bottom + i;
        if index < self.values.len() {
            Some(self.values.remove(index))
        } else {
            None
        }
    }

    pub fn len(&self) -> usize {
        self.values.len() - self.bottom
    }

    pub fn is_empty(&self) -> bool {
        self.values.len() == self.bottom
    }

    pub fn clear(&mut self) {
        self.values.truncate(self.bottom);
    }

    pub fn resize(&mut self, size: usize) {
        self.values.resize(self.bottom + size, Value::Undefined);
    }

    pub fn reserve(&mut self, additional: usize) {
        self.values.reserve(additional);
    }

    pub fn capacity(&self) -> usize {
        self.values.capacity() - self.bottom
    }

    pub fn drain<R: RangeBounds<usize>>(&mut self, range: R) -> vec::Drain<Value<'gc>> {
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

impl<'gc: 'b, 'a, 'b> IntoIterator for &'b Stack<'gc, 'a> {
    type Item = Value<'gc>;
    type IntoIter = iter::Copied<slice::Iter<'b, Value<'gc>>>;

    fn into_iter(self) -> Self::IntoIter {
        self.values[self.bottom..].iter().copied()
    }
}

impl<'gc, 'a> Extend<Value<'gc>> for Stack<'gc, 'a> {
    fn extend<T: IntoIterator<Item = Value<'gc>>>(&mut self, iter: T) {
        self.values.extend(iter);
    }
}

impl<'gc, 'a, 'b> Extend<Value<'gc>> for &'b mut Stack<'gc, 'a> {
    fn extend<T: IntoIterator<Item = Value<'gc>>>(&mut self, iter: T) {
        self.values.extend(iter);
    }
}

impl<'gc: 'b, 'a, 'b> Extend<&'a Value<'gc>> for Stack<'gc, 'a> {
    fn extend<T: IntoIterator<Item = &'a Value<'gc>>>(&mut self, iter: T) {
        self.values.extend(iter);
    }
}

impl<'gc: 'b, 'a, 'b, 'c> Extend<&'b Value<'gc>> for &'c mut Stack<'gc, 'a> {
    fn extend<T: IntoIterator<Item = &'b Value<'gc>>>(&mut self, iter: T) {
        self.values.extend(iter);
    }
}

impl<'gc, 'a, I: SliceIndex<[Value<'gc>]>> Index<I> for Stack<'gc, 'a> {
    type Output = <Vec<Value<'gc>> as Index<I>>::Output;

    fn index(&self, index: I) -> &Self::Output {
        &self.values[self.bottom..][index]
    }
}

impl<'gc, 'a, I: SliceIndex<[Value<'gc>]>> IndexMut<I> for Stack<'gc, 'a> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.values[self.bottom..][index]
    }
}
