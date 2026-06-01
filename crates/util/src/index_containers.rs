use std::{ops, slice};

use crate::bit_containers::BitSlice as _;

/// A map of `usize` to `V` designed for low-value `usize` indexes and backed by a `Vec<Option<V>>`.
#[derive(Debug, Clone)]
pub struct IndexMap<V> {
    vec: Vec<Option<V>>,
}

impl<V> Default for IndexMap<V> {
    fn default() -> Self {
        Self { vec: Vec::new() }
    }
}

impl<V> IndexMap<V> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn clear(&mut self) {
        self.vec.clear();
    }

    #[inline]
    pub fn contains(&self, i: usize) -> bool {
        self.get(i).is_some()
    }

    #[inline]
    pub fn get(&self, i: usize) -> Option<&V> {
        self.vec.get(i)?.as_ref()
    }

    #[inline]
    pub fn get_mut(&mut self, i: usize) -> Option<&mut V> {
        self.vec.get_mut(i)?.as_mut()
    }

    #[inline]
    pub fn get_mut_option(&mut self, i: usize) -> &mut Option<V> {
        if i >= self.vec.len() {
            self.vec.resize_with(i.checked_add(1).unwrap(), || None);
        }
        &mut self.vec[i]
    }

    #[inline]
    pub fn get_or_insert_with(&mut self, i: usize, f: impl FnOnce() -> V) -> &mut V {
        self.get_mut_option(i).get_or_insert_with(f)
    }

    #[inline]
    pub fn get_or_insert_default(&mut self, i: usize) -> &mut V
    where
        V: Default,
    {
        self.get_mut_option(i).get_or_insert_default()
    }

    #[inline]
    pub fn insert(&mut self, i: usize, v: V) -> Option<V> {
        self.get_mut_option(i).replace(v)
    }

    #[inline]
    pub fn remove(&mut self, i: usize) -> Option<V> {
        if let Some(v) = self.vec.get_mut(i) {
            v.take()
        } else {
            None
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (usize, &V)> + '_ {
        self.vec
            .iter()
            .enumerate()
            .filter_map(|(i, v)| Some((i, v.as_ref()?)))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (usize, &mut V)> + '_ {
        self.vec
            .iter_mut()
            .enumerate()
            .filter_map(|(i, v)| Some((i, v.as_mut()?)))
    }

    pub fn into_iter(self) -> impl Iterator<Item = (usize, V)> {
        self.vec
            .into_iter()
            .enumerate()
            .filter_map(|(i, v)| Some((i, v?)))
    }

    pub fn keys(&self) -> impl Iterator<Item = usize> + '_ {
        self.iter().map(|(i, _)| i)
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.iter().map(|(_, v)| v)
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.iter_mut().map(|(_, v)| v)
    }
}

impl<V> FromIterator<(usize, V)> for IndexMap<V> {
    fn from_iter<T: IntoIterator<Item = (usize, V)>>(iter: T) -> Self {
        let mut map = Self::new();
        for (i, v) in iter {
            map.insert(i, v);
        }
        map
    }
}

impl<V> ops::Index<usize> for IndexMap<V> {
    type Output = V;

    fn index(&self, index: usize) -> &V {
        self.get(index).expect("no such index in `IndexMap`")
    }
}

/// A `usize` set, designed for low-value `usize` indexes and backed by a bit vector.
#[derive(Debug, Clone, Default)]
pub struct IndexSet {
    bytes: Vec<u8>,
    len: usize,
}

impl IndexSet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn clear(&mut self) {
        self.bytes.clear();
        self.len = 0;
    }

    #[inline]
    pub fn contains(&self, i: usize) -> bool {
        if i < self.bytes.bit_len() {
            self.bytes.get_bit(i)
        } else {
            false
        }
    }

    /// Returns `true` if the index `i` is newly inserted, false otherwise.
    #[inline]
    pub fn insert(&mut self, i: usize) -> bool {
        !self.set(i, true)
    }

    /// Removes the index `i` from the set, returns true if `i` was present in the set.
    #[inline]
    pub fn remove(&mut self, i: usize) -> bool {
        self.set(i, false)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    #[inline]
    fn set(&mut self, i: usize, val: bool) -> bool {
        if i >= self.bytes.bit_len() {
            if val {
                self.bytes.resize((i / 8) + 1, 0);
            } else {
                return false;
            }
        }

        let old = self.bytes.get_bit(i);
        self.bytes.set_bit(i, val);

        if !old && val {
            self.len += 1;
        } else if old && !val {
            self.len -= 1;
        }

        old
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = usize> + '_ {
        struct ByteIter {
            base: usize,
            byte: u8,
            i: u8,
        }

        impl Iterator for ByteIter {
            type Item = usize;

            fn next(&mut self) -> Option<Self::Item> {
                let byte = slice::from_mut(&mut self.byte);
                while self.i < 8 {
                    let i = self.i;
                    self.i += 1;

                    if byte.get_bit(i as usize) {
                        byte.set_bit(i as usize, false);
                        return Some(self.base + i as usize);
                    }
                }

                None
            }
        }

        self.bytes
            .as_slice()
            .iter()
            .copied()
            .enumerate()
            .flat_map(|(i, b)| ByteIter {
                base: i * 8,
                byte: b,
                i: 0,
            })
    }
}

impl FromIterator<usize> for IndexSet {
    fn from_iter<T: IntoIterator<Item = usize>>(iter: T) -> Self {
        let mut set = Self::new();
        for i in iter {
            set.insert(i);
        }
        set
    }
}

impl ops::Index<usize> for IndexSet {
    type Output = bool;

    fn index(&self, index: usize) -> &bool {
        if self.contains(index) { &true } else { &false }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_index_set_iter() {
        let mut iset = IndexSet::new();

        assert!(iset.iter().next().is_none());

        assert!(iset.insert(0));
        assert_eq!(iset.iter().collect::<Vec<_>>(), [0]);

        assert!(iset.insert(7));
        assert_eq!(iset.iter().collect::<Vec<_>>(), [0, 7]);

        assert!(iset.insert(8));
        assert_eq!(iset.iter().collect::<Vec<_>>(), [0, 7, 8]);

        assert!(iset.insert(14));
        assert_eq!(iset.iter().collect::<Vec<_>>(), [0, 7, 8, 14]);

        assert!(!iset.insert(14));
        assert_eq!(iset.iter().collect::<Vec<_>>(), [0, 7, 8, 14]);

        assert!(iset.insert(15));
        assert_eq!(iset.iter().collect::<Vec<_>>(), [0, 7, 8, 14, 15]);

        assert!(iset.insert(16));
        assert_eq!(iset.iter().collect::<Vec<_>>(), [0, 7, 8, 14, 15, 16]);

        assert!(iset.remove(7));
        assert_eq!(iset.iter().collect::<Vec<_>>(), [0, 8, 14, 15, 16]);

        assert!(iset.remove(8));
        assert_eq!(iset.iter().collect::<Vec<_>>(), [0, 14, 15, 16]);

        assert!(iset.remove(15));
        assert_eq!(iset.iter().collect::<Vec<_>>(), [0, 14, 16]);

        assert!(iset.remove(16));
        assert_eq!(iset.iter().collect::<Vec<_>>(), [0, 14]);

        assert!((iset.remove(0)));
        assert_eq!(iset.iter().collect::<Vec<_>>(), [14]);

        assert!(!iset.remove(24));
        assert_eq!(iset.iter().collect::<Vec<_>>(), [14]);
    }
}
