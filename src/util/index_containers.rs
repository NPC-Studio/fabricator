use std::{mem, ops};

use crate::util::bit_containers::BitVec;

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

    pub fn contains(&self, i: usize) -> bool {
        self.get(i).is_some()
    }

    pub fn get(&self, i: usize) -> Option<&V> {
        self.vec.get(i)?.as_ref()
    }

    pub fn get_mut(&mut self, i: usize) -> Option<&mut V> {
        self.vec.get_mut(i)?.as_mut()
    }

    pub fn get_mut_option(&mut self, i: usize) -> &mut Option<V> {
        if i >= self.vec.len() {
            self.vec.resize_with(i.checked_add(1).unwrap(), || None);
        }
        &mut self.vec[i]
    }

    pub fn insert(&mut self, i: usize, v: V) -> Option<V> {
        mem::replace(&mut self.get_mut_option(i), Some(v))
    }

    pub fn remove(&mut self, i: usize) -> Option<V> {
        if let Some(v) = self.vec.get_mut(i) {
            mem::replace(v, None)
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

/// A `usize` set, designed for low-value `usize` indexes and backed by a `BitVec`.
#[derive(Debug, Clone, Default)]
pub struct IndexSet {
    vec: BitVec,
    len: usize,
}

impl IndexSet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn clear(&mut self) {
        self.vec.clear();
        self.len = 0;
    }

    pub fn contains(&self, i: usize) -> bool {
        self.vec.get(i).unwrap_or(false)
    }

    /// Returns `true` if the index `i` is newly inserted, false otherwise.
    pub fn insert(&mut self, i: usize) -> bool {
        !self.set(i, true)
    }

    /// Removes the index `i` from the set, returns true if `i` was present in the set.
    pub fn remove(&mut self, i: usize) -> bool {
        self.set(i, false)
    }

    pub fn iter(&self) -> impl Iterator<Item = usize> + '_ {
        self.vec
            .iter()
            .enumerate()
            .filter_map(|(i, b)| if b { Some(i) } else { None })
    }

    pub fn len(&self) -> usize {
        self.len
    }

    fn set(&mut self, i: usize, val: bool) -> bool {
        if i >= self.vec.len() {
            self.vec.resize(i.checked_add(1).unwrap(), false);
        }

        let old = self.vec[i];
        self.vec.set(i, val);

        if !old && val {
            self.len += 1;
        } else if old && !val {
            self.len -= 1;
        }

        old
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
        if self.contains(index) {
            &true
        } else {
            &false
        }
    }
}
