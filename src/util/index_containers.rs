use std::{mem, ops};

use bit_vec::BitVec;

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

    pub fn get(&self, i: usize) -> Option<&V> {
        self.vec.get(i)?.as_ref()
    }

    pub fn get_mut(&mut self, i: usize) -> Option<&mut V> {
        self.vec.get_mut(i)?.as_mut()
    }

    pub fn contains(&self, i: usize) -> bool {
        self.get(i).is_some()
    }

    pub fn insert(&mut self, i: usize, v: V) -> Option<V> {
        if i >= self.vec.len() {
            self.vec.resize_with(i.checked_add(1).unwrap(), || None);
        }
        mem::replace(&mut self.vec[i], Some(v))
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
}

impl<V> ops::Index<usize> for IndexMap<V> {
    type Output = V;

    fn index(&self, index: usize) -> &V {
        self.get(index).expect("no such index in `IndexMap`")
    }
}

#[derive(Debug, Clone, Default)]
pub struct IndexSet {
    vec: BitVec,
    len: usize,
}

impl IndexSet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn contains(&self, i: usize) -> bool {
        self.vec.get(i).unwrap_or(false)
    }

    pub fn insert(&mut self, i: usize) -> bool {
        self.set(i, true)
    }

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
            self.vec
                .grow(i.checked_add(1).unwrap() - self.vec.len(), false);
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
