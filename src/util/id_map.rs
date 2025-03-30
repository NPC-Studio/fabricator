use std::{
    mem::{self, ManuallyDrop},
    num::NonZero,
};

pub type Index = u32;
pub type Generation = u32;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Id {
    index: Index,
    generation: NonZero<Generation>,
}

impl Id {
    #[inline]
    pub fn index(&self) -> Index {
        self.index
    }

    #[inline]
    pub fn generation(&self) -> NonZero<Generation> {
        self.generation
    }
}

/// A map with generational index keys.
///
/// Keys are of type `Id` and their indexes are small... no larger than the largest number of live
/// values in the `IdMap`.
///
/// Indexes are re-used from deleted entries, but the generated `Id` keys are still unique because
/// they are paired with a "generation" that increases when an index is re-used.
///
/// Extremely fast and space efficient, lookup is just calling `Vec::get` on the `Id`'s index and
/// then comparing the entry generation.
pub struct IdMap<V> {
    slots: Vec<Slot<V>>,
    next_free: Index,
    occupancy: Index,
}

union SlotUnion<T> {
    value: ManuallyDrop<T>,
    next_free: Index,
}

struct Slot<T> {
    u: SlotUnion<T>,
    generation: Generation,
}

impl<T> Slot<T> {
    fn is_vacant(&self) -> bool {
        // Even generations mean the slot is vacant, odd generations mean it is occupied.
        self.generation % 2 == 0
    }
}

impl<V> Default for IdMap<V> {
    fn default() -> Self {
        Self {
            slots: Vec::default(),
            next_free: 0,
            occupancy: 0,
        }
    }
}

impl<V> IdMap<V> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, value: V) -> Id {
        if let Some(slot) = self.slots.get_mut(self.next_free as usize) {
            assert!(slot.is_vacant(), "allocated slot in free list");

            let index = self.next_free;
            let generation = slot
                .generation
                .checked_add(1)
                .expect("too many generations");
            // SAFETY: We just asserted that the slot was vacant
            let next_free = unsafe { slot.u.next_free };
            let occupancy = self
                .occupancy
                .checked_add(1)
                .expect("occupancy count desync");

            slot.generation = generation;
            slot.u.value = ManuallyDrop::new(value);
            assert!(!slot.is_vacant());

            self.next_free = next_free;
            self.occupancy = occupancy;

            Id {
                index,
                generation: NonZero::new(generation).unwrap(),
            }
        } else {
            assert_eq!(self.next_free as usize, self.slots.len());

            let index: Index = self.next_free;
            let generation = NonZero::new(1).unwrap();
            let next_free = index.checked_add(1).expect("too many IdMap entries");
            let occupancy = self
                .occupancy
                .checked_add(1)
                .expect("occupancy count desync");

            let slot = Slot {
                u: SlotUnion {
                    value: ManuallyDrop::new(value),
                },
                generation: generation.get(),
            };
            assert!(!slot.is_vacant());
            self.slots.push(slot);

            self.next_free = next_free;
            self.occupancy = occupancy;

            Id { index, generation }
        }
    }

    pub fn remove(&mut self, id: Id) -> Option<V> {
        let slot = self.slots.get_mut(id.index as usize)?;
        if slot.is_vacant() {
            return None;
        }

        let generation = slot
            .generation
            .checked_add(1)
            .expect("too many generations");
        let occupancy = self
            .occupancy
            .checked_sub(1)
            .expect("occupancy count desync");

        // SAFETY: We return early if the slot is vacant
        let value = unsafe { ManuallyDrop::take(&mut slot.u.value) };

        slot.generation = generation;
        assert!(slot.is_vacant());

        slot.u.next_free = self.next_free;
        self.next_free = id.index;
        self.occupancy = occupancy;

        Some(value)
    }

    #[inline]
    pub fn contains(&self, id: Id) -> bool {
        if let Some(slot) = self.slots.get(id.index as usize) {
            slot.generation == id.generation.get()
        } else {
            false
        }
    }

    #[inline]
    pub fn get(&self, id: Id) -> Option<&V> {
        let slot = self.slots.get(id.index as usize)?;
        if slot.generation == id.generation.get() {
            assert!(!slot.is_vacant());
            // SAFETY: We just asserted that the slot was occupied
            Some(unsafe { &slot.u.value })
        } else {
            None
        }
    }

    #[inline]
    pub fn get_mut(&mut self, id: Id) -> Option<&mut V> {
        let slot = self.slots.get_mut(id.index as usize)?;
        if slot.generation == id.generation.get() {
            assert!(!slot.is_vacant());
            // SAFETY: We just asserted that the slot was occupied
            Some(unsafe { &mut slot.u.value })
        } else {
            None
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.occupancy as usize
    }

    /// All indexes from every `Id` produced by this `IdMap` will be less than the returned `Index`.
    #[inline]
    pub fn index_upper_bound(&self) -> Index {
        // The slots length always fits in `Index` (because `next_free` must always fit in `Index`)
        self.slots.len().try_into().unwrap()
    }

    pub fn iter(&self) -> impl Iterator<Item = (Id, &V)> + '_ {
        self.slots.iter().enumerate().flat_map(|(index, slot)| {
            let index: Index = index.try_into().unwrap();
            if slot.is_vacant() {
                None
            } else {
                let id = Id {
                    index,
                    // Occupied slots always have non-zero generation
                    generation: NonZero::new(slot.generation).unwrap(),
                };
                // SAFETY: We just checked that the slot was occupied
                let value = unsafe { &*slot.u.value };
                Some((id, value))
            }
        })
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (Id, &mut V)> + '_ {
        self.slots.iter_mut().enumerate().flat_map(|(index, slot)| {
            let index: Index = index.try_into().unwrap();
            if slot.is_vacant() {
                None
            } else {
                let id = Id {
                    index,
                    // Occupied slots always have non-zero generation
                    generation: NonZero::new(slot.generation).unwrap(),
                };
                // SAFETY: We just checked that the slot was occupied
                let value = unsafe { &mut *slot.u.value };
                Some((id, value))
            }
        })
    }

    pub fn ids(&self) -> impl Iterator<Item = Id> + '_ {
        self.iter().map(|(id, _)| id)
    }

    pub fn values(&self) -> impl Iterator<Item = &V> + '_ {
        self.iter().map(|(_, v)| v)
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> + '_ {
        self.iter_mut().map(|(_, v)| v)
    }
}

impl<V> Drop for Slot<V> {
    fn drop(&mut self) {
        if !self.is_vacant() {
            // SAFETY: We just checked that the slot is occupied
            unsafe { ManuallyDrop::drop(&mut self.u.value) };
        }
    }
}

/// An associated map for `Id` keys generated from an `IdMap`.
///
/// As fast and space efficient as `IdMap` because entry lookup works in the same way (`Vec::get`
/// and compare generation).
///
/// It does not generate its own `Id`s, it is designed to be used with `Id`s generated from a single
/// paired `IdMap` (multiple `SecondaryMap`s may be used with a single `IdMap`).
pub struct SecondaryMap<V> {
    slots: Vec<SecondarySlot<V>>,
    occupancy: usize,
}

enum SecondarySlot<V> {
    Occupied {
        value: V,
        generation: NonZero<Generation>,
    },
    Vacant,
}

impl<V> Default for SecondaryMap<V> {
    fn default() -> Self {
        Self {
            slots: Vec::new(),
            occupancy: 0,
        }
    }
}

impl<V> SecondaryMap<V> {
    pub fn new() -> Self {
        Default::default()
    }

    /// Clears all stored entries.
    ///
    /// After being cleared, the `SecondaryMap` can be safely used with an entirely new `IdMap`,
    /// since there is no longer the possibility of `Id` conflicts.
    pub fn clear(&mut self) {
        self.slots.clear();
        self.occupancy = 0;
    }

    /// Insert a value into this `SecondaryMap`.
    ///
    /// Only one value per `Id` index may be stored, if a value is replaced this will return the
    /// previous value along with its `Id`.
    ///
    /// No generation version checking is performed. Values are always replaced, even if the
    /// provided key generation is older than the existing generation.
    pub fn insert(&mut self, key: Id, val: V) -> Option<(Id, V)> {
        if self.slots.len() <= key.index() as usize {
            self.slots
                .resize_with(key.index() as usize + 1, || SecondarySlot::Vacant);
        }

        let slot = &mut self.slots[key.index() as usize];

        match slot {
            SecondarySlot::Occupied { value, generation } => Some((
                Id {
                    index: key.index(),
                    generation: *generation,
                },
                mem::replace(value, val),
            )),
            SecondarySlot::Vacant => {
                *slot = SecondarySlot::Occupied {
                    value: val,
                    generation: key.generation(),
                };
                None
            }
        }
    }

    pub fn remove(&mut self, key: Id) -> Option<V> {
        let Some(slot) = self.slots.get_mut(key.index() as usize) else {
            return None;
        };

        match slot {
            SecondarySlot::Occupied { generation, .. } if *generation == key.generation() => {
                let SecondarySlot::Occupied { value, .. } =
                    mem::replace(slot, SecondarySlot::Vacant)
                else {
                    unreachable!()
                };
                Some(value)
            }
            _ => None,
        }
    }

    pub fn get(&self, key: Id) -> Option<&V> {
        match self.slots.get(key.index() as usize) {
            Some(SecondarySlot::Occupied { value, generation })
                if *generation == key.generation() =>
            {
                Some(value)
            }
            _ => None,
        }
    }

    pub fn get_mut(&mut self, key: Id) -> Option<&mut V> {
        match self.slots.get_mut(key.index() as usize) {
            Some(SecondarySlot::Occupied { value, generation })
                if *generation == key.generation() =>
            {
                Some(value)
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn id_map_insert_remove() {
        let mut map = IdMap::new();

        let i1 = map.insert(1);
        assert_eq!(i1.generation.get(), 1);

        let i2 = map.insert(2);
        assert_eq!(i2.generation.get(), 1);

        assert!(map.contains(i1));
        assert!(map.contains(i2));

        assert_eq!(map.get(i1), Some(&1));
        assert_eq!(map.get(i2), Some(&2));

        assert_eq!(map.remove(i1), Some(1));
        assert_eq!(map.remove(i2), Some(2));

        assert!(!map.contains(i1));
        assert!(!map.contains(i2));

        let i3 = map.insert(3);
        assert_eq!(i3.generation.get(), 3);

        let i4 = map.insert(4);
        assert_eq!(i4.generation.get(), 3);

        assert!(map.contains(i3));
        assert!(map.contains(i4));

        assert!(!map.contains(i1));
        assert!(!map.contains(i2));

        assert!(map.get(i1).is_none());
        assert!(map.get(i2).is_none());

        assert_eq!(map.get(i3), Some(&3));
        assert_eq!(map.get(i4), Some(&4));
    }

    #[test]
    fn id_map_secondary_map() {
        let mut map = IdMap::new();

        let i1 = map.insert(1);
        let i2 = map.insert(2);

        let mut secondary = SecondaryMap::new();

        assert!(secondary.insert(i1, 11).is_none());
        assert!(secondary.insert(i2, 22).is_none());

        assert_eq!(secondary.get(i1), Some(&11));
        assert_eq!(secondary.get(i2), Some(&22));

        map.remove(i1);
        let i3 = map.insert(3);

        // Index should be re-used
        assert_eq!(i1.index(), i3.index());

        // Inserting `i3` should overwrite the entry for `i1`
        assert_eq!(secondary.insert(i3, 33), Some((i1, 11)));
    }
}
