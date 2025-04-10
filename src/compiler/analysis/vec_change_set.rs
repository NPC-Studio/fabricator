/// Apply a series of inserts and removes to a `Vec` as a batch.
///
/// Helps to prevent accidentally making an algorithm `O(N^2)`.
#[derive(Debug)]
pub struct VecChangeSet<T> {
    inserts: Vec<(usize, T)>,
    removes: Vec<usize>,
}

impl<T> Default for VecChangeSet<T> {
    fn default() -> Self {
        Self {
            inserts: Vec::default(),
            removes: Vec::default(),
        }
    }
}

impl<T> VecChangeSet<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn clear(&mut self) {
        self.inserts.clear();
        self.removes.clear();
    }

    /// Insert an element into the target vector at the given index.
    ///
    /// The `index` parameter is relative to the existing vector without any changes applied. It is
    /// allowed to be in the range `0..=target_vec.len()`. To append to the target vector, insert an
    /// element with the index set to the target vector's length.
    ///
    /// Inserting multiple elements at the same index will be applied in the order that they are
    /// inserted. For example, inserting the elements `1`, `2`, and `3` and index `0` will make the
    /// resulting vector start with `[1, 2, 3, ...]`.
    pub fn insert(&mut self, index: usize, element: T) {
        self.inserts.push((index, element));
    }

    /// Remove an element from the target vector.
    ///
    /// The `index` parameter is relative to the existing vector without any changes applied.
    ///
    /// It is not permitted to remove the same index more than once, and doing so will result in a
    /// panic during `Self::apply`.
    pub fn remove(&mut self, index: usize) {
        self.removes.push(index);
    }

    /// Apply all changes to the given vector.
    ///
    /// Implicitly clears the `IndexChangeSet`.
    ///
    /// # Panics
    ///
    /// Will panic if an index given to `Self::insert` is greater than the length of the target
    /// vector, or if `Self::remove` was called multiple times with the same index.
    pub fn apply(&mut self, vec: &mut Vec<T>) {
        self.inserts.sort_by_key(|(index, _)| *index);
        self.removes.sort_unstable();

        // NOTE: This could be done unsafely without allocating an extra Vec.

        let old_len = vec.len();
        let inserts_len = self.inserts.len();
        let removes_len = self.removes.len();

        // Create drain iterators so that in case of panic, everything is cleared.
        let mut old_items = vec.drain(..);
        let mut inserts = self.inserts.drain(..).peekable();
        let mut removes = self.removes.drain(..).peekable();

        let mut new_vec = Vec::new();
        new_vec.reserve(
            old_len
                .checked_sub(removes_len)
                .expect("index removed more than once")
                .checked_add(inserts_len)
                .unwrap(),
        );

        let mut offset = 0;
        let mut last_removed = None;

        while inserts.peek().is_some() || removes.peek().is_some() {
            if let Some((insert_index, element)) = inserts.next_if(|(insert_index, _)| {
                removes
                    .peek()
                    .is_none_or(|remove_index| *insert_index < *remove_index)
            }) {
                assert!(insert_index <= old_len);

                while new_vec.len() < insert_index + offset {
                    new_vec.push(old_items.next().unwrap());
                }
                new_vec.push(element);
                offset += 1;
            } else {
                let remove_index = removes.next().unwrap();
                assert!(remove_index < old_len);
                assert!(last_removed != Some(remove_index));

                while new_vec.len() < remove_index + offset {
                    new_vec.push(old_items.next().unwrap());
                }
                let _skipped = old_items.next().unwrap();
                offset -= 1;
                last_removed = Some(remove_index);
            }
        }

        new_vec.extend(&mut old_items);

        assert!(old_items.next().is_none());
        drop(old_items);

        *vec = new_vec;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vec_change_set_insert_append() {
        let mut array = Vec::new();

        array.push(1);
        array.push(3);
        array.push(6);

        let mut array_changes = VecChangeSet::new();

        array_changes.insert(0, 0);
        array_changes.insert(1, 2);
        array_changes.insert(2, 4);
        array_changes.insert(2, 5);
        array_changes.insert(3, 7);
        array_changes.insert(3, 8);
        array_changes.insert(3, 9);

        array_changes.apply(&mut array);

        for i in 0..10 {
            assert_eq!(array[i], i);
        }
    }

    #[test]
    fn test_vec_change_set_insert() {
        let mut v = vec![1, 2, 3];

        let mut change_set = VecChangeSet::new();

        change_set.insert(1, 4);
        change_set.insert(1, 5);
        change_set.insert(1, 6);

        change_set.apply(&mut v);

        assert_eq!(v, [1, 4, 5, 6, 2, 3]);
    }

    #[test]
    fn test_vec_change_set_insert_append_remove() {
        let mut array = Vec::new();

        array.push(1);
        array.push(2);
        array.push(3);
        array.push(6);
        array.push(7);
        array.push(8);

        let mut array_changes = VecChangeSet::new();

        array_changes.insert(0, 0);
        array_changes.remove(1);
        array_changes.insert(1, 2);
        array_changes.insert(3, 4);
        array_changes.insert(3, 5);
        array_changes.remove(4);
        array_changes.remove(5);
        array_changes.insert(4, 7);
        array_changes.insert(4, 8);
        array_changes.insert(5, 9);

        array_changes.apply(&mut array);

        for i in 0..10 {
            assert_eq!(array[i], i);
        }
    }

    #[test]
    #[should_panic]
    fn test_vec_change_set_remove_twice() {
        let mut array = Vec::new();

        array.push(1);
        array.push(2);

        let mut array_inserts = VecChangeSet::new();

        array_inserts.remove(0);
        array_inserts.remove(0);

        array_inserts.apply(&mut array);
    }
}
