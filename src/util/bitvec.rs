#[derive(Debug)]
pub struct BitVec {
    bits: Vec<u8>,
    len: usize,
}

impl BitVec {
    pub fn new() -> Self {
        Self {
            bits: Vec::new(),
            len: 0,
        }
    }

    #[inline]
    pub fn resize(&mut self, len: usize, value: bool) {
        let oldlen = self.len;

        self.bits
            .resize(len.div_ceil(8), if value { 255 } else { 0 });
        self.len = len;

        // Set the remaining bits in the previously last byte, if there are any.
        for i in oldlen..oldlen.checked_next_multiple_of(8).unwrap_or(len).min(len) {
            self.set(i, value);
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub fn set(&mut self, i: usize, val: bool) {
        assert!(i < self.len, "index out of range");

        let base = i / 8;
        let off = i % 8;

        // SAFETY: We know that self.len is less than or equal to the total number of bits we can
        // store, so we can avoid the extra bounds check.
        let byte = unsafe { self.bits.get_unchecked_mut(base) };

        if val {
            *byte |= 1 << off;
        } else {
            *byte &= !(1 << off);
        }
    }

    #[inline]
    pub fn get(&self, i: usize) -> bool {
        let base = i / 8;
        let off = i % 8;

        // SAFETY: We know that self.len is less than or equal to the total number of bits we can
        // store, so we can avoid the extra bounds check.
        let byte = unsafe { self.bits.get_unchecked(base) };

        *byte & (1 << off) != 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bit_vec() {
        let mut bv = BitVec::new();
        bv.resize(17, false);

        bv.set(0, true);
        bv.set(7, true);
        bv.set(16, true);

        assert!(bv.get(0));
        assert!(bv.get(7));
        assert!(bv.get(16));
        assert!(!bv.get(1));
        assert!(!bv.get(6));
        assert!(!bv.get(8));
        assert!(!bv.get(15));
    }

    #[test]
    fn test_bit_vec_resize() {
        let mut bv = BitVec::new();
        bv.resize(9, false);

        bv.set(1, true);
        bv.set(7, true);

        assert!(!bv.get(0));
        assert!(bv.get(1));
        assert!(bv.get(7));
        assert!(!bv.get(8));

        bv.resize(17, true);

        assert!(bv.get(7));
        assert!(!bv.get(8));
        assert!(bv.get(9));
        assert!(bv.get(10));
        assert!(bv.get(15));
        assert!(bv.get(16));

        let mut bv = BitVec::new();
        bv.resize(9, false);

        bv.set(0, true);
        bv.set(8, true);

        assert!(bv.get(0));
        assert!(!bv.get(1));
        assert!(!bv.get(7));
        assert!(bv.get(8));

        bv.resize(17, false);

        assert!(!bv.get(7));
        assert!(bv.get(8));
        assert!(!bv.get(9));
        assert!(!bv.get(10));
        assert!(!bv.get(15));
        assert!(!bv.get(16));
    }
}
