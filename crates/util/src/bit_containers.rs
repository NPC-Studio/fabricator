use std::ops;

pub trait BitNum {
    const BITS: usize;

    fn get_bit(&self, i: usize) -> bool;
    fn set_bit(&mut self, i: usize, val: bool);
}

impl BitNum for u8 {
    const BITS: usize = 8;

    fn get_bit(&self, i: usize) -> bool {
        (*self) & (1 << i) != 0
    }

    fn set_bit(&mut self, i: usize, val: bool) {
        if val {
            *self |= 1 << i;
        } else {
            *self &= !(1 << i);
        }
    }
}

pub trait BitSlice {
    fn bit_len(&self) -> usize;
    fn get_bit(&self, i: usize) -> bool;
    fn set_bit(&mut self, i: usize, val: bool);
}

impl<B: BitNum> BitSlice for [B] {
    #[inline]
    fn bit_len(&self) -> usize {
        self.len()
            .checked_mul(B::BITS)
            .expect("overflow in BitSlice::bit_len")
    }

    #[inline]
    fn get_bit(&self, i: usize) -> bool {
        let base = i / 8;
        let off = i % 8;
        self[base].get_bit(off)
    }

    #[inline]
    fn set_bit(&mut self, i: usize, val: bool) {
        let base = i / 8;
        let off = i % 8;
        self[base].set_bit(off, val);
    }
}

#[derive(Debug, Clone)]
pub struct BitVec {
    bits: Vec<u8>,
    len: usize,
}

impl Default for BitVec {
    fn default() -> Self {
        Self {
            bits: Vec::new(),
            len: 0,
        }
    }
}

impl BitVec {
    pub fn new() -> Self {
        Self::default()
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
    pub fn clear(&mut self) {
        self.bits.clear();
        self.len = 0;
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
    pub fn get(&self, i: usize) -> Option<bool> {
        if i < self.len() {
            Some(self.bits.as_slice().get_bit(i))
        } else {
            None
        }
    }

    #[inline]
    pub fn set(&mut self, i: usize, val: bool) {
        assert!(i < self.len, "index out of range");
        self.bits.as_mut_slice().set_bit(i, val);
    }
}

impl ops::Index<usize> for BitVec {
    type Output = bool;

    #[inline]
    fn index(&self, index: usize) -> &Self::Output {
        if self.get(index).expect("index out of range") {
            &true
        } else {
            &false
        }
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

        assert!(bv[0]);
        assert!(bv[7]);
        assert!(bv[16]);
        assert!(!bv[1]);
        assert!(!bv[6]);
        assert!(!bv[8]);
        assert!(!bv[15]);
    }

    #[test]
    fn test_bit_vec_resize() {
        let mut bv = BitVec::new();
        bv.resize(9, false);

        bv.set(1, true);
        bv.set(7, true);

        assert!(!bv[0]);
        assert!(bv[1]);
        assert!(bv[7]);
        assert!(!bv[8]);

        bv.resize(17, true);

        assert!(bv[7]);
        assert!(!bv[8]);
        assert!(bv[9]);
        assert!(bv[10]);
        assert!(bv[15]);
        assert!(bv[16]);

        let mut bv = BitVec::new();
        bv.resize(9, false);

        bv.set(0, true);
        bv.set(8, true);

        assert!(bv[0]);
        assert!(!bv[1]);
        assert!(!bv[7]);
        assert!(bv[8]);

        bv.resize(17, false);

        assert!(!bv[7]);
        assert!(bv[8]);
        assert!(!bv[9]);
        assert!(!bv[10]);
        assert!(!bv[15]);
        assert!(!bv[16]);
    }
}
