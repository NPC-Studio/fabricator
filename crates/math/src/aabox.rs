use std::{array, ops};

use crate::vector::Vector;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct AABox<T, const N: usize> {
    pub min: Vector<T, N>,
    pub max: Vector<T, N>,
}

pub type Box2<T> = AABox<T, 2>;
pub type Box3<T> = AABox<T, 3>;

impl<T, const N: usize> AABox<T, N> {
    pub const fn new(min: Vector<T, N>, max: Vector<T, N>) -> Self {
        Self { min, max }
    }
}

impl<T: num::Zero, const N: usize> AABox<T, N> {
    pub fn zero() -> Self {
        Self::new(Vector::zero(), Vector::zero())
    }
}

impl<T: Copy, const N: usize> AABox<T, N> {
    pub fn corners(self) -> impl Iterator<Item = Vector<T, N>> {
        AABox::<u8, N>::new(Vector::splat(0), Vector::splat(2))
            .iter(array::from_fn(|i| i))
            .map(move |sides: [u8; N]| {
                Vector::from_fn(|i| {
                    let side = sides[i];
                    debug_assert!(side == 0 || side == 1);
                    if side == 0 { self.min[i] } else { self.max[i] }
                })
            })
    }
}

impl<T, const N: usize> AABox<T, N>
where
    T: ops::Add<Output = T> + Copy,
{
    pub fn with_size(min: Vector<T, N>, size: Vector<T, N>) -> Self {
        Self {
            min,
            max: min + size,
        }
    }

    pub fn translate(self, offset: Vector<T, N>) -> Self {
        Self {
            min: self.min + offset,
            max: self.max + offset,
        }
    }
}

impl<T, const N: usize> AABox<T, N>
where
    T: PartialOrd,
{
    /// Points are considered "contained" by the `AABox` if, along every dimension, min <= c < max.
    pub fn contains(&self, p: Vector<T, N>) -> bool {
        for i in 0..N {
            if !(p[i] >= self.min[i] && p[i] < self.max[i]) {
                return false;
            }
        }
        true
    }

    /// The given `AABox` is considered "contained" if along every dimension, `other.min >= self.min
    /// && other.max <= self.max`.
    pub fn contains_box(&self, other: Self) -> bool {
        for i in 0..N {
            if !(self.min[i] <= other.min[i] && self.max[i] >= other.max[i]) {
                return false;
            }
        }
        true
    }

    /// Two `AABox`es are considered intersecting if there is any point which is contained in both
    /// boxes.
    pub fn intersects(&self, other: Self) -> bool {
        for i in 0..N {
            if !(self.min[i] < other.max[i]) || !(self.max[i] > other.min[i]) {
                return false;
            }
        }
        true
    }

    /// Returns true if along any axis, the minimum value for this axis is not strictly less than
    /// the maximum value.
    pub fn is_empty(&self) -> bool {
        for i in 0..N {
            if !(self.min[i] < self.max[i]) {
                return true;
            }
        }
        false
    }
}

impl<T, const N: usize> AABox<T, N>
where
    T: PartialOrd + Copy,
{
    pub fn constrain(&self, p: Vector<T, N>) -> Vector<T, N> {
        p.zip(self.min, |a, b| if a < b { b } else { a })
            .zip(self.max, |a, b| if a > b { b } else { a })
    }
}

impl<T, const N: usize> AABox<T, N>
where
    T: Ord,
{
    pub fn union(self, other: Self) -> Self {
        let min = self.min.zip(other.min, |a, b| a.min(b));
        let max = self.max.zip(other.max, |a, b| a.max(b));
        Self { min, max }
    }

    pub fn intersection(self, other: Self) -> Self {
        let min = self.min.zip(other.min, |a, b| a.max(b));
        let max = self.max.zip(other.max, |a, b| a.min(b));
        Self { min, max }
    }
}

impl<T, const N: usize> AABox<T, N>
where
    T: Ord + Copy,
{
    pub fn from_points(mut i: impl Iterator<Item = Vector<T, N>>) -> Option<Self> {
        let first = i.next()?;
        Some(i.fold(Self::new(first, first), |a, b| a.union(Self::new(b, b))))
    }
}

impl<T, const N: usize> AABox<T, N>
where
    T: num::Float,
{
    pub fn funion(self, other: Self) -> Self {
        let min = self.min.zip(other.min, |a, b| a.min(b));
        let max = self.max.zip(other.max, |a, b| a.max(b));
        Self { min, max }
    }

    pub fn fintersection(self, other: Self) -> Self {
        let min = self.min.zip(other.min, |a, b| a.max(b));
        let max = self.max.zip(other.max, |a, b| a.min(b));
        Self { min, max }
    }

    pub fn from_fpoints(mut i: impl Iterator<Item = Vector<T, N>>) -> Option<Self> {
        let first = i.next()?;
        Some(i.fold(Self::new(first, first), |a, b| a.funion(Self::new(b, b))))
    }
}

impl<T, const N: usize> AABox<T, N>
where
    T: num::Integer + Copy,
{
    pub fn iter<const D: usize>(self, dims: [usize; D]) -> impl Iterator<Item = [T; D]> {
        struct Iter<T, const D: usize>
        where
            T: num::Integer + Copy,
        {
            min: [T; D],
            max: [T; D],
            next: [T; D],
        }

        impl<T, const D: usize> Iterator for Iter<T, D>
        where
            T: num::Integer + Copy,
        {
            type Item = [T; D];

            fn next(&mut self) -> Option<Self::Item> {
                if D == 0 || !(self.next[D - 1] < self.max[D - 1]) {
                    return None;
                }

                let current = self.next;
                self.next[0] = self.next[0] + T::one();
                for i in 1..D {
                    if self.next[i - 1] < self.max[i - 1] {
                        break;
                    } else {
                        self.next[i - 1] = self.min[i - 1];
                        self.next[i] = self.next[i] + T::one();
                    }
                }

                Some(current)
            }
        }

        let min = array::from_fn(|i| self.min[dims[i]]);
        let max = array::from_fn(|i| self.max[dims[i]]);
        Iter {
            min,
            max,
            next: min,
        }
    }

    /// Iterates over each point inside an integral AABox, the iteration order is that lower
    /// dimensions change more frequently than higher ones.
    pub fn iter_points(self) -> impl Iterator<Item = Vector<T, N>> {
        let mut indices: [usize; N] = array::from_fn(|i| i);
        indices.reverse();
        self.iter::<N>(indices).map(|mut a| {
            a.reverse();
            Vector::from_array(a)
        })
    }
}

impl<T: num::Float, const N: usize> AABox<T, N> {
    pub fn with_center(center: Vector<T, N>, size: Vector<T, N>) -> Self {
        let half_size = size / T::from(2).unwrap();
        Self {
            min: center - half_size,
            max: center + half_size,
        }
    }

    pub fn scale(mut self, scale: Vector<T, N>) -> Self {
        self.min = self.min * scale;
        self.max = self.max * scale;
        self
    }

    pub fn round_out(self) -> Self {
        Self {
            min: self.min.map(T::floor),
            max: self.max.map(T::ceil),
        }
    }

    pub fn center(&self) -> Vector<T, N> {
        let half = T::one() / T::from(2).unwrap();
        self.min * half + self.max * half
    }

    pub fn eval(self, axes: [T; N]) -> Vector<T, N> {
        Vector::from_fn(|i| self.min[i] * (T::one() - axes[i]) + self.max[i] * axes[i])
    }
}

impl<T, const N: usize> AABox<T, N>
where
    T: ops::Sub<Output = T>,
{
    pub fn size(self) -> Vector<T, N> {
        self.max - self.min
    }
}

impl<T, const N: usize> AABox<T, N>
where
    T: ops::Sub<Output = T> + Copy,
{
    pub fn dim_size(&self, i: usize) -> T {
        self.max[i] - self.min[i]
    }
}

impl<T: num::NumCast, const N: usize> AABox<T, N> {
    pub fn cast<U: num::NumCast>(self) -> AABox<U, N> {
        AABox {
            min: self.min.cast(),
            max: self.max.cast(),
        }
    }

    pub fn try_cast<U: num::NumCast>(self) -> Option<AABox<U, N>> {
        Some(AABox {
            min: self.min.try_cast()?,
            max: self.max.try_cast()?,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_aabox_iter() {
        let b = AABox::<i32, 3> {
            min: [1, 11, 21].into(),
            max: [3, 14, 25].into(),
        };

        assert_eq!(
            b.iter([0, 1]).collect::<Vec<_>>(),
            vec![[1, 11], [2, 11], [1, 12], [2, 12], [1, 13], [2, 13],],
        );

        assert_eq!(
            b.iter([0, 2]).collect::<Vec<_>>(),
            vec![
                [1, 21],
                [2, 21],
                [1, 22],
                [2, 22],
                [1, 23],
                [2, 23],
                [1, 24],
                [2, 24],
            ],
        );

        assert_eq!(b.iter([1]).collect::<Vec<_>>(), vec![[11], [12], [13]],);

        assert!(b.iter([]).next().is_none());

        assert_eq!(
            b.iter([2, 1, 0]).collect::<Vec<_>>(),
            vec![
                [21, 11, 1],
                [22, 11, 1],
                [23, 11, 1],
                [24, 11, 1],
                [21, 12, 1],
                [22, 12, 1],
                [23, 12, 1],
                [24, 12, 1],
                [21, 13, 1],
                [22, 13, 1],
                [23, 13, 1],
                [24, 13, 1],
                [21, 11, 2],
                [22, 11, 2],
                [23, 11, 2],
                [24, 11, 2],
                [21, 12, 2],
                [22, 12, 2],
                [23, 12, 2],
                [24, 12, 2],
                [21, 13, 2],
                [22, 13, 2],
                [23, 13, 2],
                [24, 13, 2],
            ],
        );
    }

    #[test]
    fn test_intersects() {
        assert!(
            AABox::<i32, 2>::new([1, 1].into(), [3, 3].into())
                .intersects(AABox::new([2, 2].into(), [4, 4].into()))
        );

        assert!(
            !AABox::<i32, 2>::new([1, 1].into(), [2, 2].into())
                .intersects(AABox::new([2, 2].into(), [4, 4].into()))
        );

        assert!(
            AABox::<i32, 2>::new([2, 2].into(), [3, 3].into())
                .intersects(AABox::new([1, 1].into(), [4, 4].into()))
        );

        assert!(
            AABox::<i32, 2>::new([1, 1].into(), [4, 4].into())
                .intersects(AABox::new([2, 2].into(), [3, 3].into()))
        );

        assert!(
            !AABox::<i32, 2>::new([1, 1].into(), [2, 3].into())
                .intersects(AABox::new([2, 2].into(), [4, 4].into()))
        );
    }

    #[test]
    fn test_intersection() {
        assert_eq!(
            AABox::<i32, 2>::new([1, 1].into(), [3, 3].into())
                .intersection(AABox::new([2, 2].into(), [4, 4].into())),
            AABox::<i32, 2>::new([2, 2].into(), [3, 3].into())
        );
    }

    #[test]
    fn test_union() {
        assert_eq!(
            AABox::<i32, 2>::new([1, 1].into(), [3, 3].into())
                .union(AABox::new([2, 2].into(), [4, 4].into())),
            AABox::<i32, 2>::new([1, 1].into(), [4, 4].into())
        );
    }

    #[test]
    fn test_is_empty() {
        assert!(!AABox::<i32, 2>::new([1, 1].into(), [3, 3].into()).is_empty());
        assert!(AABox::<i32, 2>::new([2, 2].into(), [2, 2].into()).is_empty());
        assert!(AABox::<i32, 2>::new([3, 3].into(), [1, 1].into()).is_empty());
        assert!(AABox::<i32, 2>::new([2, 2].into(), [2, 3].into()).is_empty());
    }

    #[test]
    fn test_corners() {
        assert_eq!(
            AABox::<i32, 3>::new([0, 1, 2].into(), [5, 6, 7].into())
                .corners()
                .collect::<Vec<_>>(),
            vec![
                [0, 1, 2].into(),
                [5, 1, 2].into(),
                [0, 6, 2].into(),
                [5, 6, 2].into(),
                [0, 1, 7].into(),
                [5, 1, 7].into(),
                [0, 6, 7].into(),
                [5, 6, 7].into(),
            ]
        );
    }

    #[test]
    fn test_center() {
        let center = AABox::<f32, 3>::new([0.0, 1.0, 2.0].into(), [2.0, 3.0, 4.0].into()).center();
        assert_eq!(center[0], 1.0);
        assert_eq!(center[1], 2.0);
        assert_eq!(center[2], 3.0);
    }
}
