use std::{array, ops};

use crate::{
    cast,
    vector::{Vec2, Vec3, Vector},
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Matrix<T, const R: usize, const C: usize>([Vector<T, R>; C]);

pub type Mat2<T> = Matrix<T, 2, 2>;
pub type Mat3<T> = Matrix<T, 3, 3>;

impl<T, const R: usize, const C: usize> From<[Vector<T, R>; C]> for Matrix<T, R, C> {
    fn from(a: [Vector<T, R>; C]) -> Self {
        Self(a)
    }
}

impl<T, const R: usize, const C: usize> From<[[T; R]; C]> for Matrix<T, R, C> {
    fn from(a: [[T; R]; C]) -> Self {
        Self::from_array(a)
    }
}

impl<T, I, const R: usize, const C: usize> ops::Index<I> for Matrix<T, R, C>
where
    [Vector<T, R>; C]: ops::Index<I>,
{
    type Output = <[Vector<T, R>; C] as ops::Index<I>>::Output;

    fn index(&self, index: I) -> &Self::Output {
        &self.0[index]
    }
}

impl<T, I, const R: usize, const C: usize> ops::IndexMut<I> for Matrix<T, R, C>
where
    [Vector<T, R>; C]: ops::IndexMut<I>,
{
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.0[index]
    }
}

impl<T, const R: usize, const C: usize> ops::Mul<Vector<T, C>> for Matrix<T, R, C>
where
    T: ops::Add<Output = T> + ops::Mul<Output = T> + Copy,
{
    type Output = Vector<T, R>;

    fn mul(self, rhs: Vector<T, C>) -> Vector<T, R> {
        Vector::from_fn(|r| {
            (0..C)
                .map(|c| self[c][r].mul(rhs[c]))
                .reduce(|a, b| a.add(b))
                .unwrap()
        })
    }
}

impl<T, const R: usize, const M: usize, const C: usize> ops::Mul<Matrix<T, M, C>>
    for Matrix<T, R, M>
where
    T: ops::Add<Output = T> + ops::Mul<Output = T> + Copy,
{
    type Output = Matrix<T, R, C>;

    fn mul(self, rhs: Matrix<T, M, C>) -> Self::Output {
        Matrix(array::from_fn(|c| self * rhs[c]))
    }
}

impl<T, const R: usize, const C: usize> Matrix<T, R, C> {
    pub const fn from_cols(cols: [Vector<T, R>; C]) -> Self {
        Self(cols)
    }

    pub fn into_cols(self) -> [Vector<T, R>; C] {
        self.0
    }

    pub fn from_array(arr: [[T; R]; C]) -> Self {
        Self(arr.map(Vector::from_array))
    }

    pub fn into_array(self) -> [[T; R]; C] {
        self.0.map(|v| v.into_array())
    }

    pub fn to_array(&self) -> [[T; R]; C]
    where
        T: Copy,
    {
        self.into_array()
    }

    pub fn from_fn(f: impl Fn(usize, usize) -> T) -> Self {
        Self::from_array(array::from_fn(|c| array::from_fn(|r| f(c, r))))
    }

    pub fn map<U>(self, f: impl Fn(T) -> U) -> Matrix<U, R, C> {
        Matrix(self.0.map(|c| c.map(&f)))
    }

    pub fn try_map<U>(self, f: impl Fn(T) -> Option<U>) -> Option<Matrix<U, R, C>> {
        let a: [Option<Vector<U, R>>; C] = self.0.map(|v| v.try_map(&f));
        if a.iter().any(Option::is_none) {
            None
        } else {
            Some(Matrix(a.map(Option::unwrap)))
        }
    }

    pub fn transpose(self) -> Matrix<T, C, R> {
        let mut m = self.map(|v| Some(v));
        let mut n = Matrix::from_fn(|_, _| None);
        for c in 0..C {
            for r in 0..R {
                n[r][c] = m[c][r].take()
            }
        }
        n.map(|v| v.unwrap())
    }
}

impl<T: num::NumCast, const R: usize, const C: usize> Matrix<T, R, C> {
    pub fn cast<U: num::NumCast>(self) -> Matrix<T, R, C> {
        self.map(cast::cast)
    }

    pub fn try_cast<U: num::NumCast>(self) -> Option<Matrix<U, R, C>> {
        self.try_map(cast::try_cast)
    }
}

impl<T: num::Zero, const N: usize> Matrix<T, N, N> {
    pub fn from_diagonal(diagonal: Vector<T, N>) -> Self {
        let mut m = Matrix::from_fn(|_, _| T::zero());
        for (i, s) in diagonal.into_array().into_iter().enumerate() {
            m[i][i] = s;
        }
        m
    }
}

impl<T: num::Zero + num::One, const N: usize> Matrix<T, N, N> {
    pub fn identity() -> Self {
        Self::from_diagonal(Vector::one())
    }
}

impl<T: num::Float> Mat2<T> {
    pub fn determinant(&self) -> T {
        self[0][0] * self[1][1] - self[0][1] * self[1][0]
    }

    pub fn inverse(&self) -> Self {
        let inv_determinant = self.determinant().recip();
        Self::from_cols([
            Vec2::new(self[1][1], -self[0][1]) * inv_determinant,
            Vec2::new(-self[1][0], self[0][0]) * inv_determinant,
        ])
    }
}

impl<T: num::Float> Mat3<T> {
    pub fn determinant(&self) -> T {
        self[2].dot(self[0].cross(self[1]))
    }

    pub fn inverse(&self) -> Self {
        let a = self[1].cross(self[2]);
        let b = self[2].cross(self[0]);
        let c = self[0].cross(self[1]);
        let det = self[2].dot(c);
        let inv_det = Vec3::splat(det.recip());
        Self::from_cols([a * inv_det, b * inv_det, c * inv_det]).transpose()
    }
}
