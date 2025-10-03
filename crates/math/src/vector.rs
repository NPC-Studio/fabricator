use std::{array, ops, slice};

use crate::cast;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Vector<T, const N: usize>([T; N]);

pub type Vec2<T> = Vector<T, 2>;
pub type Vec3<T> = Vector<T, 3>;
pub type Vec4<T> = Vector<T, 4>;

impl<T: Default, const N: usize> Default for Vector<T, N> {
    fn default() -> Self {
        Self::from_fn(|_| Default::default())
    }
}

impl<T, const N: usize> From<[T; N]> for Vector<T, N> {
    fn from(a: [T; N]) -> Self {
        Self(a)
    }
}

impl<T, const N: usize> IntoIterator for Vector<T, N> {
    type Item = T;
    type IntoIter = array::IntoIter<T, N>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, T, const N: usize> IntoIterator for &'a Vector<T, N> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<T, I, const N: usize> ops::Index<I> for Vector<T, N>
where
    [T; N]: ops::Index<I>,
{
    type Output = <[T; N] as ops::Index<I>>::Output;

    fn index(&self, index: I) -> &Self::Output {
        &self.0[index]
    }
}

impl<T, I, const N: usize> ops::IndexMut<I> for Vector<T, N>
where
    [T; N]: ops::IndexMut<I>,
{
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.0[index]
    }
}

impl<T: ops::Neg, const N: usize> ops::Neg for Vector<T, N> {
    type Output = Vector<<T as ops::Neg>::Output, N>;

    fn neg(self) -> Self::Output {
        Vector(self.0.map(|v| -v))
    }
}

impl<T: ops::Add<T, Output = T>, const N: usize> ops::Add<Self> for Vector<T, N> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        self.zip(rhs, ops::Add::add)
    }
}

impl<T: ops::AddAssign<T>, const N: usize> ops::AddAssign<Self> for Vector<T, N> {
    fn add_assign(&mut self, rhs: Self) {
        for (i, r) in rhs.0.into_iter().enumerate() {
            self[i] += r;
        }
    }
}

impl<T: ops::Sub<T, Output = T>, const N: usize> ops::Sub<Self> for Vector<T, N> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        self.zip(rhs, ops::Sub::sub)
    }
}

impl<T: ops::SubAssign<T>, const N: usize> ops::SubAssign<Self> for Vector<T, N> {
    fn sub_assign(&mut self, rhs: Self) {
        for (i, r) in rhs.0.into_iter().enumerate() {
            self[i] -= r;
        }
    }
}

impl<T: ops::Mul<T, Output = T>, const N: usize> ops::Mul<Self> for Vector<T, N> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self {
        self.zip(rhs, ops::Mul::mul)
    }
}

impl<T: ops::MulAssign<T>, const N: usize> ops::MulAssign<Self> for Vector<T, N> {
    fn mul_assign(&mut self, rhs: Self) {
        for (i, r) in rhs.0.into_iter().enumerate() {
            self[i] *= r;
        }
    }
}

impl<T: ops::Mul<T, Output = T> + Copy, const N: usize> ops::Mul<T> for Vector<T, N> {
    type Output = Self;

    fn mul(self, rhs: T) -> Self {
        Self::from_fn(|i| self[i] * rhs)
    }
}

impl<T: ops::MulAssign<T> + Copy, const N: usize> ops::MulAssign<T> for Vector<T, N> {
    fn mul_assign(&mut self, rhs: T) {
        for i in 0..N {
            self[i] *= rhs;
        }
    }
}

impl<T: ops::Div<T, Output = T>, const N: usize> ops::Div<Self> for Vector<T, N> {
    type Output = Self;

    fn div(self, rhs: Self) -> Self {
        self.zip(rhs, ops::Div::div)
    }
}

impl<T: ops::DivAssign<T>, const N: usize> ops::DivAssign<Self> for Vector<T, N> {
    fn div_assign(&mut self, rhs: Self) {
        for (i, r) in rhs.0.into_iter().enumerate() {
            self[i] /= r;
        }
    }
}

impl<T: ops::Div<T, Output = T> + Copy, const N: usize> ops::Div<T> for Vector<T, N> {
    type Output = Self;

    fn div(self, rhs: T) -> Self {
        Self::from_fn(|i| self[i] / rhs)
    }
}

impl<T: ops::DivAssign<T> + Copy, const N: usize> ops::DivAssign<T> for Vector<T, N> {
    fn div_assign(&mut self, rhs: T) {
        for i in 0..N {
            self[i] /= rhs;
        }
    }
}

impl<T, const N: usize> Vector<T, N> {
    #[must_use]
    pub const fn from_array(arr: [T; N]) -> Self {
        Self(arr)
    }

    #[must_use]
    pub fn into_array(self) -> [T; N] {
        self.0
    }

    #[must_use]
    pub fn to_array(&self) -> [T; N]
    where
        T: Copy,
    {
        self.into_array()
    }

    #[must_use]
    pub const fn as_array(&self) -> &[T; N] {
        &self.0
    }

    pub fn iter(&self) -> slice::Iter<'_, T> {
        self.0.iter()
    }

    #[must_use]
    pub fn from_fn(f: impl Fn(usize) -> T) -> Self {
        Self(array::from_fn(f))
    }

    #[must_use]
    pub fn splat(t: T) -> Self
    where
        T: Copy,
    {
        Self::from_fn(|_| t)
    }

    #[must_use]
    pub fn map<U>(self, f: impl Fn(T) -> U) -> Vector<U, N> {
        Vector(self.0.map(f))
    }

    #[must_use]
    pub fn try_map<U>(self, f: impl Fn(T) -> Option<U>) -> Option<Vector<U, N>> {
        let a: [Option<U>; N] = self.0.map(f);
        if a.iter().any(Option::is_none) {
            None
        } else {
            Some(Vector(a.map(Option::unwrap)))
        }
    }

    #[must_use]
    pub fn zip<U, V>(self, other: Vector<U, N>, f: impl Fn(T, U) -> V) -> Vector<V, N> {
        let mut a: [Option<T>; N] = self.0.map(Some);
        let mut b: [Option<U>; N] = other.0.map(Some);
        let mut res: [Option<V>; N] = array::from_fn(|_| None);
        for i in 0..N {
            res[i] = Some(f(a[i].take().unwrap(), b[i].take().unwrap()));
        }

        Vector(res.map(|v| v.unwrap()))
    }

    #[must_use]
    pub fn try_zip<U, V>(
        self,
        other: Vector<U, N>,
        f: impl Fn(T, U) -> Option<V>,
    ) -> Option<Vector<V, N>> {
        let mut a: [Option<T>; N] = self.0.map(Some);
        let mut b: [Option<U>; N] = other.0.map(Some);
        let mut res: [Option<V>; N] = array::from_fn(|_| None);
        for i in 0..N {
            res[i] = Some(f(a[i].take().unwrap(), b[i].take().unwrap())?);
        }

        Some(Vector(res.map(|v| v.unwrap())))
    }
}

impl<T: num::Zero, const N: usize> Vector<T, N> {
    #[must_use]
    pub fn zero() -> Self {
        Self::from_fn(|_| num::zero())
    }
}

impl<T: num::One, const N: usize> Vector<T, N> {
    #[must_use]
    pub fn one() -> Self {
        Self::from_fn(|_| num::one())
    }
}

impl<T, const N: usize> Vector<T, N>
where
    T: num::Zero + ops::Mul<T, Output = T> + ops::Add<T, Output = T>,
{
    #[must_use]
    pub fn dot(self, rhs: Self) -> T {
        (self * rhs)
            .into_array()
            .into_iter()
            .fold(T::zero(), |a, b| a + b)
    }
}

impl<T: num::Float, const N: usize> Vector<T, N> {
    #[must_use]
    pub fn length_squared(self) -> T {
        self.dot(self)
    }

    #[must_use]
    pub fn length(self) -> T {
        self.length_squared().sqrt()
    }

    #[must_use]
    pub fn normalize(self) -> Self {
        self * self.length().recip()
    }

    #[must_use]
    pub fn floor(self) -> Self {
        self.map(num::Float::floor)
    }

    #[must_use]
    pub fn round(self) -> Self {
        self.map(num::Float::round)
    }

    #[must_use]
    pub fn ceil(self) -> Self {
        self.map(num::Float::ceil)
    }

    /// Returns the *unsigned* angle between this vector and the given vector.
    #[must_use]
    pub fn angle_between(self, rhs: Self) -> T {
        let one = num::one::<T>();
        num::clamp(
            self.dot(rhs) / (self.length_squared() * rhs.length_squared()).sqrt(),
            -one,
            one,
        )
        .acos()
    }

    #[must_use]
    pub fn project_onto(self, onto: Self) -> Self {
        onto * (self.dot(onto) / onto.length_squared())
    }
}

impl<T: num::NumCast, const N: usize> Vector<T, N> {
    #[must_use]
    pub fn cast<U: num::NumCast>(self) -> Vector<U, N> {
        self.map(cast::cast)
    }

    #[must_use]
    pub fn try_cast<U: num::NumCast>(self) -> Option<Vector<U, N>> {
        self.try_map(cast::try_cast)
    }
}

impl<T> Vec2<T> {
    #[must_use]
    pub const fn new(x: T, y: T) -> Self {
        Self([x, y])
    }

    #[must_use]
    pub fn extend(self, z: T) -> Vec3<T> {
        let [x, y] = self.0;
        Vector([x, y, z])
    }
}

impl<T> Vec2<T>
where
    T: ops::Mul<T, Output = T> + ops::Sub<T, Output = T>,
{
    /// Perpendicular dot product, AKA the 2d cross product.
    #[must_use]
    pub fn perp_dot(self, rhs: Self) -> T {
        let Self([x, y]) = self;
        let Self([rx, ry]) = rhs;
        x * ry - y * rx
    }
}

impl<T> Vec2<T>
where
    T: ops::Mul<T, Output = T> + ops::Add<T, Output = T> + ops::Sub<T, Output = T> + Copy,
{
    /// Rotate `self` by the angle of `rhs`, multiply `self` by the magnitude of `rhs`.
    #[must_use]
    pub fn rotate(self, rhs: Self) -> Self {
        let Self([x, y]) = self;
        let Self([rx, ry]) = rhs;
        Self([x * rx - y * ry, y * rx + x * ry])
    }
}

impl<T: num::Float> Vec2<T> {
    /// Returns a unit vector with the given angle from the unit X vector.
    #[must_use]
    pub fn from_unit_angle(angle: T) -> Self {
        let (y, x) = angle.sin_cos();
        Self([x, y])
    }

    /// Returns the *signed* angle from this vector to the given vector.
    #[must_use]
    pub fn angle_to(self, rhs: Self) -> T {
        let angle = self.angle_between(rhs);
        angle * self.perp_dot(rhs).signum()
    }

    /// Returns the *signed* angle from the given vector to this vector.
    #[must_use]
    pub fn angle_from(self, rhs: Self) -> T {
        rhs.angle_to(self)
    }

    /// Returns the signed angle from the unit X vector.
    #[must_use]
    pub fn unit_angle(self) -> T {
        Vec2::new(T::one(), T::zero()).angle_to(self)
    }

    #[must_use]
    pub fn rotate_angle(self, angle: T) -> Self {
        self.rotate(Self::from_unit_angle(angle))
    }
}

impl<T> Vec3<T> {
    #[must_use]
    pub const fn new(x: T, y: T, z: T) -> Self {
        Self([x, y, z])
    }

    #[must_use]
    pub fn extend(self, w: T) -> Vec4<T> {
        let [x, y, z] = self.0;
        Vector([x, y, z, w])
    }

    #[must_use]
    pub fn truncate(self) -> Vec2<T> {
        let Self([x, y, _]) = self;
        Vector([x, y])
    }
}

impl<T: num::Float> Vec3<T> {
    #[must_use]
    pub fn cross(self, rhs: Self) -> Self {
        Self([
            self[1] * rhs[2] - rhs[1] * self[2],
            self[2] * rhs[0] - rhs[2] * self[0],
            self[0] * rhs[1] - rhs[0] * self[1],
        ])
    }
}

impl<T> Vec4<T> {
    #[must_use]
    pub const fn new(x: T, y: T, z: T, w: T) -> Self {
        Self([x, y, z, w])
    }

    #[must_use]
    pub fn truncate(self) -> Vec3<T> {
        let Self([x, y, z, _]) = self;
        Vector([x, y, z])
    }
}
