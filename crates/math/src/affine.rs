use crate::{
    matrix::{Mat2, Mat3, Matrix},
    vector::{Vec2, Vector},
};

#[derive(Debug, Copy, Clone)]
pub struct Affine<T, const N: usize> {
    pub matrix: Matrix<T, N, N>,
    pub translation: Vector<T, N>,
}

pub type Affine2<T> = Affine<T, 2>;

impl<T, const N: usize> Affine<T, N>
where
    T: num::Zero + num::One,
{
    pub fn new() -> Self {
        Self {
            matrix: Matrix::identity(),
            translation: Vector::zero(),
        }
    }
}

impl<T: num::NumCast, const N: usize> Affine<T, N> {
    pub fn cast<U: num::NumCast>(self) -> Affine<U, N> {
        Affine {
            matrix: self.matrix.cast(),
            translation: self.translation.cast(),
        }
    }

    pub fn try_cast<U: num::NumCast>(self) -> Option<Affine<U, N>> {
        Some(Affine {
            matrix: self.matrix.try_cast()?,
            translation: self.translation.try_cast()?,
        })
    }
}

impl<T: num::Num + Copy, const N: usize> Affine<T, N> {
    pub fn then(self, other: Affine<T, N>) -> Self {
        Self {
            matrix: other.matrix * self.matrix,
            translation: other.matrix * self.translation + other.translation,
        }
    }

    pub fn translate(self, translation: Vector<T, N>) -> Self {
        self.then(Affine {
            matrix: Matrix::identity(),
            translation,
        })
    }

    pub fn scale(self, scale: Vector<T, N>) -> Self {
        self.then(Affine {
            matrix: Matrix::from_diagonal(scale),
            translation: Vector::zero(),
        })
    }
}

impl<T> Affine2<T>
where
    T: num::Zero + num::One,
{
    pub fn into_mat3(self) -> Mat3<T> {
        let [c1, c2] = self.matrix.into_cols();
        Mat3::from_cols([
            c1.extend(T::zero()),
            c2.extend(T::zero()),
            self.translation.extend(T::one()),
        ])
    }
}

impl<T: num::Float> Affine2<T> {
    pub fn rotate(&self, angle: T) -> Self {
        let (sina, cosa) = angle.sin_cos();
        self.then(Self {
            matrix: Mat2::from_cols([Vec2::new(cosa, sina), Vec2::new(-sina, cosa)]),
            translation: Vec2::zero(),
        })
    }

    pub fn inverse(&self) -> Self {
        let matrix = self.matrix.inverse();
        let translation = -(matrix * self.translation);
        Self {
            matrix,
            translation,
        }
    }

    pub fn transform_point(&self, p: Vec2<T>) -> Vec2<T> {
        self.matrix * p + self.translation
    }

    pub fn transform_vector(&self, v: Vec2<T>) -> Vec2<T> {
        self.matrix * v
    }
}
