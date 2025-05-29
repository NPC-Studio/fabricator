use std::ops;

use fabricator_math::{Affine2, Box2, Mat2, Vec2};

use crate::support::{SupportMap, SupportPoint};

#[derive(Debug, Copy, Clone)]
pub struct Point<N>(pub Vec2<N>);

impl<N: Copy> SupportMap<N> for Point<N> {
    type Context = Vec2<N>;

    fn support_point(&self, _ndir: Vec2<N>) -> SupportPoint<N, Vec2<N>> {
        SupportPoint {
            point: self.0,
            context: self.0,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Circle<N> {
    pub center: Vec2<N>,
    pub radius: N,
}

impl<N> SupportMap<N> for Circle<N>
where
    N: Copy + ops::Add<N, Output = N> + ops::Sub<N, Output = N> + ops::Mul<N, Output = N>,
{
    type Context = Vec2<N>;

    fn support_point(&self, ndir: Vec2<N>) -> SupportPoint<N, Self::Context> {
        SupportPoint {
            point: self.center + ndir * self.radius,
            context: ndir,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct AABox<N>(pub Box2<N>);

impl<N> SupportMap<N> for AABox<N>
where
    N: num::Num + PartialOrd + Copy,
{
    type Context = Vec2<N>;

    fn support_point(&self, ndir: Vec2<N>) -> SupportPoint<N, Vec2<N>> {
        let x = if ndir[0] > N::zero() {
            self.0.max[0]
        } else {
            self.0.min[0]
        };
        let y = if ndir[1] > N::zero() {
            self.0.max[1]
        } else {
            self.0.min[1]
        };

        SupportPoint {
            point: Vec2::new(x, y),
            context: Vec2::new(x, y),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Negate<S>(pub S);

impl<S, N> SupportMap<N> for Negate<S>
where
    N: ops::Neg<Output = N>,
    S: SupportMap<N>,
{
    type Context = S::Context;

    fn support_point(&self, ndir: Vec2<N>) -> SupportPoint<N, Self::Context> {
        let SupportPoint { point, context } = self.0.support_point(-ndir);
        SupportPoint {
            point: -point,
            context,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Add<A, B>(pub A, pub B);

impl<A, B, N> SupportMap<N> for Add<A, B>
where
    A: SupportMap<N>,
    B: SupportMap<N>,
    N: Copy + ops::Add<Output = N>,
{
    type Context = (A::Context, B::Context);

    fn support_point(&self, ndir: Vec2<N>) -> SupportPoint<N, Self::Context> {
        let SupportPoint {
            point: av,
            context: ac,
        } = self.0.support_point(ndir);
        let SupportPoint {
            point: bv,
            context: bc,
        } = self.1.support_point(ndir);
        SupportPoint {
            point: av + bv,
            context: (ac, bc),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Translate<S, N>(pub S, pub Vec2<N>);

impl<S, N> SupportMap<N> for Translate<S, N>
where
    N: Copy + ops::Add<Output = N>,
    S: SupportMap<N>,
{
    type Context = S::Context;

    fn support_point(&self, ndir: Vec2<N>) -> SupportPoint<N, Self::Context> {
        let SupportPoint { point, context } = self.0.support_point(ndir);
        SupportPoint {
            point: point + self.1,
            context,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct AffineTransform<S, N> {
    pub support: S,
    pub transform: Affine2<N>,
    pub inverse: Mat2<N>,
}

impl<S, N> AffineTransform<S, N>
where
    N: num::Float,
{
    pub fn new(support: S, transform: Affine2<N>) -> Self {
        AffineTransform {
            support,
            transform,
            inverse: transform.matrix.inverse(),
        }
    }
}

impl<S, N> SupportMap<N> for AffineTransform<S, N>
where
    N: num::Float,
    S: SupportMap<N>,
{
    type Context = S::Context;

    fn support_point(&self, ndir: Vec2<N>) -> SupportPoint<N, Self::Context> {
        let ndir = (self.inverse * ndir).normalize();
        let SupportPoint { point, context } = self.support.support_point(ndir);
        SupportPoint {
            point: self.transform.transform_point(point),
            context,
        }
    }
}
