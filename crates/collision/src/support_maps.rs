use std::ops;

use fabricator_math::{Box2, Vec2};

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
        let point = self.center + ndir * self.radius;
        SupportPoint {
            point,
            context: point,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Ellipse<N> {
    pub center: Vec2<N>,
    pub radius: Vec2<N>,
}

impl<N> SupportMap<N> for Ellipse<N>
where
    N: num::Float,
{
    type Context = Vec2<N>;

    fn support_point(&self, ndir: Vec2<N>) -> SupportPoint<N, Self::Context> {
        let ndir = (ndir * self.radius)
            .normalize()
            .rotate_angle(num::cast::<_, _>(-0.000).unwrap());
        let point = self.center + ndir * self.radius;

        SupportPoint {
            point,
            context: point,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Line<N>(pub [Vec2<N>; 2]);

impl<N> SupportMap<N> for Line<N>
where
    N: PartialOrd + num::Zero + ops::Mul<N, Output = N> + ops::Add<N, Output = N> + Copy,
{
    type Context = Vec2<N>;

    fn support_point(&self, ndir: Vec2<N>) -> SupportPoint<N, Self::Context> {
        if ndir.dot(self.0[1]) > ndir.dot(self.0[0]) {
            SupportPoint {
                point: self.0[1],
                context: self.0[1],
            }
        } else {
            SupportPoint {
                point: self.0[0],
                context: self.0[0],
            }
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

        let point = Vec2::new(x, y);

        SupportPoint {
            point,
            context: point,
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
pub struct Rotate<S, N>(S, Vec2<N>);

impl<S, N> Rotate<S, N>
where
    N: num::Float,
{
    pub fn new(support_map: S, angle: N) -> Self {
        Self(support_map, Vec2::from_angle(angle))
    }
}

impl<S, N> SupportMap<N> for Rotate<S, N>
where
    N: num::Float,
    S: SupportMap<N>,
{
    type Context = S::Context;

    fn support_point(&self, ndir: Vec2<N>) -> SupportPoint<N, Self::Context> {
        let rot = self.1;
        let inv_rot = Vec2::new(rot[0], -rot[1]);

        let SupportPoint { point, context } = self.0.support_point(ndir.rotate(inv_rot));
        SupportPoint {
            point: point.rotate(rot),
            context,
        }
    }
}
