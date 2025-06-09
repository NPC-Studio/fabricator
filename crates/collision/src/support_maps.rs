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
        // Parameterize the elipse by an angle `t` (theta). For now, ignore the center and assume
        // the ellipse is centered on the origin.
        //
        // Define `nx` and `ny` as the x and y component of our normalized direction vector `n`
        // (`ndir`).
        //
        // Define the semi-major x-axis (`radius[0]`) as `a` and semi-major y-axis (`radius[1]`)
        // as `b`.
        //
        // The equation for every point on this ellipse is:
        //
        // 1) `x = a * cos(t)` and `y = `b * sin(t)`
        //
        // for some theta `t`. We want to find a point on this ellipse that maximizes the projection
        // onto `n`, and the equation for the projection onto `n` is:
        //
        // 2) `nx * a * cos(t) + ny * b * sin(t)`
        //
        // Since we are trying to maximize this value, we find the derivative and look for the point
        // at which the derivative is 0. This equation for the derivative of the projection with
        // respect to `t` is:
        //
        // 3) `nx * a * -sin(t) + ny * b * cos(t) = 0`
        //
        // We can re-arrange this equation into:
        //
        // 4) `cos(t) / sin(t) = nx * a / ny * b`
        //
        // We also know that for any `t`:
        //
        // 5) `cos(t)^2 + sin(t)^2 = 1`
        //
        // so using both of these we can produce formulas for `cos(t)` and `sin(t)`:
        //
        // 6) `cos(t) = nx * a / sqrt((nx * a)^2 + (ny * b)^2)`
        // 7) `sin(t) = ny * b / sqrt((nx * a)^2 + (ny * b)^2)`
        //
        // Technically these systems of equations have *two* solutions, one for positive `a` and `b`
        // and one for negative `-a` and `-b`. The negative solution *minimizes* the projection and
        // the positive one maximizes it.
        //
        // Taking the solution that maximizes the projection and plugging in both of these back into
        // equation 1 and produce equations for both points `x` and `y`:
        //
        // 8) `x = nx * a^2 / sqrt((nx * a)^2 + (ny * b)^2)`
        // 9) `y = ny * b^2 / sqrt((nx * a)^2 + (ny * b)^2)`
        //
        // which expressed in vector pseudo code is:
        //
        // 8) `[a, b] * norm(n * [a, b])`
        //
        // Now we can just use this equation and make sure to add the center of the ellipse.

        let point = self.center + (ndir * self.radius).normalize() * self.radius;

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
        Self(support_map, Vec2::from_unit_angle(angle))
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
