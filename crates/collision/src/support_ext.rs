use fabricator_math::{Affine2, Box2, Vec2};

use crate::{
    support::{SupportMap, SupportPoint},
    support_maps::{Add, AffineTransform, Negate, Translate},
};

#[derive(Debug, Copy, Clone)]
pub struct MapContext<S, F> {
    pub support_map: S,
    pub map_context: F,
}

impl<S, F, N, C1, C2> SupportMap<N> for MapContext<S, F>
where
    S: SupportMap<N, Context = C1>,
    F: Fn(C1) -> C2,
    C2: Copy,
{
    type Context = C2;

    fn support_point(&self, ndir: Vec2<N>) -> SupportPoint<N, C2> {
        let SupportPoint { point, context } = self.support_map.support_point(ndir);
        SupportPoint {
            point,
            context: (self.map_context)(context),
        }
    }
}

pub trait SupportMapExt<N>: SupportMap<N> {
    fn bound_box(&self) -> Box2<N>
    where
        N: num::Float,
    {
        let _0 = N::zero();
        let _1 = N::one();
        let xmin = self.support_point(Vec2::new(-_1, _0)).point[0];
        let xmax = self.support_point(Vec2::new(_1, _0)).point[0];
        let ymin = self.support_point(Vec2::new(_0, -_1)).point[1];
        let ymax = self.support_point(Vec2::new(_0, _1)).point[1];
        Box2 {
            min: Vec2::new(xmin, ymin),
            max: Vec2::new(xmax, ymax),
        }
    }

    fn negate(self) -> Negate<Self>
    where
        Self: Sized,
    {
        Negate(self)
    }

    fn add<S>(self, other: S) -> Add<Self, S>
    where
        Self: Sized,
    {
        Add(self, other)
    }

    fn translate(self, offset: Vec2<N>) -> Translate<Self, N>
    where
        Self: Sized,
    {
        Translate(self, offset)
    }

    fn transform(self, transform: Affine2<N>) -> AffineTransform<Self, N>
    where
        Self: Sized,
        N: num::Float,
    {
        AffineTransform::new(self, transform)
    }

    /// Takes this support function (A) and a provided support (B) function and finds the support
    /// function for the minkowski sum of A and -B.
    ///
    /// If we take the sum of shape A and the negation of shape B, then this shape will contain the
    /// origin if and only if the original shapes A and B intersect at some point (if they share a
    /// point `p` in common, `p + -p` must exist in the minkowski sum of A and -B).
    ///
    /// This is equivalent to calling `a.add(b.negate())`.
    fn intersect<S>(self, other: S) -> Add<Self, Negate<S>>
    where
        Self: Sized,
    {
        Add(self, Negate(other))
    }

    fn map_context<F>(self, map_context: F) -> MapContext<Self, F>
    where
        Self: Sized,
    {
        MapContext {
            support_map: self,
            map_context,
        }
    }
}

impl<S, N> SupportMapExt<N> for S where S: SupportMap<N> {}
