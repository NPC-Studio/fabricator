use either::Either;
use fabricator_math::Vec2;

/// A point returned by a `SupportMap` which is a point on the boundary of a convex shape, along
/// with some context for this point.
#[derive(Debug, Copy, Clone)]
pub struct SupportPoint<N, C> {
    pub point: Vec2<N>,
    pub context: C,
}

impl<N, C> SupportPoint<N, C> {
    pub fn new(point: Vec2<N>, context: C) -> Self {
        Self { point, context }
    }
}

/// A trait for convex shapes that are representable by a support mapping function.
///
/// A "support mapping function", or just "support function", finds the point inside a given convex
/// shape that is furthest in a given direction. Since the shape is convex, the provided direction
/// and the returned point produce a "supporting hyperplane", which in 2D is just a line that
/// is tangential to the shape (touches it at at least one point) where every point in the shape
/// lies on the side of the line opposite the normal direction. Any convex shape can be uniquely
/// determined by its support function.
pub trait SupportMap<N> {
    /// `SupportMap` instances return custom `Context` types for each support point. This can be
    /// used when transforming or combining `SupportMap` functions so that the original points can
    /// be determined.
    type Context: Copy;

    /// Produce the distance from the origin of the point in the shape furthest in the given
    /// normalized direction, along with the context for this point.
    ///
    /// The function is allowed to assume the given direction is non-NaN and normalized, and may
    /// return inaccurate results if it is not.
    fn support_point(&self, ndir: Vec2<N>) -> SupportPoint<N, Self::Context>;
}

impl<'a, S, N> SupportMap<N> for &'a S
where
    S: SupportMap<N>,
{
    type Context = S::Context;

    fn support_point(&self, ndir: Vec2<N>) -> SupportPoint<N, S::Context> {
        (*self).support_point(ndir)
    }
}

impl<S, N> SupportMap<N> for Box<S>
where
    S: SupportMap<N> + ?Sized,
{
    type Context = S::Context;

    fn support_point(&self, ndir: Vec2<N>) -> SupportPoint<N, S::Context> {
        (**self).support_point(ndir)
    }
}

impl<A, B, N> SupportMap<N> for Either<A, B>
where
    A: SupportMap<N>,
    B: SupportMap<N>,
{
    type Context = Either<A::Context, B::Context>;

    fn support_point(&self, ndir: Vec2<N>) -> SupportPoint<N, Self::Context> {
        match self {
            Either::Left(sm) => {
                let sp = sm.support_point(ndir);
                SupportPoint {
                    point: sp.point,
                    context: Either::Left(sp.context),
                }
            }
            Either::Right(sm) => {
                let sp = sm.support_point(ndir);
                SupportPoint {
                    point: sp.point,
                    context: Either::Right(sp.context),
                }
            }
        }
    }
}
