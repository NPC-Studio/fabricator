use std::ops::Deref;

use arrayvec::ArrayVec;
use fabricator_math::Vec2;

use crate::{
    projection::{LineProjection, TriangleProjection},
    support::SupportPoint,
};

/// Either the empty set, a single point, a line, or a triangle whose vertices lie on the boundary
/// of a support mapping.
#[derive(Debug, Clone)]
pub struct Simplex<N, C>(ArrayVec<SupportPoint<N, C>, 3>);

/// The result of projecting a point onto a simplex.
#[derive(Copy, Clone, Debug)]
pub enum Projection<N> {
    /// The closest point on the simplex is a single point, and the simplex has been reduced to this
    /// single point.
    Point,
    /// The closest point to the given point lies between two points, and the simplex has been
    /// reduced to these two points. Returns the barycentric coordinates of the closest point.
    Line([N; 2]),
    /// The simplex is a triangle which contains the given point, and the simplex remains
    /// unchanged. The projection is equal to the input point.
    Triangle(Vec2<N>),
}

impl<N, C> Default for Simplex<N, C> {
    fn default() -> Self {
        Self(ArrayVec::new())
    }
}

impl<N, C> Deref for Simplex<N, C> {
    type Target = [SupportPoint<N, C>];

    fn deref(&self) -> &Self::Target {
        self.0.as_slice()
    }
}

impl<N, C> Simplex<N, C>
where
    N: num::Float,
    C: Copy,
{
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn point(p: SupportPoint<N, C>) -> Self {
        Self(ArrayVec::from_iter([p]))
    }

    pub fn push(&mut self, point: SupportPoint<N, C>) {
        self.0.push(point);
    }

    /// Add a unique point to the given simplex.
    ///
    /// `eq_tol` is the *squared* equality tolerance for the matching point check. If the simplex
    /// length is already 3, or if any point in the simplex `x` satisfies the condition
    /// `(p - x).square_length() <= eq_tol`, then this function will do nothing and return false.
    /// Otherwise, the point will be added to the simplex at the end and this function will return
    /// true.
    pub fn add_unique_point(&mut self, point: SupportPoint<N, C>, eq_tol: N) -> bool {
        if self.len() == 3 {
            return false;
        }

        for sp in self.iter() {
            if (sp.point - point.point).length_squared() <= eq_tol {
                return false;
            }
        }

        self.push(point);
        true
    }

    /// Projects a point onto the simplex and reduces the simplex to the smallest simplex that still
    /// contains the projected point. If the simplex is empty, returns None.
    pub fn project_and_reduce(&mut self, point: Vec2<N>) -> Option<Projection<N>> {
        let keep = |this: &mut Self, idx: &[usize]| {
            this.0 = idx.iter().map(|&i| this.0[i]).collect();
        };

        match self.len() {
            0 => None,
            1 => Some(Projection::Point),
            2 => match LineProjection::project(point, [self[0].point, self[1].point]) {
                LineProjection::Point(p) => {
                    keep(self, &[p as usize]);
                    Some(Projection::Point)
                }
                LineProjection::Line([u, v]) => Some(Projection::Line([u, v])),
            },
            3 => match TriangleProjection::project(
                point,
                [self[0].point, self[1].point, self[2].point],
            ) {
                TriangleProjection::Point(p) => {
                    keep(self, &[p as usize]);
                    Some(Projection::Point)
                }
                TriangleProjection::Line(points, weights) => {
                    keep(self, &points.map(|p| p as usize));
                    Some(Projection::Line(weights))
                }
                TriangleProjection::Triangle => Some(Projection::Triangle(point)),
            },
            _ => unreachable!(),
        }
    }

    pub fn eval(&self, projection: Projection<N>) -> Vec2<N> {
        match projection {
            Projection::Point => self[0].point,
            Projection::Line([u, v]) => self[0].point * u + self[1].point * v,
            Projection::Triangle(p) => p,
        }
    }
}
