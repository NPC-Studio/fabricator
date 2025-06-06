use fabricator_math::Vec2;

use crate::{
    simplex::{Projection, Simplex},
    support::SupportMap,
};

#[derive(Debug)]
pub struct Settings<N> {
    pub tolerance: N,
    /// If the GJK algorithm has not otherwise terminated after this number of iterations, it will
    /// return with either `GjkResult::Proximity` or `GjkResult::NoProximity`, depending on whether
    /// the current best guess is within `max_distance`.
    pub max_iterations: u32,
    /// The GJK algorithm will return `GjkResult::NoProximity` once the support map can be
    /// determined to be more than `max_distance` away from the origin.
    pub max_distance: N,
    /// If `find_closest_point` is false, then `GjkResult::Proximity` will be returned as soon as
    /// a point is found within the requested `max_distance`. If it is true, the GJK algorithm will
    /// continue to try to find the closest point possible given `tolerance` and `max_iterations`.
    pub find_closest_point: bool,
}

/// The result of the GJK algorithm
#[derive(Debug)]
pub enum Result<N> {
    /// The origin was determined to be inside the support map, or on the surface within the given
    /// tolerance. If the terminating simplex is a triangle, then the origin is known to be
    /// strictly *inside* the support map, and is inside the returned triangle.
    Touching,
    /// The support map is within the requested max distance from the origin. If
    /// `find_closest_point` is false, then this is returned as soon as a support point is found
    /// within the given max distance. If `find_closest_point` is true, then the algorithm will
    /// continue until the closest point is within tolerance of being the closest point on the
    /// simplex to the origin, or the algorithm stops making progress.
    Proximity(Projection<N>),
    /// The origin is further away from the support map than the requested max distance.
    NoProximity,
}

/// Use the GJK algorithm to determine the location of the origin relative to the given support map.
///
/// If the provided simplex is not empty, it must contain points that lie on the boundary of
/// the provided support map. If it is empty it will be automatically initialized, but it may be
/// provided as non-empty as an optimization.
pub fn gjk<N, S>(
    settings: Settings<N>,
    support: S,
    simplex: &mut Simplex<N, S::Context>,
) -> Result<N>
where
    N: num::Float,
    S: SupportMap<N>,
{
    let tolerance = settings.tolerance;
    let tolerance_squared = tolerance.powi(2);

    let mut max_bound = N::max_value();
    if simplex.is_empty() {
        *simplex = Simplex::point(support.support_point(Vec2::new(N::one(), N::zero())));
    }

    let mut closest = simplex.project_and_reduce(Vec2::zero()).unwrap();
    if simplex.len() == 3 {
        return Result::Touching;
    }

    let mut iterations = 0;

    loop {
        let dir = -simplex.eval(closest);
        let dir_length = dir.length();
        if dir_length <= tolerance {
            return Result::Touching;
        }
        let ndir = dir / dir_length;

        if !(dir_length < max_bound) {
            // Our upper bound has stopped converging, so we can make no more progress
            return if max_bound <= settings.max_distance {
                Result::Proximity(closest)
            } else {
                Result::NoProximity
            };
        } else {
            max_bound = dir_length;
        }

        let support_point = support.support_point(ndir);
        let min_bound = -ndir.dot(support_point.point);

        if min_bound > settings.max_distance {
            // We know that we are at least the requested max distance away from the origin.
            return Result::NoProximity;
        } else if !settings.find_closest_point
            && min_bound > N::zero()
            && max_bound <= settings.max_distance
        {
            // We know that we are not intersecting and we know that we are within the requested max
            // distance.
            return Result::Proximity(closest);
        } else if max_bound - min_bound <= tolerance {
            // Our precision is close enough for finding the closest point.
            return Result::Proximity(closest);
        }

        if !simplex.add_unique_point(support_point, tolerance_squared) {
            // Our simplex has stopped changing and thus we have stopped converging
            return if max_bound <= settings.max_distance {
                Result::Proximity(closest)
            } else {
                Result::NoProximity
            };
        }

        closest = simplex.project_and_reduce(Vec2::zero()).unwrap();
        if simplex.len() == 3 {
            return Result::Touching;
        }

        if iterations >= settings.max_iterations {
            return if max_bound <= settings.max_distance {
                Result::Proximity(closest)
            } else {
                Result::NoProximity
            };
        } else {
            iterations += 1;
        }
    }
}
