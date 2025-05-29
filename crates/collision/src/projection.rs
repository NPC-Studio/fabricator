use fabricator_math::Vec2;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum LinePoint {
    A,
    B,
}

#[derive(Debug, Copy, Clone)]
pub enum LineProjection<N> {
    Point(LinePoint),
    Line([N; 2]),
}

impl<N: num::Float> LineProjection<N> {
    pub fn project(p: Vec2<N>, line: [Vec2<N>; 2]) -> Self {
        use LinePoint::*;

        let _0 = N::zero();
        let _1 = N::one();

        let [a, b] = line;
        let ab = b - a;
        let t = ab.dot(p - a) / ab.length_squared();

        if t <= _0 {
            Self::Point(A)
        } else if t >= _1 {
            Self::Point(B)
        } else {
            Self::Line([_1 - t, t])
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TrianglePoint {
    A,
    B,
    C,
}

#[derive(Debug, Copy, Clone)]
pub enum TriangleProjection<N> {
    Point(TrianglePoint),
    Line([TrianglePoint; 2], [N; 2]),
    Triangle,
}

impl<N: num::Float> TriangleProjection<N> {
    pub fn project(p: Vec2<N>, tri: [Vec2<N>; 3]) -> Self {
        use TrianglePoint::*;

        // This is a modification of the algorithm for point -> triangle projection in:
        //
        // Real Time Collision Detection, Ericson (2005)

        let _0 = N::zero();
        let _1 = N::one();

        let [a, b, c] = tri;

        let ab = b - a;
        let ac = c - a;
        let ap = p - a;

        let ab_ap = ab.dot(ap);
        let ac_ap = ac.dot(ap);

        if ab_ap <= _0 && ac_ap <= _0 {
            return Self::Point(A);
        }

        let bp = p - b;
        let ab_bp = ab.dot(bp);
        let ac_bp = ac.dot(bp);

        if ab_bp >= _0 && ac_bp <= ab_bp {
            return Self::Point(B);
        }

        let cp = p - c;
        let ab_cp = ab.dot(cp);
        let ac_cp = ac.dot(cp);

        if ac_cp >= _0 && ab_cp <= ac_cp {
            return Self::Point(C);
        }

        let n = ab.perp_dot(ac);
        let vc = n * ab.perp_dot(ap);
        if vc <= _0 && ab_ap >= _0 && ab_bp <= _0 {
            let t = ab_ap / ab.length_squared();
            return Self::Line([A, B], [_1 - t, t]);
        }

        let vb = -n * ac.perp_dot(cp);
        if vb <= _0 && ac_ap >= _0 && ac_cp <= _0 {
            let t = ac_ap / ac.length_squared();
            return Self::Line([A, C], [_1 - t, t]);
        }

        let bc = c - b;
        let va = n * bc.perp_dot(bp);
        if va <= _0 && ac_bp >= ab_bp && ab_cp >= ac_cp {
            let t = bc.dot(bp) / bc.length_squared();
            return Self::Line([B, C], [_1 - t, t]);
        }

        // The point lies inside the triangle. We don't return barycentric coordinates here, but
        // they can be calculated with
        //
        // let denom = _1 / (va + vb + vc);
        // let v = vb * denom;
        // let w = vc * denom;
        // let barycentric = [_1 - v - w, v, w];

        Self::Triangle
    }
}
