use fabricator_math::{Box2, Vec2};

use crate::{gjk, simplex::Simplex, support::SupportMap, support_ext::SupportMapExt, support_maps};

#[test]
fn test_gjk_point_circle() {
    let circle = support_maps::Circle {
        center: Vec2::splat(2.0),
        radius: 1.0,
    };

    let point_inside = support_maps::Point(Vec2::splat(2.5));
    let point_outside = support_maps::Point(Vec2::splat(3.5));

    assert!(matches!(
        gjk::gjk(
            gjk::Settings {
                tolerance: 0.0001,
                max_iterations: 12,
                max_distance: 0.0,
                find_closest_point: false,
            },
            circle.intersect(point_inside),
            &mut Simplex::empty(),
        ),
        gjk::Result::Touching
    ));

    assert!(matches!(
        gjk::gjk(
            gjk::Settings {
                tolerance: 0.0001,
                max_iterations: 12,
                max_distance: 0.0,
                find_closest_point: false,
            },
            circle.intersect(point_outside),
            &mut Simplex::empty(),
        ),
        gjk::Result::NoProximity
    ));
}

#[test]
fn test_gjk_line_circle() {
    let circle = support_maps::Circle {
        center: Vec2::splat(2.0),
        radius: 1.0,
    };

    let intersecting_line = support_maps::Line([Vec2::new(1.5, 1.0), Vec2::new(3.5, 3.0)]);
    let non_intersecting_line = support_maps::Line([Vec2::new(2.5, 1.0), Vec2::new(4.5, 3.0)]);

    assert!(matches!(
        gjk::gjk(
            gjk::Settings {
                tolerance: 0.0001,
                max_iterations: 12,
                max_distance: 0.0,
                find_closest_point: false,
            },
            circle.intersect(intersecting_line),
            &mut Simplex::empty(),
        ),
        gjk::Result::Touching
    ));

    assert!(matches!(
        gjk::gjk(
            gjk::Settings {
                tolerance: 0.0001,
                max_iterations: 12,
                max_distance: 0.0,
                find_closest_point: false,
            },
            circle.intersect(non_intersecting_line),
            &mut Simplex::empty(),
        ),
        gjk::Result::NoProximity
    ));
}

#[test]
fn test_gjk_line_ellipse() {
    let ellipse = support_maps::Ellipse {
        center: Vec2::new(32.0, 0.0),
        radius: Vec2::new(32.0, 16.0),
    };

    let line = support_maps::Line([Vec2::new(-32.0, -32.0), Vec2::new(32.0, 32.0)]);

    assert!(matches!(
        gjk::gjk(
            gjk::Settings {
                tolerance: 0.0001,
                max_iterations: 12,
                max_distance: 0.0,
                find_closest_point: false,
            },
            ellipse.intersect(line),
            &mut Simplex::empty(),
        ),
        gjk::Result::Touching
    ));
}

#[test]
fn test_support_maps_find_furthest() {
    fn test_support_map<S: SupportMap<f64>>(s: S) {
        // For the provided support map, try to ensure that points returned from S::support_point
        // for various normal vectors have the largest dot product in comparison to points returned
        // from S::support_point with the same normal vector rotated by various offsets.

        for dir in (0..50).map(|i| Vec2::new(1.0, 0.0).rotate_angle(0.63 * i as f64)) {
            let support_point = s.support_point(dir);
            for rot_angle in (0..20).map(|i| 0.001 * 1.55f64.powi(i)) {
                for rot_angle in [rot_angle, -rot_angle] {
                    let other_dir = dir.rotate_angle(rot_angle);
                    let other_support_point = s.support_point(other_dir);
                    assert!(support_point.point.dot(dir) >= other_support_point.point.dot(dir));
                }
            }
        }
    }

    test_support_map(support_maps::Point(Vec2::splat(2.5)));
    test_support_map(support_maps::Line([Vec2::splat(-2.5), Vec2::splat(3.0)]));
    test_support_map(support_maps::Line([Vec2::splat(-2.5), Vec2::splat(3.0)]));
    test_support_map(support_maps::Circle {
        center: Vec2::new(1.7, 1.9),
        radius: 1.0,
    });
    test_support_map(support_maps::Ellipse {
        center: Vec2::new(-1.7, 1.5),
        radius: Vec2::new(7.0, 2.0),
    });
    test_support_map(support_maps::AABox(Box2::with_size(
        Vec2::new(-5.0, -3.0),
        Vec2::new(10.0, 7.0),
    )));
}
