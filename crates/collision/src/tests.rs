use fabricator_math::Vec2;

use crate::{gjk, simplex::Simplex, support_ext::SupportMapExt, support_maps};

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
