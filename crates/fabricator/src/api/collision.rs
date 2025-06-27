use fabricator_collision::{
    bound_box_tree::BoundBoxQuery, gjk, support_ext::SupportMapExt, support_maps,
};
use fabricator_math::Vec2;
use fabricator_vm as vm;
use gc_arena::{Collect, Rootable};

use crate::{
    api::magic::MagicExt as _,
    state::{InstanceState, State},
};

pub fn no_one<'gc>(ctx: vm::Context<'gc>) -> vm::UserData<'gc> {
    #[derive(Collect)]
    #[collect(require_static)]
    struct NoOne;

    #[derive(Collect)]
    #[collect(no_drop)]
    struct Singleton<'gc>(vm::UserData<'gc>);

    impl<'gc> vm::Singleton<'gc> for Singleton<'gc> {
        fn create(ctx: vm::Context<'gc>) -> Self {
            Singleton(vm::UserData::new_static(&ctx, NoOne))
        }
    }

    ctx.singleton::<Rootable![Singleton<'_>]>().0
}

pub fn all<'gc>(ctx: vm::Context<'gc>) -> vm::UserData<'gc> {
    #[derive(Collect)]
    #[collect(require_static)]
    struct All;

    #[derive(Collect)]
    #[collect(no_drop)]
    struct Singleton<'gc>(vm::UserData<'gc>);

    impl<'gc> vm::Singleton<'gc> for Singleton<'gc> {
        fn create(ctx: vm::Context<'gc>) -> Self {
            Singleton(vm::UserData::new_static(&ctx, All))
        }
    }

    ctx.singleton::<Rootable![Singleton<'_>]>().0
}

pub fn collision_api<'gc>(ctx: vm::Context<'gc>) -> vm::MagicSet<'gc> {
    let mut magic = vm::MagicSet::new();

    magic
        .add_constant(&ctx, ctx.intern("noone"), no_one(ctx).into())
        .unwrap();

    magic
        .add_constant(&ctx, ctx.intern("all"), all(ctx).into())
        .unwrap();

    let collision_line = vm::Callback::from_fn(&ctx, |ctx, _, mut stack| {
        let (x1, y1, x2, y2, _obj, _prec, notme): (f64, f64, f64, f64, vm::UserData, bool, bool) =
            stack.consume(ctx)?;

        let line_collision = support_maps::Line([Vec2::new(x1, y1), Vec2::new(x2, y2)]);
        let bounds = line_collision.bound_box();

        let self_instance = InstanceState::ctx_with(ctx, |inst| inst.instance_id).ok();

        State::ctx_with(ctx, |state| {
            let mut query = BoundBoxQuery::default();
            for &instance_id in query.intersects(&state.instance_bound_tree, bounds) {
                let Some(instance) = state.instances.get(instance_id) else {
                    continue;
                };

                if notme {
                    if self_instance == Some(instance_id) {
                        continue;
                    }
                }

                if let Some(collision) = state.instance_collision(instance_id) {
                    let intersection = collision.intersect(line_collision);

                    let res = gjk::gjk(
                        gjk::Settings {
                            tolerance: 0.1,
                            max_iterations: 12,
                            max_distance: 0.0,
                            find_closest_point: false,
                        },
                        intersection,
                        &mut Default::default(),
                    );

                    if matches!(res, gjk::Result::Touching) {
                        stack.replace(ctx, ctx.fetch(&instance.this));
                        return Ok(());
                    }
                }
            }

            stack.replace(ctx, no_one(ctx));
            Ok(())
        })?
    });
    magic
        .add_constant(&ctx, ctx.intern("collision_line"), collision_line.into())
        .unwrap();

    magic
}
