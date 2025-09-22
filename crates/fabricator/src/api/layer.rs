use fabricator_vm as vm;
use gc_arena::{Collect, Gc, Rootable};

use crate::{
    api::magic::MagicExt as _,
    state::{Layer, State, state::LayerId},
};

#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct LayerIdUserData<'gc> {
    #[collect(require_static)]
    pub id: LayerId,
    pub name: Option<vm::String<'gc>>,
}

impl<'gc> LayerIdUserData<'gc> {
    pub fn new(
        ctx: vm::Context<'gc>,
        layer_id: LayerId,
        name: Option<vm::String<'gc>>,
    ) -> vm::UserData<'gc> {
        #[derive(Collect)]
        #[collect(require_static)]
        struct Methods;

        impl<'gc> vm::UserDataMethods<'gc> for Methods {
            fn coerce_string(
                &self,
                ud: vm::UserData<'gc>,
                _ctx: vm::Context<'gc>,
            ) -> Option<vm::String<'gc>> {
                ud.downcast::<Rootable![LayerIdUserData<'_>]>()
                    .unwrap()
                    .name
            }

            fn coerce_integer(&self, ud: vm::UserData<'gc>, _ctx: vm::Context<'gc>) -> Option<i64> {
                Some(
                    ud.downcast::<Rootable![LayerIdUserData<'_>]>()
                        .unwrap()
                        .id
                        .index() as i64,
                )
            }
        }

        #[derive(Collect)]
        #[collect(no_drop)]
        struct MethodsSingleton<'gc>(Gc<'gc, dyn vm::UserDataMethods<'gc>>);

        impl<'gc> vm::Singleton<'gc> for MethodsSingleton<'gc> {
            fn create(ctx: vm::Context<'gc>) -> Self {
                let methods = Gc::new(&ctx, Methods);
                MethodsSingleton(gc_arena::unsize!(methods => dyn vm::UserDataMethods<'gc>))
            }
        }

        let methods = ctx.singleton::<Rootable![MethodsSingleton<'_>]>().0;

        let userdata = vm::UserData::new::<Rootable![LayerIdUserData<'_>]>(
            &ctx,
            LayerIdUserData { id: layer_id, name },
        );
        userdata.set_methods(&ctx, Some(methods));

        userdata
    }

    pub fn downcast(
        userdata: vm::UserData<'gc>,
    ) -> Result<&'gc Self, vm::user_data::BadUserDataType> {
        userdata.downcast::<Rootable![LayerIdUserData<'_>]>()
    }
}

pub fn find_layer<'gc>(
    state: &State,
    layer_id_or_name: vm::Value<'gc>,
) -> Result<LayerId, vm::RuntimeError> {
    match layer_id_or_name {
        vm::Value::String(name) => state
            .named_layers
            .get(name.as_str())
            .copied()
            .ok_or_else(|| vm::RuntimeError::msg(format!("no such layer named {name:?}"))),
        vm::Value::UserData(ud) => Ok(LayerIdUserData::downcast(ud)?.id),
        _ => Err(vm::TypeError {
            expected: "userdata or string",
            found: layer_id_or_name.type_name(),
        }
        .into()),
    }
}

pub fn layers_api<'gc>(ctx: vm::Context<'gc>) -> vm::MagicSet<'gc> {
    let mut magic = vm::MagicSet::new();

    let layer_create = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (depth, name): (i32, Option<vm::String>) = exec.stack().consume(ctx)?;

        let layer_ud = State::ctx_with_mut(ctx, |state| {
            if let Some(name) = name {
                if state.named_layers.contains_key(name.as_str()) {
                    return Err(vm::RuntimeError::msg(format!(
                        "duplicate layer named {:?}",
                        name
                    )));
                }
            }

            let layer_id = state.layers.insert_with_id(|id| {
                let layer_ud = LayerIdUserData::new(ctx, id, name);
                Layer {
                    depth,
                    visible: true,
                    this: ctx.stash(layer_ud),
                }
            });

            if let Some(name) = name {
                state
                    .named_layers
                    .insert(name.as_str().to_owned(), layer_id);
            }

            Ok(ctx.fetch(&state.layers[layer_id].this))
        })??;

        exec.stack().replace(ctx, layer_ud);
        Ok(())
    });
    magic
        .add_constant(&ctx, ctx.intern("layer_create"), layer_create)
        .unwrap();

    let layer_get_id = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let name: vm::String = exec.stack().consume(ctx)?;

        let layer_ud = State::ctx_with_mut(ctx, |state| {
            if let Some(&layer_id) = state.named_layers.get(name.as_str()) {
                Ok(ctx.fetch(&state.layers[layer_id].this))
            } else {
                Err(vm::RuntimeError::msg(format!(
                    "no such layer named {name:?}"
                )))
            }
        })??;

        exec.stack().replace(ctx, layer_ud);
        Ok(())
    });
    magic
        .add_constant(&ctx, ctx.intern("layer_get_id"), layer_get_id)
        .unwrap();

    let layer_get_depth = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let layer_id_or_name: vm::Value = exec.stack().consume(ctx)?;
        let layer_id = State::ctx_with(ctx, |state| find_layer(state, layer_id_or_name))??;
        exec.stack().replace(
            ctx,
            State::ctx_with(ctx, |state| state.layers[layer_id].depth)?,
        );
        Ok(())
    });
    magic
        .add_constant(&ctx, ctx.intern("layer_get_depth"), layer_get_depth)
        .unwrap();

    let layer_depth = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (layer_id_or_name, depth): (vm::Value, i32) = exec.stack().consume(ctx)?;
        State::ctx_with_mut(ctx, |state| {
            let layer_id = find_layer(state, layer_id_or_name)?;
            state.layers[layer_id].depth = depth;
            Ok(())
        })?
    });
    magic
        .add_constant(&ctx, ctx.intern("layer_depth"), layer_depth)
        .unwrap();

    let layer_set_visible = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (layer_id_or_name, visible): (vm::Value, bool) = exec.stack().consume(ctx)?;
        State::ctx_with_mut(ctx, |state| {
            let layer_id = find_layer(state, layer_id_or_name)?;
            state.layers[layer_id].visible = visible;
            Ok(())
        })?
    });
    magic
        .add_constant(&ctx, ctx.intern("layer_set_visible"), layer_set_visible)
        .unwrap();

    magic
}
