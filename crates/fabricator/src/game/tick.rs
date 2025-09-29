use anyhow::Error;
use fabricator_collision::support_ext::SupportMapExt as _;
use fabricator_util::{freeze::FreezeMany, index_containers::IndexSet};
use fabricator_vm as vm;

use crate::{
    api::{instance::InstanceUserData, layer::LayerIdUserData, tile::TileMapUserData},
    project::ObjectEvent,
    state::{
        DrawingState, EventState, InputState, Instance, State,
        configuration::RoomLayerType,
        state::{Layer, TileMap},
    },
};

pub fn tick_state(
    state: &mut State,
    drawing_state: &mut DrawingState,
    input_state: &InputState,
    interpreter: &mut vm::Interpreter,
    thread: &vm::StashedThread,
) -> Result<(), Error> {
    if let Some(next_room) = state.next_room.take() {
        log::info!(
            "switching room to {:?}",
            &state.config.rooms[next_room].name
        );

        // Only instances that exist at the *beginning* of the switch to the next room will have the
        // `RoomEnd` event called and be removed from the room (if not persistent). If an instance
        // is added during any of these events, it will be retained as though it was created at the
        // start of the next room.

        let room_end_instances = state
            .instances
            .iter()
            .filter_map(|(instance_id, instance)| {
                if !instance.dead {
                    Some(instance_id)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        let remove_instances = state
            .instances
            .iter()
            .filter_map(|(instance_id, instance)| {
                if !state.config.objects[instance.object].persistent {
                    Some(instance_id)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        for instance_id in room_end_instances {
            interpreter.enter(|ctx| -> Result<_, Error> {
                let instance = &state.instances[instance_id];
                if let Some(room_end_closure) = instance.event_closures.get(&ObjectEvent::RoomEnd)
                    && !instance.dead
                {
                    let room_end_closure = ctx.fetch(room_end_closure);
                    let instance_ud = ctx.fetch(&instance.this);
                    let object_id = instance.object;
                    FreezeMany::new()
                        .freeze(State::ctx_cell(ctx), state)
                        .freeze(
                            EventState::ctx_cell(ctx),
                            &EventState {
                                instance_id,
                                object_id,
                                current_event: ObjectEvent::RoomEnd,
                            },
                        )
                        .freeze(InputState::ctx_cell(ctx), input_state)
                        .in_scope(|| {
                            ctx.fetch(thread).run_with(
                                ctx,
                                room_end_closure,
                                instance_ud,
                                vm::Value::Undefined,
                            )
                        })?;
                }

                Ok(())
            })?;
        }

        for instance_id in remove_instances {
            interpreter.enter(|ctx| -> Result<_, Error> {
                let instance = &state.instances[instance_id];

                if let Some(clean_up_closure) = instance.event_closures.get(&ObjectEvent::CleanUp)
                    && !instance.dead
                {
                    let clean_up_closure = ctx.fetch(clean_up_closure);
                    let instance_ud = ctx.fetch(&instance.this);
                    let object_id = instance.object;
                    FreezeMany::new()
                        .freeze(State::ctx_cell(ctx), state)
                        .freeze(
                            EventState::ctx_cell(ctx),
                            &EventState {
                                instance_id,
                                object_id,
                                current_event: ObjectEvent::RoomEnd,
                            },
                        )
                        .freeze(InputState::ctx_cell(ctx), input_state)
                        .in_scope(|| {
                            ctx.fetch(thread).run_with(
                                ctx,
                                clean_up_closure,
                                instance_ud,
                                vm::Value::Undefined,
                            )
                        })?;
                }

                Ok(())
            })?;

            state.instances[instance_id].dead = true;
        }
        state.instances.retain(|_, instance| !instance.dead);

        // Clean up removed instances in the instance maps.

        state
            .instance_for_template
            .retain(|_, id| state.instances.contains(*id));

        for set in state.instances_for_object.values_mut() {
            set.retain(|&id| state.instances.contains(id));
        }
        state.instances_for_object.retain(|_, set| !set.is_empty());

        for set in state.instances_for_layer.values_mut() {
            set.retain(|&id| state.instances.contains(id));
        }
        state.instances_for_layer.retain(|_, set| !set.is_empty());

        // Clean up all empty layers. Persistent objects may keep their layer alive.

        let mut used_layers = IndexSet::new();
        for layer_id in state.instances_for_layer.ids() {
            used_layers.insert(layer_id.index() as usize);
        }

        state
            .layers
            .retain(|layer_id, _| used_layers.contains(layer_id.index() as usize));

        state
            .named_layers
            .retain(|_, layer_id| used_layers.contains(layer_id.index() as usize));

        // Clean up all orphaned tile maps.

        let mut used_tile_maps = IndexSet::new();
        for layer in state.layers.values() {
            if let Some(tile_map_id) = layer.tile_map {
                used_tile_maps.insert(tile_map_id.index() as usize);
            }
        }

        state
            .tile_maps
            .retain(|tile_map_id, _| used_tile_maps.contains(tile_map_id.index() as usize));

        // Create all new layers for the next room, reusing the existing layer if there is one with
        // the same name. This "moves" persistent objects to matching named layers on room change.

        for layer in state.config.rooms[next_room].layers.values() {
            if let Some(&existing_layer_id) = state.named_layers.get(&layer.name) {
                let existing_layer = &mut state.layers[existing_layer_id];
                existing_layer.depth = layer.depth;
                existing_layer.visible = layer.visible;
            } else {
                let tile_map = if let RoomLayerType::Tile(tile_layer) = &layer.layer_type {
                    Some(state.tile_maps.insert_with_id(|id| TileMap {
                        this: interpreter.enter(|ctx| ctx.stash(TileMapUserData::new(ctx, id))),
                        position: tile_layer.position,
                        tile_set: tile_layer.tile_set,
                        grid_dimensions: tile_layer.grid_dimensions,
                        grid: tile_layer.grid.clone(),
                    }))
                } else {
                    None
                };

                let layer_id = state.layers.insert_with_id(|id| {
                    let this = interpreter.enter(|ctx| {
                        ctx.stash(LayerIdUserData::new(ctx, id, Some(ctx.intern(&layer.name))))
                    });
                    Layer {
                        this,
                        depth: layer.depth,
                        visible: layer.visible,
                        tile_map,
                    }
                });

                state.named_layers.insert(layer.name.clone(), layer_id);
            }
        }

        // Switch to the next room, creating all new instances (other than persistent instances
        // which already exist).

        state.current_room = Some(next_room);
        let next_room_data = state.config.rooms[next_room].clone();

        for layer in next_room_data.layers.values() {
            let layer_id = state.named_layers[&layer.name];
            if let RoomLayerType::Instances(template_ids) = &layer.layer_type {
                for &template_id in template_ids {
                    interpreter.enter(|ctx| -> Result<_, Error> {
                        let instance_template = state.config.instance_templates[template_id];

                        if state.config.objects[instance_template.object].persistent {
                            if state.instance_for_template.contains(template_id) {
                                return Ok(());
                            }
                        }

                        let event_closures = state.event_closures(instance_template.object);

                        let instance_id = state.instances.insert_with_id(|instance_id| Instance {
                            this: ctx.stash(InstanceUserData::new(ctx, instance_id)),
                            object: instance_template.object,
                            active: true,
                            dead: false,
                            position: instance_template.position,
                            rotation: 0.0,
                            layer: layer_id,
                            properties: ctx.stash(vm::Object::new(&ctx)),
                            event_closures,
                            animation_time: 0.0,
                        });

                        assert!(
                            state
                                .instance_for_template
                                .insert(template_id, instance_id)
                                .is_none()
                        );
                        assert!(
                            state
                                .instances_for_object
                                .get_or_insert_default(instance_template.object)
                                .insert(instance_id)
                        );
                        assert!(
                            state
                                .instances_for_layer
                                .get_or_insert_default(layer_id)
                                .insert(instance_id)
                        );

                        let instance = &mut state.instances[instance_id];

                        if let Some(create_closure) =
                            instance.event_closures.get(&ObjectEvent::Create)
                        {
                            let thread = ctx.fetch(thread);
                            let this = ctx.fetch(&instance.this);
                            let closure = ctx.fetch(create_closure);
                            let object_id = instance.object;

                            FreezeMany::new()
                                .freeze(State::ctx_cell(ctx), state)
                                .freeze(
                                    EventState::ctx_cell(ctx),
                                    &EventState {
                                        instance_id,
                                        object_id,
                                        current_event: ObjectEvent::Create,
                                    },
                                )
                                .freeze(InputState::ctx_cell(ctx), input_state)
                                .in_scope(|| {
                                    thread.run_with(ctx, closure, this, vm::Value::Undefined)
                                })?;
                        }

                        Ok(())
                    })?;
                }
            }
        }

        let room_start_instances = state
            .instances
            .iter()
            .filter_map(|(instance_id, instance)| {
                if !instance.dead {
                    Some(instance_id)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        for instance_id in room_start_instances {
            interpreter.enter(|ctx| -> Result<_, Error> {
                let instance = &state.instances[instance_id];
                if let Some(room_start_closure) =
                    instance.event_closures.get(&ObjectEvent::RoomStart)
                    && !instance.dead
                {
                    let room_start_closure = ctx.fetch(room_start_closure);
                    let instance_ud = ctx.fetch(&instance.this);
                    let object_id = instance.object;

                    FreezeMany::new()
                        .freeze(State::ctx_cell(ctx), state)
                        .freeze(
                            EventState::ctx_cell(ctx),
                            &EventState {
                                instance_id,
                                object_id,
                                current_event: ObjectEvent::RoomStart,
                            },
                        )
                        .freeze(InputState::ctx_cell(ctx), input_state)
                        .in_scope(|| {
                            ctx.fetch(thread).run_with(
                                ctx,
                                room_start_closure,
                                instance_ud,
                                vm::Value::Undefined,
                            )
                        })?;
                }

                Ok(())
            })?;
        }
    }

    let to_update = state
        .instances
        .iter()
        .filter_map(|(id, instance)| if instance.active { Some(id) } else { None })
        .collect::<Vec<_>>();

    let instance_bounds = to_update
        .iter()
        .filter_map(|&instance_id| {
            Some((
                state.instance_collision(instance_id)?.bound_box(),
                instance_id,
            ))
        })
        .collect::<Vec<_>>();

    state.instance_bound_tree.clear();
    state.instance_bound_tree.fextend(instance_bounds);

    for &instance_id in &to_update {
        let object_id = state.instances[instance_id].object;

        interpreter.enter(|ctx| -> Result<(), Error> {
            if let Some(step_closure) = state.instances[instance_id]
                .event_closures
                .get(&ObjectEvent::BeginStep)
            {
                let thread = ctx.fetch(thread);
                let closure = ctx.fetch(step_closure);
                let this = ctx.fetch(&state.instances[instance_id].this);

                FreezeMany::new()
                    .freeze(State::ctx_cell(ctx), state)
                    .freeze(
                        EventState::ctx_cell(ctx),
                        &EventState {
                            instance_id,
                            object_id,
                            current_event: ObjectEvent::BeginStep,
                        },
                    )
                    .freeze(InputState::ctx_cell(ctx), input_state)
                    .in_scope(|| thread.run_with(ctx, closure, this, vm::Value::Undefined))?;
            }
            Ok(())
        })?;

        interpreter.enter(|ctx| -> Result<(), Error> {
            if let Some(step_closure) = state.instances[instance_id]
                .event_closures
                .get(&ObjectEvent::Step)
            {
                let thread = ctx.fetch(thread);
                let closure = ctx.fetch(step_closure);
                let this = ctx.fetch(&state.instances[instance_id].this);

                FreezeMany::new()
                    .freeze(State::ctx_cell(ctx), state)
                    .freeze(
                        EventState::ctx_cell(ctx),
                        &EventState {
                            instance_id,
                            object_id,
                            current_event: ObjectEvent::Step,
                        },
                    )
                    .freeze(InputState::ctx_cell(ctx), input_state)
                    .in_scope(|| thread.run_with(ctx, closure, this, vm::Value::Undefined))?;
            }
            Ok(())
        })?;

        interpreter.enter(|ctx| -> Result<(), Error> {
            if let Some(step_closure) = state.instances[instance_id]
                .event_closures
                .get(&ObjectEvent::EndStep)
            {
                let thread = ctx.fetch(thread);
                let closure = ctx.fetch(step_closure);
                let this = ctx.fetch(&state.instances[instance_id].this);

                FreezeMany::new()
                    .freeze(State::ctx_cell(ctx), state)
                    .freeze(
                        EventState::ctx_cell(ctx),
                        &EventState {
                            instance_id,
                            object_id,
                            current_event: ObjectEvent::EndStep,
                        },
                    )
                    .freeze(InputState::ctx_cell(ctx), input_state)
                    .in_scope(|| thread.run_with(ctx, closure, this, vm::Value::Undefined))?;
            }
            Ok(())
        })?;

        interpreter.enter(|ctx| -> Result<(), Error> {
            if let Some(draw_closure) = state.instances[instance_id]
                .event_closures
                .get(&ObjectEvent::Draw)
            {
                let thread = ctx.fetch(thread);
                let closure = ctx.fetch(draw_closure);
                let this = ctx.fetch(&state.instances[instance_id].this);

                FreezeMany::new()
                    .freeze(State::ctx_cell(ctx), state)
                    .freeze(
                        EventState::ctx_cell(ctx),
                        &EventState {
                            instance_id,
                            object_id,
                            current_event: ObjectEvent::Draw,
                        },
                    )
                    .freeze(InputState::ctx_cell(ctx), input_state)
                    .freeze(DrawingState::ctx_cell(ctx), drawing_state)
                    .in_scope(|| thread.run_with(ctx, closure, this, vm::Value::Undefined))?;
            }
            Ok(())
        })?;

        let instance = &mut state.instances[instance_id];

        let object = &state.config.objects[instance.object];
        if let Some(sprite_id) = object.sprite {
            let sprite = &state.config.sprites[sprite_id];

            instance.animation_time = (instance.animation_time
                + sprite.playback_speed / state.config.tick_rate)
                % sprite.playback_length;
        }
    }

    state.instances.retain(|_, instance| !instance.dead);

    Ok(())
}
