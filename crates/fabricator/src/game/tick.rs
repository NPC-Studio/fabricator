use anyhow::Error;
use fabricator_collision::support_ext::SupportMapExt as _;
use fabricator_util::freeze::FreezeMany;
use fabricator_vm as vm;

use crate::{
    api::instance::InstanceUserData,
    project::ObjectEvent,
    state::{DrawingState, InputState, Instance, InstanceState, State},
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
                    FreezeMany::new()
                        .freeze(InputState::ctx_cell(ctx), input_state)
                        .freeze(State::ctx_cell(ctx), state)
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
                    FreezeMany::new()
                        .freeze(InputState::ctx_cell(ctx), input_state)
                        .freeze(State::ctx_cell(ctx), state)
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

        state.current_room = Some(next_room);

        for layer in state.config.rooms[state.current_room.unwrap()]
            .layers
            .clone()
            .into_values()
        {
            for &template_id in &layer.instances {
                interpreter.enter(|ctx| -> Result<_, Error> {
                    let instance_template = state.config.instance_templates[template_id];

                    if state.config.objects[instance_template.object].persistent {
                        if state.persistent_instances.contains(&template_id) {
                            return Ok(());
                        }

                        state.persistent_instances.insert(template_id);
                    }

                    let instance_id = state.instances.insert_with_id(|instance_id| Instance {
                        object: instance_template.object,
                        active: true,
                        dead: false,
                        position: instance_template.position,
                        rotation: 0.0,
                        depth: layer.depth,
                        this: ctx.stash(InstanceUserData::new(ctx, instance_id)),
                        properties: ctx.stash(vm::Object::new(&ctx)),
                        event_closures: state.scripts.event_closures(ctx, instance_template.object),
                        animation_time: 0.0,
                    });

                    let instance = &mut state.instances[instance_id];

                    if let Some(create_closure) = instance.event_closures.get(&ObjectEvent::Create)
                    {
                        let thread = ctx.fetch(thread);
                        let this = ctx.fetch(&instance.this);
                        let closure = ctx.fetch(create_closure);

                        FreezeMany::new()
                            .freeze(State::ctx_cell(ctx), state)
                            .freeze(InputState::ctx_cell(ctx), input_state)
                            .freeze(InstanceState::ctx_cell(ctx), &InstanceState { instance_id })
                            .in_scope(|| {
                                thread.run_with(ctx, closure, this, vm::Value::Undefined)
                            })?;
                    }

                    Ok(())
                })?;
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
                    FreezeMany::new()
                        .freeze(InputState::ctx_cell(ctx), input_state)
                        .freeze(State::ctx_cell(ctx), state)
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
                    .freeze(InputState::ctx_cell(ctx), input_state)
                    .freeze(InstanceState::ctx_cell(ctx), &InstanceState { instance_id })
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
                    .freeze(InputState::ctx_cell(ctx), input_state)
                    .freeze(InstanceState::ctx_cell(ctx), &InstanceState { instance_id })
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
                    .freeze(InputState::ctx_cell(ctx), input_state)
                    .freeze(InstanceState::ctx_cell(ctx), &InstanceState { instance_id })
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
                    .freeze(InputState::ctx_cell(ctx), input_state)
                    .freeze(InstanceState::ctx_cell(ctx), &InstanceState { instance_id })
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
