use std::collections::HashMap;

use anyhow::Error;
use fabricator_collision::support_ext::SupportMapExt as _;
use fabricator_util::freeze::FreezeMany;
use fabricator_vm as vm;

use crate::{
    api::instance::create_instance_ud,
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
        state
            .instances
            .retain(|_, instance| state.config.objects[instance.object].persistent);

        state.current_room = Some(next_room);

        let dummy_ud = interpreter.enter(|ctx| ctx.stash(vm::UserData::new_static(&ctx, ())));

        for layer in state.config.rooms[state.current_room.unwrap()]
            .layers
            .clone()
            .into_values()
        {
            for &template_id in &layer.instances {
                interpreter
                    .enter(|ctx| -> Result<_, vm::Error> {
                        let instance_template = &state.config.instance_templates[template_id];

                        let instance_id = state.instances.insert(Instance {
                            object: instance_template.object,
                            position: instance_template.position,
                            rotation: 0.0,
                            depth: layer.depth,
                            this: dummy_ud.clone(),
                            properties: ctx.stash(vm::Object::new(&ctx)),
                            event_closures: HashMap::new(),
                            animation_time: 0.0,
                        });

                        state.instances[instance_id].this =
                            ctx.stash(create_instance_ud(ctx, instance_id));

                        if state.config.objects[instance_template.object].persistent {
                            if state.persistent_instances.contains(&template_id) {
                                return Ok(());
                            }

                            state.persistent_instances.insert(template_id);
                        }

                        for (&event, script) in state
                            .scripts
                            .object_events
                            .get(&instance_template.object)
                            .into_iter()
                            .flatten()
                        {
                            if event != ObjectEvent::Create {
                                state.instances[instance_id]
                                    .event_closures
                                    .insert(event, ctx.stash(script.create_closure(ctx)));
                            }
                        }

                        if let Some(create_script) = state
                            .scripts
                            .object_events
                            .get(&instance_template.object)
                            .and_then(|evs| evs.get(&ObjectEvent::Create))
                            .cloned()
                        {
                            let thread = ctx.fetch(thread);
                            let this = ctx.fetch(&state.instances[instance_id].this);

                            FreezeMany::new()
                                .freeze(State::ctx_cell(ctx), state)
                                .freeze(InputState::ctx_cell(ctx), input_state)
                                .freeze(
                                    InstanceState::ctx_cell(ctx),
                                    &InstanceState { instance_id },
                                )
                                .in_scope(|| {
                                    thread.exec_with(
                                        ctx,
                                        create_script.create_closure(ctx),
                                        this.into(),
                                        this.into(),
                                    )
                                })?;
                        }

                        Ok(())
                    })
                    .map_err(|e| e.into_inner())?;
            }
        }
    }

    let to_update = state.instances.ids().collect::<Vec<_>>();

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
                .get(&ObjectEvent::Step)
            {
                let thread = ctx.fetch(thread);
                let closure = ctx.fetch(step_closure);
                let this = ctx.fetch(&state.instances[instance_id].this);

                FreezeMany::new()
                    .freeze(State::ctx_cell(ctx), state)
                    .freeze(InputState::ctx_cell(ctx), input_state)
                    .freeze(InstanceState::ctx_cell(ctx), &InstanceState { instance_id })
                    .in_scope(|| {
                        thread
                            .exec_with(ctx, closure, this.into(), this.into())
                            .map_err(|e| e.into_inner())
                    })?;
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
                    .in_scope(|| {
                        thread
                            .exec_with(ctx, closure, this.into(), this.into())
                            .map_err(|e| e.into_inner())
                    })?;
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

    Ok(())
}
