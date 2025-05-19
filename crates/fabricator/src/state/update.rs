use anyhow::Error;
use fabricator_vm as vm;

use crate::state::{Instance, ObjectEvent, State, instance::InstanceUserData};

impl State {
    pub fn tick(&mut self, interpreter: &mut vm::Interpreter) -> Result<(), Error> {
        if let Some(next_room) = self.next_room.take() {
            self.instances
                .retain(|_, instance| self.objects[instance.object].persistent);

            self.current_room = Some(next_room);

            let dummy_ud = interpreter.enter(|ctx| ctx.stash(vm::UserData::new_static(&ctx, ())));
            let magic = self.magic.clone();

            for layer in self.rooms[self.current_room.unwrap()]
                .layers
                .clone()
                .into_values()
            {
                for &template_id in &layer.instances {
                    interpreter
                        .enter(|ctx| -> Result<_, vm::Error> {
                            let instance_template = &self.instance_templates[template_id];

                            let instance_id = self.instances.insert(Instance {
                                object: instance_template.object,
                                position: instance_template.position,
                                depth: layer.depth,
                                this: dummy_ud.clone(),
                                step_closure: None,
                                animation_time: 0.0,
                            });

                            self.instances[instance_id].this =
                                ctx.stash(InstanceUserData::create(ctx, instance_id));

                            if self.objects[instance_template.object].persistent {
                                if self.persistent_instances.contains(&template_id) {
                                    return Ok(());
                                }

                                self.persistent_instances.insert(template_id);
                            }

                            if let Some(step_script) = self.objects[instance_template.object]
                                .event_scripts
                                .get(&ObjectEvent::Step)
                            {
                                let closure = ctx.stash(
                                    vm::Closure::new(
                                        &ctx,
                                        ctx.fetch(step_script),
                                        ctx.fetch(&magic),
                                        vm::Value::Undefined,
                                    )
                                    .unwrap(),
                                );
                                self.instances[instance_id].step_closure = Some(closure);
                            };

                            if let Some(create_script) = self.objects[instance_template.object]
                                .event_scripts
                                .get(&ObjectEvent::Create)
                                .cloned()
                            {
                                let thread = ctx.fetch(&self.main_thread);
                                let this = ctx.fetch(&self.instances[instance_id].this);

                                State::ctx_set(ctx, self, || {
                                    thread.exec_with(
                                        ctx,
                                        vm::Closure::new(
                                            &ctx,
                                            ctx.fetch(&create_script),
                                            ctx.fetch(&magic),
                                            vm::Value::Undefined,
                                        )
                                        .unwrap(),
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

        let to_update = self.instances.ids().collect::<Vec<_>>();

        for instance_id in to_update {
            interpreter.enter(|ctx| -> Result<(), Error> {
                if let Some(step_closure) = self.instances[instance_id].step_closure.as_ref() {
                    let thread = ctx.fetch(&self.main_thread);
                    let closure = ctx.fetch(step_closure);
                    let this = ctx.fetch(&self.instances[instance_id].this);

                    State::ctx_set(ctx, self, || {
                        thread
                            .exec_with(ctx, closure, this.into())
                            .map_err(|e| e.into_inner())
                    })?;
                }
                Ok(())
            })?;

            let instance = &mut self.instances[instance_id];

            let object = &self.objects[instance.object];
            if let Some(sprite_id) = object.sprite {
                let sprite = &self.sprites[sprite_id];

                instance.animation_time = (instance.animation_time
                    + sprite.playback_speed / self.tick_rate)
                    % sprite.playback_length;
            }
        }

        Ok(())
    }
}
