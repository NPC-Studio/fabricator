use std::f64;

use fabricator_vm::{self as vm, magic::MagicConstant};
use gc_arena::{Collect, Gc, Rootable};

pub trait StdlibContext<'gc> {
    fn stdlib(self) -> Gc<'gc, vm::MagicSet<'gc>>;

    fn testing_stdlib(self) -> Gc<'gc, vm::MagicSet<'gc>>;
}

impl<'gc> StdlibContext<'gc> for vm::Context<'gc> {
    fn stdlib(self) -> Gc<'gc, vm::MagicSet<'gc>> {
        #[derive(Collect)]
        #[collect(no_drop)]
        struct StdlibSingleton<'gc>(Gc<'gc, vm::MagicSet<'gc>>);

        impl<'gc> vm::Singleton<'gc> for StdlibSingleton<'gc> {
            fn create(ctx: vm::Context<'gc>) -> Self {
                let mut stdlib = vm::MagicSet::new();

                stdlib.insert(
                    ctx.intern("global"),
                    MagicConstant::new_ptr(&ctx, ctx.globals().into()),
                );

                let method = vm::Callback::from_fn(&ctx, |ctx, _, mut stack| {
                    let Some(func) = stack.get(1).to_function() else {
                        return Err(vm::Error::msg(
                            "`method` must be called on a callback or closure",
                        ));
                    };

                    match stack.get(0) {
                        obj @ (vm::Value::Undefined
                        | vm::Value::Object(_)
                        | vm::Value::UserData(_)) => {
                            stack.clear();
                            stack.push_back(func.rebind(&ctx, obj).into());
                            Ok(())
                        }
                        _ => Err(vm::Error::msg(
                            "`method` self value must be an object, userdata, or undefined",
                        )),
                    }
                });
                stdlib.insert(
                    ctx.intern("method"),
                    MagicConstant::new_ptr(&ctx, method.into()),
                );

                stdlib.insert(
                    ctx.intern("pi"),
                    MagicConstant::new_ptr(&ctx, f64::consts::PI.into()),
                );

                let cos = vm::Callback::from_fn(&ctx, |ctx, _, mut stack| {
                    let arg: f64 = stack.consume(ctx)?;
                    stack.replace(ctx, arg.cos());
                    Ok(())
                });
                stdlib.insert(ctx.intern("cos"), MagicConstant::new_ptr(&ctx, cos.into()));

                let sin = vm::Callback::from_fn(&ctx, |ctx, _, mut stack| {
                    let arg: f64 = stack.consume(ctx)?;
                    stack.replace(ctx, arg.sin());
                    Ok(())
                });
                stdlib.insert(ctx.intern("sin"), MagicConstant::new_ptr(&ctx, sin.into()));

                let abs = vm::Callback::from_fn(&ctx, |ctx, _, mut stack| {
                    let arg: f64 = stack.consume(ctx)?;
                    stack.replace(ctx, arg.abs());
                    Ok(())
                });
                stdlib.insert(ctx.intern("abs"), MagicConstant::new_ptr(&ctx, abs.into()));

                Self(Gc::new(&ctx, stdlib))
            }
        }

        self.registry()
            .singleton::<Rootable![StdlibSingleton<'_>]>(self)
            .0
    }

    fn testing_stdlib(self) -> Gc<'gc, vm::MagicSet<'gc>> {
        #[derive(Collect)]
        #[collect(no_drop)]
        struct TestingStdlibSingleton<'gc>(Gc<'gc, vm::MagicSet<'gc>>);

        impl<'gc> vm::Singleton<'gc> for TestingStdlibSingleton<'gc> {
            fn create(ctx: vm::Context<'gc>) -> Self {
                let mut testing_stdlib = vm::MagicSet::new();
                let assert = vm::Callback::from_fn(&ctx, |_, _, stack| {
                    for i in 0..stack.len() {
                        if !stack.get(i).to_bool() {
                            return Err(vm::Error::msg("assert failed"));
                        }
                    }
                    Ok(())
                });
                testing_stdlib.insert(
                    ctx.intern("assert"),
                    MagicConstant::new_ptr(&ctx, assert.into()),
                );

                let print = vm::Callback::from_fn(&ctx, |_, _, stack| {
                    for i in 0..stack.len() {
                        print!("{:?}", stack.get(i));
                        if i != stack.len() - 1 {
                            print!("\t");
                        }
                    }
                    println!();
                    Ok(())
                });
                testing_stdlib.insert(
                    ctx.intern("print"),
                    MagicConstant::new_ptr(&ctx, print.into()),
                );

                let black_box = vm::Callback::from_fn(&ctx, |_, _, _| Ok(()));
                testing_stdlib.insert(
                    ctx.intern("black_box"),
                    MagicConstant::new_ptr(&ctx, black_box.into()),
                );

                let mut combined = vm::MagicSet::new();
                combined.merge(&ctx.stdlib());
                combined.merge(&testing_stdlib);

                Self(Gc::new(&ctx, combined))
            }
        }

        self.registry()
            .singleton::<Rootable![TestingStdlibSingleton<'_>]>(self)
            .0
    }
}
