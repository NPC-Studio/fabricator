use fabricator_vm as vm;
use gc_arena::{Collect, Gc, Rootable};

pub trait StdlibContext<'gc> {
    fn stdlib(self) -> Gc<'gc, vm::MagicSet<'gc>>;

    fn testing_stdlib(self) -> Gc<'gc, vm::MagicSet<'gc>>;
}

impl<'gc> StdlibContext<'gc> for vm::Context<'gc> {
    fn stdlib(self) -> Gc<'gc, fabricator_vm::MagicSet<'gc>> {
        #[derive(Collect)]
        #[collect(no_drop)]
        struct StdlibSingleton<'gc>(Gc<'gc, vm::MagicSet<'gc>>);

        impl<'gc> vm::Singleton<'gc> for StdlibSingleton<'gc> {
            fn create(ctx: fabricator_vm::Context<'gc>) -> Self {
                let mut stdlib = vm::MagicSet::new();
                let method = vm::Callback::from_fn(&ctx, |ctx, _, mut stack| {
                    let Some(func) = stack.get(1).to_function() else {
                        return Err("`method` must be called on a callback or closure".into());
                    };

                    match stack.get(0) {
                        obj @ (vm::Value::Undefined
                        | vm::Value::Object(_)
                        | vm::Value::UserData(_)) => {
                            stack.clear();
                            stack.push_back(func.rebind(&ctx, obj).into());
                            Ok(())
                        }
                        _ => Err(
                            "`method` self value must be an object, userdata, or undefined".into(),
                        ),
                    }
                });
                stdlib
                    .add_constant(&ctx, vm::String::new(&ctx, "method"), method.into())
                    .unwrap();
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
            fn create(ctx: fabricator_vm::Context<'gc>) -> Self {
                let mut testing_stdlib = vm::MagicSet::new();
                let assert = vm::Callback::from_fn(&ctx, |_, _, stack| {
                    for i in 0..stack.len() {
                        if !stack.get(i).to_bool() {
                            return Err("assert failed".into());
                        }
                    }
                    Ok(())
                });
                testing_stdlib
                    .add_constant(&ctx, vm::String::new(&ctx, "assert"), assert.into())
                    .unwrap();

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
                testing_stdlib
                    .add_constant(&ctx, vm::String::new(&ctx, "print"), print.into())
                    .unwrap();

                let black_box = vm::Callback::from_fn(&ctx, |_, _, _| Ok(()));
                testing_stdlib
                    .add_constant(&ctx, vm::String::new(&ctx, "black_box"), black_box.into())
                    .unwrap();

                let stdlib = ctx.stdlib();
                let combined = stdlib.merge(&testing_stdlib).unwrap();

                Self(Gc::new(&ctx, combined))
            }
        }

        self.registry()
            .singleton::<Rootable![TestingStdlibSingleton<'_>]>(self)
            .0
    }
}
