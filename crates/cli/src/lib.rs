use anyhow::Error;
use fabricator_compiler as compiler;
use fabricator_stdlib::StdlibContext as _;
use fabricator_vm as vm;
use gc_arena::{Collect, Gc, Rootable};

pub trait TestingStdlibContext<'gc> {
    /// The stdlib with some additional test methods.
    fn testing_stdlib(self) -> Gc<'gc, vm::MagicSet<'gc>>;
}

impl<'gc> TestingStdlibContext<'gc> for vm::Context<'gc> {
    fn testing_stdlib(self) -> Gc<'gc, vm::MagicSet<'gc>> {
        #[derive(Collect)]
        #[collect(no_drop)]
        struct TestingStdlibSingleton<'gc>(Gc<'gc, vm::MagicSet<'gc>>);

        impl<'gc> vm::Singleton<'gc> for TestingStdlibSingleton<'gc> {
            fn create(ctx: vm::Context<'gc>) -> Self {
                let mut lib = vm::MagicSet::new();
                lib.merge(&ctx.stdlib());

                let assert = vm::Callback::from_fn(&ctx, |_, mut exec| {
                    let stack = exec.stack();
                    for i in 0..stack.len() {
                        if !stack.get(i).cast_bool() {
                            return Err(vm::RuntimeError::msg("assert failed"));
                        }
                    }
                    Ok(())
                });
                lib.insert(
                    ctx.intern("assert"),
                    vm::magic::MagicConstant::new_ptr(&ctx, assert),
                );

                let black_box = vm::Callback::from_fn(&ctx, |_, _| Ok(()));
                lib.insert(
                    ctx.intern("black_box"),
                    vm::magic::MagicConstant::new_ptr(&ctx, black_box),
                );

                TestingStdlibSingleton(Gc::new(&ctx, lib))
            }
        }

        self.singleton::<Rootable![TestingStdlibSingleton<'_>]>().0
    }
}

pub fn run_code(
    name: &str,
    code: &str,
    compile_settings: compiler::CompileSettings,
) -> Result<bool, Error> {
    let interpreter = vm::Interpreter::new();

    interpreter.enter(|ctx| {
        let output = compiler::Compiler::compile_chunk(
            ctx,
            "default",
            compiler::ImportItems::with_magic(&ctx, ctx.testing_stdlib()),
            compile_settings,
            name,
            code,
        )?;
        let closure = vm::Closure::new(&ctx, output.chunk_prototype, vm::Value::Undefined).unwrap();

        let thread = vm::Thread::new(&ctx);
        thread.exec(ctx, |mut exec| {
            exec.call(ctx, closure)?;
            Ok(exec.stack().get(0) == vm::Value::Boolean(true))
        })
    })
}
