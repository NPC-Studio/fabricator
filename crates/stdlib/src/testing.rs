use fabricator_vm::{
    self as vm,
    magic::{MagicConstant, MagicSet},
};

pub fn testing_lib<'gc>(ctx: vm::Context<'gc>, lib: &mut MagicSet<'gc>) {
    let assert = vm::Callback::from_fn(&ctx, |_, mut exec| {
        let stack = exec.stack();
        for i in 0..stack.len() {
            if !stack.get(i).to_bool() {
                return Err("assert failed".into());
            }
        }
        Ok(())
    });
    lib.insert(
        ctx.intern("assert"),
        MagicConstant::new_ptr(&ctx, assert.into()),
    );

    let print = vm::Callback::from_fn(&ctx, |_, mut exec| {
        let stack = exec.stack();
        for i in 0..stack.len() {
            print!("{:?}", stack.get(i));
            if i != stack.len() - 1 {
                print!("\t");
            }
        }
        println!();
        Ok(())
    });
    lib.insert(
        ctx.intern("print"),
        MagicConstant::new_ptr(&ctx, print.into()),
    );

    let black_box = vm::Callback::from_fn(&ctx, |_, _| Ok(()));
    lib.insert(
        ctx.intern("black_box"),
        MagicConstant::new_ptr(&ctx, black_box.into()),
    );
}
