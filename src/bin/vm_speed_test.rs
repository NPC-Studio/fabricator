use std::time::Instant;

use fabricator::{
    constant::Constant,
    ops,
    prototype::{Prototype, StackSize},
    thread::Thread,
};

fn main() {
    gc_arena::arena::rootless_mutate(|mc| {
        let prototype = Prototype {
            fixed_params: 0,
            stack_size: StackSize::new(5),
            constants: vec![
                Constant::Integer(0),
                Constant::Integer(1),
                Constant::Integer(1000000),
            ]
            .into_boxed_slice(),
            instructions: vec![
                ops::Instruction::encode(ops::Load {
                    constant: ops::Constant(0),
                    dest: ops::Register(0),
                }),
                ops::Instruction::encode(ops::Load {
                    constant: ops::Constant(1),
                    dest: ops::Register(1),
                }),
                ops::Instruction::encode(ops::Load {
                    constant: ops::Constant(2),
                    dest: ops::Register(2),
                }),
                ops::Instruction::encode(ops::Move {
                    source: ops::Register(0),
                    dest: ops::Register(3),
                }),
                ops::Instruction::encode(ops::Move {
                    source: ops::Register(1),
                    dest: ops::Register(4),
                }),
                ops::Instruction::encode(ops::Add {
                    arg1: ops::Register(3),
                    arg2: ops::Register(4),
                    dest: ops::Register(3),
                }),
                ops::Instruction::encode(ops::Add {
                    arg1: ops::Register(4),
                    arg2: ops::Register(1),
                    dest: ops::Register(4),
                }),
                ops::Instruction::encode(ops::IsLessEqual {
                    skip_if: false,
                    arg1: ops::Register(4),
                    arg2: ops::Register(2),
                }),
                ops::Instruction::encode(ops::Jump { offset: -4 }),
                ops::Instruction::encode(ops::Push {
                    source: ops::Register(3),
                    len: 1,
                }),
                ops::Instruction::encode(ops::Return { returns: 1 }),
            ]
            .into_boxed_slice(),
        };

        let mut thread = Thread::default();

        let instant = Instant::now();
        let res = thread.exec(mc, &prototype).unwrap();
        let elapsed = instant.elapsed();

        println!("result: {:?}, total time: {elapsed:?}", res[0]);
    })
}
