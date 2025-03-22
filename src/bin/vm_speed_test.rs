use std::time::Instant;

use fabricator::{
    bytecode::ByteCode,
    closure::{Closure, Prototype},
    constant::Constant,
    instructions::Instruction,
    thread::Thread,
};

fn main() {
    gc_arena::arena::rootless_mutate(|mc| {
        let prototype = Prototype {
            fixed_params: 0,
            max_register: 4,
            constants: vec![
                Constant::Integer(0),
                Constant::Integer(1),
                Constant::Integer(1000000),
            ]
            .into_boxed_slice(),
            bytecode: ByteCode::encode(&[
                Instruction::Load {
                    constant: 0,
                    dest: 0,
                },
                Instruction::Load {
                    constant: 1,
                    dest: 1,
                },
                Instruction::Load {
                    constant: 2,
                    dest: 2,
                },
                Instruction::Move { source: 0, dest: 3 },
                Instruction::Move { source: 1, dest: 4 },
                Instruction::Add {
                    arg1: 3,
                    arg2: 4,
                    dest: 3,
                },
                Instruction::Add {
                    arg1: 4,
                    arg2: 1,
                    dest: 4,
                },
                Instruction::JumpIfLessEqual {
                    arg1: 4,
                    arg2: 2,
                    offset: -2,
                },
                Instruction::Push { source: 3, len: 1 },
                Instruction::Return { returns: 1 },
            ])
            .unwrap(),
        };

        let closure = Closure::new(mc, prototype);

        let mut thread = Thread::default();

        let instant = Instant::now();
        let res = thread.exec(mc, closure).unwrap();
        let elapsed = instant.elapsed();

        println!("result: {:?}, total time: {elapsed:?}", res[0]);
    })
}
