use std::hint::black_box;

use criterion::{criterion_group, criterion_main, Criterion};
use fabricator::{
    bytecode::ByteCode,
    closure::{Closure, Prototype},
    constant::Constant,
    instructions::Instruction,
    thread::Thread,
    value::Value,
};
use gc_arena::{Arena, Gc, Rootable};

pub fn criterion_benchmark(c: &mut Criterion) {
    struct Root<'gc> {
        proto: Gc<'gc, Prototype<'gc>>,
        thread: Thread<'gc>,
    }

    let mut arena: Arena<Rootable![Root<'_>]> = Arena::new(|mc| {
        let proto = Prototype {
            fixed_params: 0,
            constants: vec![
                Constant::Integer(0),
                Constant::Integer(1),
                Constant::Integer(10000000),
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
                Instruction::TestLessEqual {
                    arg1: 4,
                    arg2: 2,
                    dest: 5,
                },
                Instruction::JumpIf {
                    arg: 5,
                    is_true: true,
                    offset: -3,
                },
                Instruction::Push { source: 3, len: 1 },
                Instruction::Return { returns: 1 },
            ])
            .unwrap(),
        };

        Root {
            proto: Gc::new(mc, proto),
            thread: Thread::default(),
        }
    });

    c.bench_function("sum loop", move |b| {
        b.iter(|| {
            let arena = black_box(&mut arena);
            arena.mutate_root(|mc, root| {
                let closure = Closure::new(root.proto);
                assert_eq!(
                    root.thread.exec(mc, closure).unwrap()[0],
                    Value::Integer(50000005000000)
                );
            });
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
