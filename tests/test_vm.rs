use fabricator::{
    bytecode::ByteCode,
    closure::{Closure, Prototype},
    constant::Constant,
    instructions::Instruction,
    thread::Thread,
    value::Value,
};
use gc_arena::{arena, Gc};

#[test]
fn test_vm_sum_loop() {
    arena::rootless_mutate(|mc| {
        let proto = Gc::new(
            mc,
            Prototype {
                constants: vec![
                    Constant::Integer(0),
                    Constant::Integer(1),
                    Constant::Integer(100000),
                ]
                .into_boxed_slice(),
                bytecode: ByteCode::encode(&[
                    Instruction::LoadConstant {
                        constant: 0,
                        dest: 0,
                    },
                    Instruction::LoadConstant {
                        constant: 1,
                        dest: 1,
                    },
                    Instruction::LoadConstant {
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
                used_registers: 6,
                used_heap: 0,
            },
        );

        let mut thread = Thread::default();

        let closure = Closure::new(mc, proto);

        assert_eq!(
            thread.exec(mc, closure).unwrap()[0],
            Value::Integer(5000050000)
        );
    });
}

#[test]
fn test_vm_sum_loop_with_heap() {
    arena::rootless_mutate(|mc| {
        let proto = Gc::new(
            mc,
            Prototype {
                constants: vec![
                    Constant::Integer(0),
                    Constant::Integer(1),
                    Constant::Integer(100000),
                ]
                .into_boxed_slice(),
                bytecode: ByteCode::encode(&[
                    Instruction::LoadConstant {
                        constant: 0,
                        dest: 0,
                    },
                    Instruction::LoadConstant {
                        constant: 1,
                        dest: 1,
                    },
                    Instruction::LoadConstant {
                        constant: 2,
                        dest: 2,
                    },
                    Instruction::SetHeap { source: 0, heap: 0 },
                    Instruction::Move { source: 1, dest: 3 },
                    Instruction::GetHeap { heap: 0, dest: 4 },
                    Instruction::Add {
                        arg1: 3,
                        arg2: 4,
                        dest: 4,
                    },
                    Instruction::SetHeap { source: 4, heap: 0 },
                    Instruction::Add {
                        arg1: 3,
                        arg2: 1,
                        dest: 3,
                    },
                    Instruction::TestLessEqual {
                        arg1: 3,
                        arg2: 2,
                        dest: 4,
                    },
                    Instruction::JumpIf {
                        arg: 4,
                        is_true: true,
                        offset: -5,
                    },
                    Instruction::GetHeap { heap: 0, dest: 3 },
                    Instruction::Push { source: 3, len: 1 },
                    Instruction::Return { returns: 1 },
                ])
                .unwrap(),
                used_registers: 5,
                used_heap: 1,
            },
        );

        let mut thread = Thread::default();

        let closure = Closure::new(mc, proto);

        assert_eq!(
            thread.exec(mc, closure).unwrap()[0],
            Value::Integer(5000050000)
        );
    });
}
