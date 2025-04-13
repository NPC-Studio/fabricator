use fabricator::{
    closure::Closure,
    compiler::{codegen::codegen, constant::Constant, ir},
    object::Object,
    string::String,
    thread::Thread,
    value::Value,
};
use gc_arena::{arena, Gc};

#[test]
fn test_ir_codegen() {
    arena::rootless_mutate(|mc| {
        let mut parts = ir::FunctionParts::<String<'_>>::default();

        let var_sum = parts.variables.insert(());
        let var_i = parts.variables.insert(());

        let start_block_id = parts.blocks.insert(ir::Block::default());
        let loop_block_id = parts.blocks.insert(ir::Block::default());
        let end_block_id = parts.blocks.insert(ir::Block::default());

        let start_block = parts.blocks.get_mut(start_block_id).unwrap();

        let const_0 = parts
            .instructions
            .insert(ir::Instruction::Constant(Constant::Integer(0)));
        start_block.instructions.push(const_0);

        let const_1 = parts
            .instructions
            .insert(ir::Instruction::Constant(Constant::Integer(1)));
        start_block.instructions.push(const_1);

        let const_100000 = parts
            .instructions
            .insert(ir::Instruction::Constant(Constant::Integer(100000)));
        start_block.instructions.push(const_100000);

        start_block.instructions.push(
            parts
                .instructions
                .insert(ir::Instruction::SetVariable(var_sum, const_0)),
        );

        start_block.instructions.push(
            parts
                .instructions
                .insert(ir::Instruction::SetVariable(var_i, const_1)),
        );

        start_block.exit = ir::Exit::Jump(loop_block_id);

        let loop_block = parts.blocks.get_mut(loop_block_id).unwrap();

        let sum = parts
            .instructions
            .insert(ir::Instruction::GetVariable(var_sum));
        loop_block.instructions.push(sum);

        let i = parts
            .instructions
            .insert(ir::Instruction::GetVariable(var_i));
        loop_block.instructions.push(i);

        let sum_plus_i = parts.instructions.insert(ir::Instruction::BinOp {
            left: sum,
            right: i,
            op: ir::BinOp::Add,
        });
        loop_block.instructions.push(sum_plus_i);

        let i_plus_one = parts.instructions.insert(ir::Instruction::BinOp {
            left: i,
            right: const_1,
            op: ir::BinOp::Add,
        });
        loop_block.instructions.push(i_plus_one);

        loop_block.instructions.push(
            parts
                .instructions
                .insert(ir::Instruction::SetVariable(var_sum, sum_plus_i)),
        );

        loop_block.instructions.push(
            parts
                .instructions
                .insert(ir::Instruction::SetVariable(var_i, i_plus_one)),
        );

        let i_le_100000 = parts.instructions.insert(ir::Instruction::BinComp {
            left: i_plus_one,
            right: const_100000,
            comp: ir::BinComp::LessEqual,
        });
        loop_block.instructions.push(i_le_100000);

        loop_block.exit = ir::Exit::Branch {
            cond: i_le_100000,
            if_true: loop_block_id,
            if_false: end_block_id,
        };

        let end_block = parts.blocks.get_mut(end_block_id).unwrap();

        end_block
            .instructions
            .push(parts.instructions.insert(ir::Instruction::Push(sum_plus_i)));

        end_block.exit = ir::Exit::Return { returns: 1 };

        let function = ir::Function {
            parts,
            start_block: start_block_id,
        };

        let prototype = Gc::new(mc, codegen(function).unwrap());
        let closure = Closure::new(mc, prototype, Object::new(mc));

        let thread = Thread::new(mc);
        assert_eq!(
            thread.exec(mc, closure).unwrap()[0],
            Value::Integer(5000050000)
        );
    });
}

#[test]
fn test_ir_phi_upsilon() {
    arena::rootless_mutate(|mc| {
        let mut parts = ir::FunctionParts::<String<'_>>::default();

        let var_sum = parts.shadow_vars.insert(());
        let var_i = parts.shadow_vars.insert(());

        let start_block_id = parts.blocks.insert(ir::Block::default());
        let loop_block_id = parts.blocks.insert(ir::Block::default());
        let end_block_id = parts.blocks.insert(ir::Block::default());

        let start_block = parts.blocks.get_mut(start_block_id).unwrap();

        let const_0 = parts
            .instructions
            .insert(ir::Instruction::Constant(Constant::Integer(0)));
        start_block.instructions.push(const_0);

        let const_1 = parts
            .instructions
            .insert(ir::Instruction::Constant(Constant::Integer(1)));
        start_block.instructions.push(const_1);

        let const_100000 = parts
            .instructions
            .insert(ir::Instruction::Constant(Constant::Integer(100000)));
        start_block.instructions.push(const_100000);

        start_block.instructions.push(
            parts
                .instructions
                .insert(ir::Instruction::Upsilon(var_sum, const_0)),
        );

        start_block.instructions.push(
            parts
                .instructions
                .insert(ir::Instruction::Upsilon(var_i, const_1)),
        );

        start_block.exit = ir::Exit::Jump(loop_block_id);

        let loop_block = parts.blocks.get_mut(loop_block_id).unwrap();

        let sum = parts.instructions.insert(ir::Instruction::Phi(var_sum));
        loop_block.instructions.push(sum);

        let i = parts.instructions.insert(ir::Instruction::Phi(var_i));
        loop_block.instructions.push(i);

        let sum_plus_i = parts.instructions.insert(ir::Instruction::BinOp {
            left: sum,
            right: i,
            op: ir::BinOp::Add,
        });
        loop_block.instructions.push(sum_plus_i);

        let i_plus_one = parts.instructions.insert(ir::Instruction::BinOp {
            left: i,
            right: const_1,
            op: ir::BinOp::Add,
        });
        loop_block.instructions.push(i_plus_one);

        loop_block.instructions.push(
            parts
                .instructions
                .insert(ir::Instruction::Upsilon(var_sum, sum_plus_i)),
        );

        loop_block.instructions.push(
            parts
                .instructions
                .insert(ir::Instruction::Upsilon(var_i, i_plus_one)),
        );

        let i_le_100000 = parts.instructions.insert(ir::Instruction::BinComp {
            left: i_plus_one,
            right: const_100000,
            comp: ir::BinComp::LessEqual,
        });
        loop_block.instructions.push(i_le_100000);

        loop_block.exit = ir::Exit::Branch {
            cond: i_le_100000,
            if_true: loop_block_id,
            if_false: end_block_id,
        };

        let end_block = parts.blocks.get_mut(end_block_id).unwrap();

        end_block
            .instructions
            .push(parts.instructions.insert(ir::Instruction::Push(sum_plus_i)));

        end_block.exit = ir::Exit::Return { returns: 1 };

        let function = ir::Function {
            parts,
            start_block: start_block_id,
        };

        let prototype = Gc::new(mc, codegen(function).unwrap());
        let closure = Closure::new(mc, prototype, Object::new(mc));

        let thread = Thread::new(mc);
        assert_eq!(
            thread.exec(mc, closure).unwrap()[0],
            Value::Integer(5000050000)
        );
    });
}
