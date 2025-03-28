use fabricator::{compiler::ir, constant::Constant, value::String};
use gc_arena::arena;

#[test]
fn test_ir_codegen() {
    arena::rootless_mutate(|_mc| {
        let mut func = ir::Function::<String<'_>>::default();

        let var_i = func.heap_vars.insert(());
        let var_sum = func.heap_vars.insert(());

        let start_block_id = func.blocks.insert(ir::Block::default());
        let loop_block_id = func.blocks.insert(ir::Block::default());
        let end_block_id = func.blocks.insert(ir::Block::default());

        let start_block = func.blocks.get_mut(start_block_id).unwrap();

        let const_0 = func
            .instructions
            .insert(ir::Instruction::Constant(Constant::Integer(0)));
        start_block.instructions.push(const_0);

        let const_1 = func
            .instructions
            .insert(ir::Instruction::Constant(Constant::Integer(1)));
        start_block.instructions.push(const_1);

        let const_100000 = func
            .instructions
            .insert(ir::Instruction::Constant(Constant::Integer(100000)));
        start_block.instructions.push(const_100000);

        start_block.exit = ir::Exit::Jump(loop_block_id);

        let loop_block = func.blocks.get_mut(loop_block_id).unwrap();

        let sum = func
            .instructions
            .insert(ir::Instruction::GetVariable(var_sum));
        loop_block.instructions.push(const_100000);

        let i = func
            .instructions
            .insert(ir::Instruction::GetVariable(var_i));
        loop_block.instructions.push(i);

        let i_plus_one = func.instructions.insert(ir::Instruction::BinOp {
            left: i,
            right: const_1,
            op: ir::BinOp::Add,
        });
        loop_block.instructions.push(i_plus_one);

        let sum_plus_i = func.instructions.insert(ir::Instruction::BinOp {
            left: sum,
            right: i,
            op: ir::BinOp::Add,
        });
        loop_block.instructions.push(sum_plus_i);

        loop_block
            .instructions
            .push(func.instructions.insert(ir::Instruction::SetVariable {
                source: sum_plus_i,
                dest: var_sum,
            }));

        loop_block
            .instructions
            .push(func.instructions.insert(ir::Instruction::SetVariable {
                source: i_plus_one,
                dest: var_i,
            }));

        let i_le_100000 = func.instructions.insert(ir::Instruction::BinComp {
            left: i,
            right: const_100000,
            comp: ir::BinComp::LessEqual,
        });
        loop_block.instructions.push(i_le_100000);

        loop_block.exit = ir::Exit::Branch {
            cond: i_le_100000,
            if_true: loop_block_id,
            if_false: end_block_id,
        };

        let end_block = func.blocks.get_mut(end_block_id).unwrap();

        end_block.instructions.push(
            func.instructions
                .insert(ir::Instruction::Push { source: sum_plus_i }),
        );

        end_block.exit = ir::Exit::Return { returns: 1 };
    });
}
