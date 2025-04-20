use crate::compiler::{constant::Constant, graph::dfs::topological_order, ir};

pub fn fold_constants<S: Clone>(ir: &mut ir::Function<S>) {
    let reachable_blocks = topological_order(ir.start_block, |b| ir.blocks[b].exit.successors());

    // Since every instruction is in SSA form and in well-formed IR every use must be dominated by a
    // definition, iterating in topological order should fold everything possible in one pass.
    for &block_id in &reachable_blocks {
        let block = &mut ir.blocks[block_id];

        for &inst_id in &block.instructions {
            let get_constant = |inst_id| {
                if let ir::Instruction::Constant(c) = ir.instructions[inst_id].clone() {
                    Some(c)
                } else {
                    None
                }
            };

            let mut new_inst = None;
            match ir.instructions[inst_id].clone() {
                ir::Instruction::Copy(source) => {
                    new_inst = get_constant(source).map(ir::Instruction::Constant);
                }
                ir::Instruction::UnOp { source, op } => {
                    if let Some(c) = get_constant(source) {
                        new_inst = Some(ir::Instruction::Constant(match op {
                            ir::UnOp::Not => Constant::Boolean(!c.to_bool()),
                        }));
                    }
                }
                ir::Instruction::BinOp { left, right, op } => {
                    if let (Some(l), Some(r)) = (get_constant(left), get_constant(right)) {
                        new_inst = match op {
                            ir::BinOp::Add => l.add(r),
                            ir::BinOp::Sub => l.sub(r),
                        }
                        .map(ir::Instruction::Constant)
                    }
                }
                ir::Instruction::BinComp { left, right, comp } => {
                    if let (Some(l), Some(r)) = (get_constant(left), get_constant(right)) {
                        let res = match comp {
                            ir::BinComp::LessThan => l.less_than(r),
                            ir::BinComp::LessEqual => l.less_equal(r),
                            ir::BinComp::Equal => l.equal(r),
                            ir::BinComp::NotEqual => l.equal(r).map(|b| !b),
                            ir::BinComp::GreaterThan => r.less_equal(l),
                            ir::BinComp::GreaterEqual => r.less_than(l),
                        };
                        new_inst = res.map(|b| ir::Instruction::Constant(Constant::Boolean(b)));
                    }
                }
                ir::Instruction::GetField { object, key } => {
                    if let Some(key) = get_constant(key) {
                        new_inst = Some(ir::Instruction::GetFieldConst { object, key })
                    }
                }
                ir::Instruction::SetField { object, key, value } => {
                    if let Some(key) = get_constant(key) {
                        new_inst = Some(ir::Instruction::SetFieldConst { object, key, value })
                    }
                }
                _ => {}
            }

            if let Some(new_inst) = new_inst {
                ir.instructions[inst_id] = new_inst;
            }
        }

        match block.exit {
            ir::Exit::Return { .. } => {}
            ir::Exit::Jump(_) => {}
            ir::Exit::Branch {
                cond,
                if_false,
                if_true,
            } => {
                if let ir::Instruction::Constant(c) = ir.instructions[cond].clone() {
                    let target = if c.to_bool() { if_true } else { if_false };
                    block.exit = ir::Exit::Jump(target);
                }
            }
        }
    }
}
