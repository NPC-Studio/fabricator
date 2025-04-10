use crate::compiler::{constant::Constant, graph::dfs::topological_order, ir};

pub fn fold_constants<S: Clone>(ir: &mut ir::Function<S>) {
    let reachable_blocks =
        topological_order(ir.start_block, |b| ir.parts.blocks[b].exit.successors());

    // Since every instruction is in SSA form and in well-formed IR every use must be dominated by a
    // definition, iterating in topological order should fold everything possible in one pass.
    for &block_id in &reachable_blocks {
        let block = &mut ir.parts.blocks[block_id];

        for &inst_id in &block.instructions {
            let get_constant = |inst_id| {
                if let ir::Instruction::Constant(c) = ir.parts.instructions[inst_id].clone() {
                    Some(c)
                } else {
                    None
                }
            };

            let mut const_val = None;
            match ir.parts.instructions[inst_id].clone() {
                ir::Instruction::Copy(source) => {
                    const_val = get_constant(source);
                }
                ir::Instruction::UnOp { source, op } => {
                    if let Some(c) = get_constant(source) {
                        const_val = Some(match op {
                            ir::UnOp::Not => Constant::Boolean(!c.to_bool()),
                        });
                    }
                }
                ir::Instruction::BinOp { left, right, op } => {
                    if let (Some(l), Some(r)) = (get_constant(left), get_constant(right)) {
                        match op {
                            ir::BinOp::Add => {
                                const_val = l.add(r);
                            }
                            ir::BinOp::Sub => {
                                const_val = l.sub(r);
                            }
                        }
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
                        const_val = res.map(Constant::Boolean);
                    }
                }
                _ => {}
            }

            if let Some(const_val) = const_val {
                ir.parts.instructions[inst_id] = ir::Instruction::Constant(const_val);
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
                if let ir::Instruction::Constant(c) = ir.parts.instructions[cond].clone() {
                    let target = if c.to_bool() { if_true } else { if_false };
                    block.exit = ir::Exit::Jump(target);
                }
            }
        }
    }
}
