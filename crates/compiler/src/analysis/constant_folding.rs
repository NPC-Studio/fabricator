use crate::{constant::Constant, graph::dfs::topological_order, ir};

pub fn fold_constants<S: Eq + Clone>(ir: &mut ir::Function<S>) {
    let reachable_blocks = topological_order(ir.start_block, |b| ir.blocks[b].exit.successors());

    // Since every instruction is in SSA form and in well-formed IR every use must be dominated by a
    // definition, iterating in topological order should fold everything possible in one pass.
    for &block_id in &reachable_blocks {
        let block = &mut ir.blocks[block_id];

        for &inst_id in &block.instructions {
            let get_constant = |inst_id| {
                if let ir::Instruction::Constant(c) = &ir.instructions[inst_id] {
                    Some(c)
                } else {
                    None
                }
            };

            let mut new_inst = None;
            match ir.instructions[inst_id].clone() {
                ir::Instruction::Copy(source) => {
                    new_inst = get_constant(source).map(|c| ir::Instruction::Constant(c.clone()));
                }
                ir::Instruction::UnOp { op, source } => {
                    if let Some(c) = get_constant(source) {
                        new_inst = match op {
                            ir::UnOp::IsDefined => Some(Constant::Boolean(!c.is_undefined())),
                            ir::UnOp::IsUndefined => Some(Constant::Boolean(c.is_undefined())),
                            ir::UnOp::Test => Some(Constant::Boolean(!c.to_bool())),
                            ir::UnOp::Not => Some(Constant::Boolean(!c.to_bool())),
                            ir::UnOp::Negate => c.negate(),
                            ir::UnOp::Increment => c.add(&Constant::Integer(1)),
                            ir::UnOp::Decrement => c.sub(&Constant::Integer(1)),
                        }
                        .map(ir::Instruction::Constant);
                    }
                }
                ir::Instruction::BinOp { left, op, right } => {
                    let left_const = get_constant(left);
                    let right_const = get_constant(right);
                    if let (Some(l), Some(r)) = (left_const, right_const) {
                        new_inst = match op {
                            ir::BinOp::Add => l.add(r),
                            ir::BinOp::Sub => l.sub(r),
                            ir::BinOp::Mult => l.mult(r),
                            ir::BinOp::Div => l.div(r),
                            ir::BinOp::Rem => l.rem(r),
                            ir::BinOp::IDiv => l.idiv(r).map(Constant::Integer),
                            ir::BinOp::Equal => Some(Constant::Boolean(l.equal(r))),
                            ir::BinOp::NotEqual => Some(Constant::Boolean(!l.equal(r))),
                            ir::BinOp::LessThan => l.less_than(r).map(Constant::Boolean),
                            ir::BinOp::LessEqual => l.less_equal(r).map(Constant::Boolean),
                            ir::BinOp::GreaterThan => r.less_than(l).map(Constant::Boolean),
                            ir::BinOp::GreaterEqual => r.less_equal(l).map(Constant::Boolean),
                            ir::BinOp::And => Some(Constant::Boolean(l.to_bool() && r.to_bool())),
                            ir::BinOp::Or => Some(Constant::Boolean(l.to_bool() || r.to_bool())),
                            ir::BinOp::NullCoalesce => Some(if l.is_undefined() {
                                r.clone()
                            } else {
                                l.clone()
                            }),
                        }
                        .map(ir::Instruction::Constant);
                    } else if let Some((un_op, source)) = match op {
                        ir::BinOp::Add => {
                            if right_const.and_then(Constant::to_integer) == Some(1) {
                                Some((ir::UnOp::Increment, left))
                            } else if right_const.and_then(Constant::to_integer) == Some(-1) {
                                Some((ir::UnOp::Decrement, left))
                            } else {
                                None
                            }
                        }
                        ir::BinOp::Sub => {
                            if right_const.and_then(Constant::to_integer) == Some(1) {
                                Some((ir::UnOp::Decrement, left))
                            } else if right_const.and_then(Constant::to_integer) == Some(-1) {
                                Some((ir::UnOp::Increment, left))
                            } else {
                                None
                            }
                        }
                        ir::BinOp::Equal => match (left_const, right_const) {
                            (_, Some(Constant::Undefined)) => Some((ir::UnOp::IsUndefined, left)),
                            (_, Some(Constant::Boolean(true))) => Some((ir::UnOp::Test, left)),
                            (_, Some(Constant::Boolean(false))) => Some((ir::UnOp::Not, left)),
                            (Some(Constant::Undefined), _) => Some((ir::UnOp::IsUndefined, right)),
                            (Some(Constant::Boolean(true)), _) => Some((ir::UnOp::Test, right)),
                            (Some(Constant::Boolean(false)), _) => Some((ir::UnOp::Not, right)),
                            _ => None,
                        },
                        ir::BinOp::NotEqual => match (left_const, right_const) {
                            (_, Some(Constant::Undefined)) => Some((ir::UnOp::IsDefined, left)),
                            (_, Some(Constant::Boolean(true))) => Some((ir::UnOp::Not, left)),
                            (_, Some(Constant::Boolean(false))) => Some((ir::UnOp::Test, left)),
                            (Some(Constant::Undefined), _) => Some((ir::UnOp::IsDefined, right)),
                            (Some(Constant::Boolean(true)), _) => Some((ir::UnOp::Not, right)),
                            (Some(Constant::Boolean(false)), _) => Some((ir::UnOp::Test, right)),
                            _ => None,
                        },
                        _ => None,
                    } {
                        new_inst = Some(ir::Instruction::UnOp { op: un_op, source });
                    } else if op == ir::BinOp::NullCoalesce {
                        if left_const.is_some_and(Constant::is_undefined) {
                            new_inst = Some(ir::Instruction::Copy(right));
                        }
                    }
                }
                ir::Instruction::GetField { object, key } => {
                    if let Some(key) = get_constant(key) {
                        new_inst = Some(ir::Instruction::GetFieldConst {
                            object,
                            key: key.clone(),
                        })
                    }
                }
                ir::Instruction::SetField { object, key, value } => {
                    if let Some(key) = get_constant(key) {
                        new_inst = Some(ir::Instruction::SetFieldConst {
                            object,
                            key: key.clone(),
                            value,
                        })
                    }
                }
                ir::Instruction::GetIndex { array, indexes } => {
                    if indexes.len() == 1 {
                        if let Some(index) = get_constant(indexes[0]) {
                            new_inst = Some(ir::Instruction::GetIndexConst {
                                array,
                                index: index.clone(),
                            })
                        }
                    }
                }
                ir::Instruction::SetIndex {
                    array,
                    indexes,
                    value,
                } => {
                    if indexes.len() == 1 {
                        if let Some(index) = get_constant(indexes[0]) {
                            new_inst = Some(ir::Instruction::SetIndexConst {
                                array,
                                index: index.clone(),
                                value,
                            })
                        }
                    }
                }
                _ => {}
            }

            if let Some(new_inst) = &mut new_inst {
                match *new_inst {
                    ir::Instruction::Constant(Constant::Undefined) => {
                        *new_inst = ir::Instruction::Undefined;
                    }
                    ir::Instruction::Boolean(b) => {
                        *new_inst = ir::Instruction::Boolean(b);
                    }
                    _ => {}
                }
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
