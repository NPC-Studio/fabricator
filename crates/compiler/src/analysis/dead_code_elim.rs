use std::collections::HashMap;

use either::Either;
use fabricator_util::{index_containers::IndexSet, typed_id_map::SecondaryMap};

use crate::{
    graph::{Node, dfs::topological_order, dominators::Dominators, predecessors::Predecessors},
    ir,
};

pub fn eliminate_dead_code<S>(ir: &mut ir::Function<S>) {
    // Dead code elimination algorithm from Cytron et al. (1991)
    // https://bears.ece.ucsb.edu/class/ece253/papers/cytron91.pdf

    let predecessors = Predecessors::compute(ir.blocks.ids(), |b| ir.blocks[b].exit.successors());

    let exit_blocks = ir
        .blocks
        .iter()
        .filter_map(|(block_id, block)| {
            if block.exit.exits_function() {
                Some(block_id)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    // Normally blocks don't have a shared "exit" block, so in order to calculate the post-dominator
    // tree, we need to create an exit node that is a successor to every block that returns.
    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    enum RevNode {
        Exit,
        Node(ir::BlockId),
    }

    impl Node for RevNode {
        fn index(&self) -> usize {
            match self {
                RevNode::Exit => 0,
                RevNode::Node(block_id) => block_id.index() as usize + 1,
            }
        }
    }

    let post_dominators = Dominators::compute(RevNode::Exit, |node| match node {
        RevNode::Exit => Either::Left(exit_blocks.iter().copied().map(RevNode::Node)),
        RevNode::Node(block_id) => Either::Right(predecessors.get(block_id).map(RevNode::Node)),
    });

    let reachable_blocks = topological_order(ir.start_block, |b| ir.blocks[b].exit.successors());

    let mut inst_blocks = SecondaryMap::new();
    for &block_id in &reachable_blocks {
        let block = &ir.blocks[block_id];
        for &inst_id in &block.instructions {
            inst_blocks.insert(inst_id, block_id);
        }
    }

    let mut live_instructions = IndexSet::new();
    let mut live_branches = IndexSet::new();

    enum Work {
        Instruction(ir::InstId),
        Branch(ir::BlockId),
    }

    let mut worklist = Vec::new();
    let mut upsilon_instructions: HashMap<ir::ShadowVar, Vec<ir::InstId>> = HashMap::new();

    // First, do two things:
    //
    // 1) For every instruction with an effect that is not an `Upsilon`, mark it as live.
    // 2) For each `Upsilon` instruction, add it and its source to the `upsilon_instructions` map.
    //    When we encounter a live `Phi` instruction, every instruction in this map for that shadow
    //    variable will become live. This way, an `Upsilon` and its sources are only live when the
    //    `Phi` is live.
    for (inst_id, _) in inst_blocks.iter() {
        let inst = &ir.instructions[inst_id];
        match inst {
            &ir::Instruction::Upsilon(shadow_var, source) => {
                upsilon_instructions
                    .entry(shadow_var)
                    .or_default()
                    .extend([inst_id, source]);
            }
            inst if inst.has_effect() => {
                live_instructions.insert(inst_id.index() as usize);
                worklist.push(Work::Instruction(inst_id));
            }
            _ => {}
        }
    }

    // Any parameter of `Exit::Return` or `Exit::Throw` is always live.
    for &block_id in &reachable_blocks {
        match ir.blocks[block_id].exit {
            ir::Exit::Return { value: Some(value) } | ir::Exit::Throw(value) => {
                live_instructions.insert(value.index() as usize);
                worklist.push(Work::Instruction(value));
            }
            _ => {}
        }
    }

    while let Some(work) = worklist.pop() {
        match work {
            Work::Instruction(inst_id) => {
                match &ir.instructions[inst_id] {
                    &ir::Instruction::Phi(shadow_var) => {
                        for &inst_id in upsilon_instructions.get(&shadow_var).into_iter().flatten()
                        {
                            if live_instructions.insert(inst_id.index() as usize) {
                                worklist.push(Work::Instruction(inst_id));
                            }
                        }
                    }
                    inst => {
                        for source in inst.sources() {
                            if live_instructions.insert(source.index() as usize) {
                                worklist.push(Work::Instruction(source));
                            }
                        }
                    }
                }

                for node in post_dominators
                    .dominance_frontier(RevNode::Node(inst_blocks[inst_id]))
                    .unwrap()
                {
                    let RevNode::Node(block_id) = node else {
                        unreachable!()
                    };
                    if live_branches.insert(block_id.index() as usize) {
                        worklist.push(Work::Branch(block_id));
                    }
                }
            }
            Work::Branch(block_id) => {
                // The `cond` parameter of `Exit::Branch` is only live if there is a live
                // instruction that is control-flow dependent on this branch.
                let block = &ir.blocks[block_id];
                match block.exit {
                    ir::Exit::Branch { cond, .. } => {
                        if live_instructions.insert(cond.index() as usize) {
                            worklist.push(Work::Instruction(cond));
                        }
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

    for (block_id, block) in ir.blocks.iter_mut() {
        for &inst_id in &block.instructions {
            if !live_instructions.contains(inst_id.index() as usize) {
                ir.instructions[inst_id] = ir::Instruction::NoOp;
            }
        }

        match block.exit {
            ir::Exit::Return { .. } | ir::Exit::Throw(_) => {}
            ir::Exit::Jump(_) => {}
            ir::Exit::Branch { if_false, .. } => {
                if !live_branches.contains(block_id.index() as usize) {
                    // If this is not a branch that any live instruction is control-flow dependent
                    // on, then nothing in either successive branch is live (but there may be live
                    // instructions in future blocks).
                    //
                    // In this case, it doesn't matter which branch we take, so just replace it with
                    // a jump to one of them.
                    block.exit = ir::Exit::Jump(if_false);
                }
            }
        }
    }
}
