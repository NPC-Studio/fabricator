use std::collections::HashMap;

use either::Either;
use fabricator_util::{
    index_containers::{IndexMap, IndexSet},
    typed_id_map::SecondaryMap,
};

use crate::{
    graph::{
        Node,
        dfs::{depth_first_search, topological_order},
        dominators::Dominators,
        predecessors::Predecessors,
    },
    ir,
};

pub fn eliminate_dead_code<S>(ir: &mut ir::Function<S>) {
    // Dead code elimination algorithm from Cytron et al. (1991)
    // https://bears.ece.ucsb.edu/class/ece253/papers/cytron91.pdf

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

    let predecessors = Predecessors::compute(ir.blocks.ids(), |b| ir.blocks[b].exit.successors());

    let mut exit_blocks = ir
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

    fn rev_successors(
        node: RevNode,
        predecessors: &Predecessors<ir::BlockId>,
        exit_blocks: impl IntoIterator<Item = ir::BlockId>,
    ) -> impl Iterator<Item = RevNode> {
        match node {
            RevNode::Exit => Either::Left(exit_blocks.into_iter().map(RevNode::Node)),
            RevNode::Node(block_id) => Either::Right(predecessors.get(block_id).map(RevNode::Node)),
        }
    }

    // Unlike in Cytron et al., we have two types of instructions: regular instructions and branch
    // instructions. Our worklist must allow for both.

    let mut live_instructions = IndexSet::new();
    let mut live_branches = IndexSet::new();

    enum Work {
        Instruction(ir::InstId),
        Branch(ir::BlockId),
    }

    let mut worklist = Vec::new();

    // Cytron et al. does not actually cover how to handle potentially infinite loops in dead code
    // elimination.
    //
    // We check for any back edges in the CFG and treat all of them as potentially an infinite
    // loop. Since an infinite loop is an effect, we mark all of these blocks with back-edges as
    // having live branches.
    //
    // Additionally, we may have blocks that are *unreachable* from our synthetic exit node due to
    // being in an infinite loop with no exit. For these blocks, any block with a back-edge is added
    // to `exit_blocks`, to ensure that every block is reachable from the synthetic exit node.

    let mut reverse_reachable_blocks = IndexSet::new();
    depth_first_search(
        RevNode::Exit,
        |node| {
            if let RevNode::Node(n) = node {
                reverse_reachable_blocks.insert(n.index() as usize);
            }
            rev_successors(node, &predecessors, exit_blocks.iter().copied())
        },
        |_| {},
    );

    let reachable_blocks = topological_order(ir.start_block, |b| ir.blocks[b].exit.successors());
    let topological_ordering = reachable_blocks
        .iter()
        .copied()
        .enumerate()
        .map(|(i, block_id)| (block_id.index() as usize, i))
        .collect::<IndexMap<_>>();

    for &block_id in &reachable_blocks {
        let topological_number = topological_ordering[block_id.index() as usize];
        let has_back_edge = ir.blocks[block_id].exit.successors().any(|successor| {
            topological_ordering[successor.index() as usize] <= topological_number
        });

        if has_back_edge {
            // A back-edge is a potentially infinite loop, which we should consider as an effect.
            live_branches.insert(block_id.index() as usize);
            worklist.push(Work::Branch(block_id));

            // For every unreachable block with a back-edge, add a link to the synthetic exit. This
            // makes every block reachable from the synthetic exit and gives us *some* information
            // about control dependencies within infinite loops.
            //
            // This is what major compilers do, see: https://reviews.llvm.org/D29705
            if !reverse_reachable_blocks.contains(block_id.index() as usize) {
                exit_blocks.push(block_id);
            }
        }
    }

    // Map every instruction to its containing block.
    let mut inst_blocks = SecondaryMap::new();
    for &block_id in &reachable_blocks {
        let block = &ir.blocks[block_id];
        for &inst_id in &block.instructions {
            inst_blocks.insert(inst_id, block_id);
        }
    }

    // We will need the post-dominance frontier to determine control-flow dependence.
    let post_dominators = Dominators::compute(RevNode::Exit, |node| {
        rev_successors(node, &predecessors, exit_blocks.iter().copied())
    });

    // We need to add all live instructions to the work queue. First do two things...
    //
    // 1) For every instruction with an effect that is not an `Upsilon`, mark it as live.
    // 2) For each `Upsilon` instruction, add it and its source to the `upsilon_instructions` map.
    //    When we encounter a live `Phi` instruction, every instruction in this map for that shadow
    //    variable will become live. This way, an `Upsilon` and its sources are only live when the
    //    `Phi` is live.
    let mut upsilon_instructions: HashMap<ir::ShadowVar, Vec<ir::InstId>> = HashMap::new();
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

    // Additionally, any parameter of `Exit::Return` or `Exit::Throw` is always live.
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
        let live_block;
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

                live_block = inst_blocks[inst_id];
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
                    _ => {
                        // This node must have a back-edge, which can make it a live branch without
                        // being an actual branch.
                    }
                }

                live_block = block_id;
            }
        }

        // All blocks in the post-dominance-frontier of this block must have branch which this block
        // is control-flow dependent on.
        for node in post_dominators
            .dominance_frontier(RevNode::Node(live_block))
            .unwrap()
        {
            let RevNode::Node(frontier_block) = node else {
                unreachable!()
            };
            if live_branches.insert(frontier_block.index() as usize) {
                worklist.push(Work::Branch(frontier_block));
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
