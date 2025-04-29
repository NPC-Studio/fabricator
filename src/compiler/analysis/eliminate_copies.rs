use std::collections::HashMap;

use crate::compiler::{graph::dfs::topological_order, ir};

pub fn eliminate_copies<S>(ir: &mut ir::Function<S>) {
    // Map from copy instructions to their sources.
    let mut copies: HashMap<ir::InstId, ir::InstId> = HashMap::new();

    let reachable_blocks = topological_order(ir.start_block, |b| ir.blocks[b].exit.successors());

    // Since every instruction is in SSA form and every use must be dominated by a definition, it
    // should be enough to do this in one pass, as long as we iterate in topological order.
    for &block_id in &reachable_blocks {
        let block = &mut ir.blocks[block_id];
        for &inst_id in &block.instructions {
            let inst = &mut ir.instructions[inst_id];
            if let &ir::Instruction::Copy(source) = &*inst {
                // We do this from the top down, so for well-formed IR (sources dominate their
                // use), any `Copy` instruction used as a source should have its real source in the
                // `copies` map already. Therefore by induction this will always get the real source
                // (for well-formed IR).
                let real_source = if let Some(&indirect_source) = copies.get(&source) {
                    indirect_source
                } else {
                    source
                };
                // Assert that what we just said is true and the source is not a previously
                // encountered `Copy` instruction.
                assert!(!copies.contains_key(&real_source));

                copies.insert(inst_id, real_source);

                // We're removing every copy, so the existing copy instruction should be unused.
                *inst = ir::Instruction::NoOp;
            }

            for source in inst.sources_mut() {
                if let Some(&real_source) = copies.get(&*source) {
                    *source = real_source;
                }
            }
        }

        for source in block.exit.sources_mut() {
            if let Some(&real_source) = copies.get(&*source) {
                *source = real_source;
            }
        }
    }
}
