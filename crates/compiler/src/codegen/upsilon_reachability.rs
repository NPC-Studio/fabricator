use std::collections::{HashMap, HashSet};

use fabricator_util::typed_id_map::SecondaryMap;

use crate::{analysis::shadow_liveness::ShadowLiveness, graph::dfs::depth_first_search, ir};

pub type UpsilonReachabilityMap = SecondaryMap<ir::ShadowVar, UpsilonReach>;

/// Analyze `ShadowLiveness` to determine, for each live region of the shadow variable, which
/// `Upsilon` instructions may have written to it.
///
/// Any `Upsilon` that could have affected the value of the shadow variable *since the last
/// execution of the `Phi` instruction* is considered to "reach" that region.
pub fn compute_upsilon_reachability<S>(
    ir: &ir::Function<S>,
    shadow_liveness: &ShadowLiveness,
) -> UpsilonReachabilityMap {
    let mut reach_map = UpsilonReachabilityMap::default();

    for shadow_var in shadow_liveness.live_shadow_vars() {
        let mut live_blocks = HashSet::new();
        let mut live_upsilons = Vec::new();
        let mut phi_block = None;

        for (block_id, liveness) in shadow_liveness.live_ranges(shadow_var) {
            live_blocks.insert(block_id);

            if let Some(incoming) = liveness.incoming_range {
                if let Some(upsilon) = incoming.start {
                    live_upsilons.push((block_id, upsilon));
                }
                phi_block = Some(block_id);
            }

            if let Some(outgoing) = liveness.outgoing_range {
                if let Some(upsilon) = outgoing.start {
                    live_upsilons.push((block_id, upsilon));
                }
            }
        }

        let phi_block = phi_block.unwrap();

        let mut outgoing_reach: HashMap<ir::BlockId, Vec<(ir::BlockId, usize)>> =
            HashMap::from_iter(live_blocks.iter().map(|&block_id| (block_id, Vec::new())));

        for &(upsilon_block_id, upsilon_index) in &live_upsilons {
            depth_first_search(
                upsilon_block_id,
                |block_id| {
                    outgoing_reach
                        .get_mut(&block_id)
                        .unwrap()
                        .push((upsilon_block_id, upsilon_index));

                    // We only need to traverse down parts of the CFG that are live for this shadow
                    // variable.
                    //
                    // We always stop *before* the phi block, because we don't need to mark the
                    // incoming range reachability (since it's always the full set of live upsilons
                    // by definition).
                    //
                    // In the case where there is also an overlapping outgoing range in the phi
                    // block that contains an `Upsilon` instruction, we start iterating on that
                    // block so it won't be skipped.
                    ir.blocks[block_id].exit.successors().filter(|&block_id| {
                        live_blocks.contains(&block_id) && block_id != phi_block
                    })
                },
                |_| {},
            );
        }

        reach_map.insert(
            shadow_var,
            UpsilonReach {
                live_upsilons,
                outgoing_reach,
            },
        );
    }

    reach_map
}

#[derive(Debug)]
pub struct UpsilonReach {
    /// Every live `Upsilon` instruction.
    ///
    /// The reach for the single incoming range for the shadow variable is always every live
    /// `Upsilon` instruction, by definition.
    pub live_upsilons: Vec<(ir::BlockId, usize)>,

    /// If there is an outgoing range for the shadow variable in the block key, the `HashMap` will
    /// contain every `Upsilon` instruction that may have written to the variable in this region
    /// since the last execution of the `Phi` instruction.
    pub outgoing_reach: HashMap<ir::BlockId, Vec<(ir::BlockId, usize)>>,
}

#[cfg(test)]
mod tests {
    use crate::constant::Constant;

    use super::*;

    #[test]
    fn test_upsilon_reach() {
        let mut instructions = ir::InstructionMap::<&'static str>::new();
        let mut blocks = ir::BlockMap::new();
        let mut shadow_vars = ir::ShadowVarSet::new();

        let shadow_var = shadow_vars.insert(());

        let block_a_id = blocks.insert(ir::Block::default());
        let block_b_id = blocks.insert(ir::Block::default());

        let block_a = &mut blocks[block_a_id];

        let one = instructions.insert(ir::Instruction::Constant(Constant::Integer(1)));
        block_a.instructions.push(one);
        block_a
            .instructions
            .push(instructions.insert(ir::Instruction::Upsilon(shadow_var, one)));

        block_a.exit = ir::Exit::Jump(block_b_id);

        let block_b = &mut blocks[block_b_id];

        block_b
            .instructions
            .push(instructions.insert(ir::Instruction::Phi(shadow_var)));

        let two = instructions.insert(ir::Instruction::Constant(Constant::Integer(2)));
        block_b.instructions.push(two);
        block_b
            .instructions
            .push(instructions.insert(ir::Instruction::Upsilon(shadow_var, two)));

        block_b.exit = ir::Exit::Jump(block_b_id);

        let ir = ir::Function {
            num_parameters: 0,
            instructions,
            blocks,
            variables: Default::default(),
            shadow_vars,
            functions: Default::default(),
            upvalues: Default::default(),
            start_block: block_a_id,
        };

        let shadow_liveness = ShadowLiveness::compute(&ir).unwrap();
        let upsilon_reach = compute_upsilon_reachability(&ir, &shadow_liveness);

        assert!(
            shadow_liveness
                .live_range_in_block(block_b_id, shadow_var)
                .unwrap()
                .incoming_range
                .is_some()
        );
        assert_eq!(
            upsilon_reach[shadow_var].outgoing_reach[&block_a_id],
            [(block_a_id, 1)]
        );
        assert_eq!(
            upsilon_reach[shadow_var].outgoing_reach[&block_b_id],
            [(block_b_id, 2)]
        );
    }

    #[test]
    fn test_upsilon_reach_overlap() {
        let mut instructions = ir::InstructionMap::<&'static str>::new();
        let mut blocks = ir::BlockMap::new();
        let mut shadow_vars = ir::ShadowVarSet::new();

        let shadow_var = shadow_vars.insert(());

        let block_a_id = blocks.insert(ir::Block::default());
        let block_b_id = blocks.insert(ir::Block::default());

        let block_a = &mut blocks[block_a_id];

        let one = instructions.insert(ir::Instruction::Constant(Constant::Integer(1)));
        block_a.instructions.push(one);
        block_a
            .instructions
            .push(instructions.insert(ir::Instruction::Upsilon(shadow_var, one)));

        block_a.exit = ir::Exit::Jump(block_b_id);

        let block_b = &mut blocks[block_b_id];

        block_b
            .instructions
            .push(instructions.insert(ir::Instruction::Phi(shadow_var)));

        block_b.exit = ir::Exit::Jump(block_b_id);

        let ir = ir::Function {
            num_parameters: 0,
            instructions,
            blocks,
            variables: Default::default(),
            shadow_vars,
            functions: Default::default(),
            upvalues: Default::default(),
            start_block: block_a_id,
        };

        let shadow_liveness = ShadowLiveness::compute(&ir).unwrap();
        let upsilon_reach = compute_upsilon_reachability(&ir, &shadow_liveness);

        assert!(
            shadow_liveness
                .live_range_in_block(block_b_id, shadow_var)
                .unwrap()
                .incoming_range
                .is_some()
        );
        assert_eq!(
            upsilon_reach[shadow_var].outgoing_reach[&block_a_id],
            [(block_a_id, 1)]
        );
        assert!(
            shadow_liveness
                .live_range_in_block(block_b_id, shadow_var)
                .unwrap()
                .outgoing_range
                .unwrap()
                .start
                .is_none()
        );
        assert_eq!(upsilon_reach[shadow_var].outgoing_reach[&block_b_id], []);
    }
}
