use std::collections::HashMap;

use arrayvec::ArrayVec;

use crate::{
    compiler::{
        analysis::{
            instruction_liveness::{InstructionLiveness, InstructionLivenessRange},
            shadow_liveness::{
                ShadowIncomingRange, ShadowLiveness, ShadowLivenessRange, ShadowOutgoingRange,
            },
        },
        graph::dfs::dfs_post_order,
        ir,
    },
    instructions::RegIdx,
    util::{bit_containers::BitSlice, typed_id_map::SecondaryMap},
};

#[derive(Debug)]
pub struct RegisterAllocation {
    pub instruction_registers: SecondaryMap<ir::InstId, RegIdx>,
    pub shadow_registers: SecondaryMap<ir::ShadowVarId, RegIdx>,
    pub used_registers: usize,
}

impl RegisterAllocation {
    /// Try and allocate registers for all instructions and shadow variables.
    ///
    /// Will try to coalesce registers for shadow variables and the registers for the `Phi` /
    /// `Upsilon` instructions that read and write to them.
    ///
    /// Returns `None` if no allocation could be found that fits in the available registers.
    pub fn allocate<S>(
        ir: &ir::Function<S>,
        instruction_liveness: &InstructionLiveness,
        shadow_liveness: &ShadowLiveness,
    ) -> Option<Self> {
        // First, we assign shadow variables and the instructions they can coalesce with via graph
        // coloring.
        //
        // We need to check every single shadow register to see if it conflicts with any other
        // shadow register or coalesced instruction register and color it differently if it does.
        // Also, we need to check potential coalescing instruction registers for conflicts here too,
        // and we potentially won't be able to coalesce if there are conflicts.
        //
        // NOTE:
        //
        // There are many potential improvements here...
        //
        // We're doing this coloring eagerly here because there is little risk of running out of
        // registers, but obviously better graph coloring algorithms exists.
        //
        // More impactful is the fact that we are coalescing registers eagerly based on no
        // heuristics at all, we could look for which registers have the most potential copies and
        // start with those. Also, we could be smarter and if not enough coalesce-able registers can
        // coalesce, pick a different, "more unique" register for the shadow variable. Both of these
        // really are extensions of the fact that we do register assignment completely eagerly.

        let mut coalesced_instruction_registers = SecondaryMap::<ir::InstId, RegIdx>::new();
        let mut assigned_shadow_registers = SecondaryMap::<ir::ShadowVarId, RegIdx>::new();

        for shadow_var_id in shadow_liveness.live_shadow_vars() {
            let mut interfering_registers = [0u8; 256];

            // For each live block for the given shadow variable, check interference with any
            // registers assigned to other shadow variables or instructions. If any such assigned
            // register interferes, set its bit in `interfering_registers`.

            for (block_id, shadow_range) in shadow_liveness.shadow_var_live_ranges(shadow_var_id) {
                let shadow_ranges = BlockRange::all_from_shadow_range(shadow_range);

                'next_var: for (other_shadow_var_id, &other_shadow_reg) in
                    assigned_shadow_registers.iter()
                {
                    if let Some(other_shadow_range) = shadow_liveness
                        .shadow_var_live_range_in_block(block_id, other_shadow_var_id)
                    {
                        for other_shadow_range in
                            BlockRange::all_from_shadow_range(other_shadow_range)
                        {
                            for shadow_range in shadow_ranges.clone() {
                                if shadow_range.interferes(other_shadow_range) {
                                    interfering_registers.set_bit(other_shadow_reg as usize, true);
                                    continue 'next_var;
                                }
                            }
                        }
                    }
                }

                'next_inst: for (inst_id, &inst_reg) in coalesced_instruction_registers.iter() {
                    if let Some(inst_range) =
                        instruction_liveness.instruction_live_range_in_block(block_id, inst_id)
                    {
                        let inst_range = BlockRange::from_instruction(inst_range);
                        for shadow_range in shadow_ranges.clone() {
                            if shadow_range.interferes(inst_range) {
                                interfering_registers.set_bit(inst_reg as usize, true);
                                continue 'next_inst;
                            }
                        }
                    }
                }
            }

            // Pick a register for this shadow variable

            let assigned_reg = interfering_registers
                .bit_iter()
                .enumerate()
                .find(|(_, conflicting)| !conflicting)?
                .0 as u8;
            assigned_shadow_registers.insert(shadow_var_id, assigned_reg);

            // Construct a list of instructions we would like to coalesce into this shadow variable
            // so that they share the same register and can avoid phi / upsilon copies.

            let mut coalescing_instructions = Vec::new();
            for (block_id, range) in shadow_liveness.shadow_var_live_ranges(shadow_var_id) {
                if let Some(incoming) = range.incoming_range {
                    coalescing_instructions
                        .push(ir.parts.blocks[block_id].instructions[incoming.end]);
                    if let Some(start) = incoming.start {
                        let ir::Instruction::Upsilon(_, source_inst_id) =
                            ir.parts.instructions[ir.parts.blocks[block_id].instructions[start]]
                        else {
                            unreachable!("all `start` fields in shadow ranges should be upsilon instructions");
                        };
                        coalescing_instructions.push(source_inst_id);
                    }
                }

                if let Some(outgoing) = range.outgoing_range {
                    if let Some(start) = outgoing.start {
                        let ir::Instruction::Upsilon(_, source_inst_id) =
                            ir.parts.instructions[ir.parts.blocks[block_id].instructions[start]]
                        else {
                            unreachable!("all `start` fields in shadow ranges should be upsilon instructions");
                        };
                        coalescing_instructions.push(source_inst_id);
                    }
                }
            }

            // For each instruction we would like to coalesce, check if it interferes with any
            // shadow variable that shares the assigned register (this will also check for conflicts
            // with the current shadow variable), and then also check if it interferes with any
            // instruction we have already decided to coalesce. If there are no conflicts, then
            // decide to coalesce this instruction.

            let mut coalesced_instructions = Vec::new();
            for &inst_id in &coalescing_instructions {
                let check_interference = || {
                    for (block_id, inst_range) in
                        instruction_liveness.instruction_live_ranges(inst_id)
                    {
                        let inst_range = BlockRange::from_instruction(inst_range);

                        for (other_shadow_var_id, &other_shadow_reg) in
                            assigned_shadow_registers.iter()
                        {
                            if other_shadow_reg != assigned_reg {
                                continue;
                            }

                            let Some(other_shadow_range) = shadow_liveness
                                .shadow_var_live_range_in_block(block_id, other_shadow_var_id)
                            else {
                                continue;
                            };

                            for other_shadow_range in
                                BlockRange::all_from_shadow_range(other_shadow_range)
                            {
                                // As a special hack, if we have a range that starts with an
                                // `Upsilon` instruction that we know is the *same* as the
                                // instruction we are trying to coalesce, we can skip interference
                                // checking. Assigning the coalescing variable to any shadow
                                // variable does not cause interference.
                                //
                                // NOTE:
                                //
                                // This analysis is weak in the sense that we only check this for
                                // ranges that actually *contain* the `Upsilon` in question, because
                                // this is the only place we can be sure without further analysis
                                // that there is only assignment from the correct instruction.
                                //
                                // We should be able to do this analysis more generally by marking
                                // which `Upsilon` instructions can possibly affect different shadow
                                // variable ranges.
                                //
                                // Frustratingly, there is very little information about this in the
                                // literature for register allocation that I can find, and there may
                                // be a more principled way to do this.
                                let can_skip = if let Some(start) = other_shadow_range.start {
                                    let ir::Instruction::Upsilon(_, source_inst_id) =
                                        ir.parts.instructions
                                            [ir.parts.blocks[block_id].instructions[start]]
                                    else {
                                        unreachable!("all `start` fields in shadow ranges should be upsilon instructions");
                                    };
                                    source_inst_id == inst_id
                                } else {
                                    false
                                };

                                if !can_skip && inst_range.interferes(other_shadow_range) {
                                    return true;
                                }
                            }
                        }

                        for &coalesced_inst_id in &coalesced_instructions {
                            if let Some(coalesced_inst_range) = instruction_liveness
                                .instruction_live_range_in_block(block_id, coalesced_inst_id)
                            {
                                if inst_range
                                    .interferes(BlockRange::from_instruction(coalesced_inst_range))
                                {
                                    return true;
                                }
                            }
                        }
                    }

                    false
                };

                if !check_interference() {
                    coalesced_instructions.push(inst_id);
                }
            }

            // Assign the coalescing register to any instructions we have determined we can
            // coalesce.

            for inst_id in coalesced_instructions {
                coalesced_instruction_registers.insert(inst_id, assigned_reg);
            }
        }

        // Now that we have a set of registers for our shadow variables and *some* instruction
        // registers, we need to assign the rest of the instruction variables.
        //
        // The graph coloring approach for shadow variables has high time complexity, but because of
        // the properties of SSA, we can assign registers to the rest of the instructions linearly.
        //
        // For each block, we determine the set of SSA instructions which are live on entry to the
        // block, and then scan the block and assign registers as we go. Because SSA instructions
        // have no "holes" due to assignment like shadow variables do, once we encounter the end
        // of their range, we know that no successive block can revive them, so we can add expired
        // instruction registers back to the pool of available registers as we go.
        //
        // NOTE:
        //
        // This is not optimal allocation because all shadow registers (and their coalesced
        // instruction registers) are allocated by graph coloring, and we assume here that the
        // entire set of these registers is simply *unavailable* to any other SSA instruction. If we
        // ever get close to running out of registers, we can be smarter here and do graph coloring
        // for *all* register allocation, but this may have a high runtime cost.

        let mut assigned_instruction_registers = SecondaryMap::<ir::InstId, RegIdx>::new();

        // The reverse of DFS post-order is a topological ordering, we want to iterate from the
        // top down.
        let mut block_order =
            dfs_post_order(ir.start_block, |id| ir.parts.blocks[id].exit.successors());
        block_order.reverse();

        for &block_id in &block_order {
            let block = &ir.parts.blocks[block_id];

            // The set of registers that are used at the start of this block.
            //
            // All shadow variable registers (and thus also coalesced instruction registers) are
            // always added to this set unconditionally.
            let mut live_in_registers = [0u8; 32];
            for &shadow_reg in assigned_shadow_registers.values() {
                live_in_registers.set_bit(shadow_reg as usize, true);
            }

            let mut inst_life_starts = HashMap::new();
            let mut inst_life_ends = HashMap::new();
            for (inst_id, range) in instruction_liveness.live_instructions_for_block(block_id) {
                if !coalesced_instruction_registers.contains(inst_id) {
                    if let Some(start) = range.start {
                        assert!(inst_life_starts.insert(start, inst_id).is_none());
                    } else {
                        live_in_registers.set_bit(
                            *assigned_instruction_registers.get(inst_id).unwrap() as usize,
                            true,
                        );
                    }

                    if let Some(end) = range.end {
                        let life_ends = inst_life_ends.entry(end).or_insert_with(|| {
                            ArrayVec::<ir::InstId, { ir::MAX_INSTRUCTION_SOURCES + 1 }>::new()
                        });
                        life_ends.push(inst_id);
                    }
                }
            }

            let mut available_registers = (0u8..=255u8)
                .rev()
                .flat_map(|index| {
                    if live_in_registers.get_bit(index as usize) {
                        None
                    } else {
                        Some(index)
                    }
                })
                .collect::<Vec<_>>();

            for inst_index in 0..=block.instructions.len() {
                for &inst_life_end in inst_life_ends.get(&inst_index).into_iter().flatten() {
                    available_registers.push(
                        assigned_instruction_registers
                            .get(inst_life_end)
                            .copied()
                            .unwrap(),
                    );
                }

                if let Some(&inst_life_start) = inst_life_starts.get(&inst_index) {
                    let reg = available_registers.pop()?;
                    assert!(assigned_instruction_registers
                        .insert(inst_life_start, reg)
                        .is_none());
                }
            }
        }

        for (inst_id, reg) in coalesced_instruction_registers.into_iter() {
            assert!(assigned_instruction_registers
                .insert(inst_id, reg)
                .is_none());
        }

        let used_registers = if let Some(max) = assigned_shadow_registers
            .values()
            .copied()
            .chain(assigned_instruction_registers.values().copied())
            .max()
        {
            max as usize + 1
        } else {
            0
        };

        Some(Self {
            instruction_registers: assigned_instruction_registers,
            shadow_registers: assigned_shadow_registers,
            used_registers,
        })
    }
}

#[derive(Debug, Copy, Clone)]
struct BlockRange {
    start: Option<usize>,
    end: Option<usize>,
}

impl BlockRange {
    fn from_shadow_incoming(incoming_range: ShadowIncomingRange) -> Self {
        Self {
            start: incoming_range.start,
            end: Some(incoming_range.end),
        }
    }

    fn from_shadow_outgoing(outgoing_range: ShadowOutgoingRange) -> Self {
        Self {
            start: outgoing_range.start,
            end: None,
        }
    }

    fn all_from_shadow_range(
        range: ShadowLivenessRange,
    ) -> impl Iterator<Item = BlockRange> + Clone {
        [
            range.incoming_range.map(BlockRange::from_shadow_incoming),
            range.outgoing_range.map(BlockRange::from_shadow_outgoing),
        ]
        .into_iter()
        .flatten()
    }

    fn from_instruction(range: InstructionLivenessRange) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }

    // We never consider ranges that touch to be interfering here because all instructions can only
    // write to one variable, so an instruction can only be a write for a variable on one or the
    // other side. For SSA instructions this is true because they can have only one output, for
    // `Upsilon` this is true because it only writes to the shadow variable.
    fn interferes(&self, other: Self) -> bool {
        if self
            .end
            .is_some_and(|se| other.start.is_some_and(|os| se <= os))
        {
            return false;
        }

        if self
            .start
            .is_some_and(|ss| other.end.is_some_and(|oe| oe <= ss))
        {
            return false;
        }

        true
    }
}
