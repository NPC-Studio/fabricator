use std::collections::HashMap;

use fabricator_util::{bit_containers::BitSlice as _, typed_id_map::SecondaryMap};
use fabricator_vm::{closure::HeapVarDescriptor, instructions::HeapIdx};

use crate::{analysis::variable_liveness::VariableLiveness, graph::dfs::topological_order, ir};

#[derive(Debug)]
pub struct HeapAllocation {
    pub heap_var_descriptors: Vec<HeapVarDescriptor>,
    pub heap_indexes: SecondaryMap<ir::Variable, HeapIdx>,
}

impl HeapAllocation {
    /// Verify all variables and determine heap descriptors for them.
    ///
    /// For all owned heap variables, will try to combine variables with independent lifetimes into
    /// the same index.
    pub fn allocate<S>(
        ir: &ir::Function<S>,
        variable_liveness: &VariableLiveness,
        parent_heap_indexes: &SecondaryMap<ir::Variable, HeapIdx>,
    ) -> Option<Self> {
        let mut heap_vars = Vec::new();
        let mut heap_indexes: SecondaryMap<ir::Variable, HeapIdx> = SecondaryMap::new();

        // First, assign all of the upvalues because those require no analysis.
        for var in ir.variables.ids() {
            if let Some(&upvalue) = ir.upvalues.get(&var) {
                let index: HeapIdx = heap_vars.len().try_into().ok()?;

                heap_vars.push(HeapVarDescriptor::UpValue(
                    *parent_heap_indexes
                        .get(upvalue)
                        .expect("upvalue not present in parent"),
                ));

                heap_indexes.insert(var, index);
            }
        }

        // Like SSA instructions, heap variables can be assigned in a single pass. Because we know
        // that a variable cannot become live again after its range ends, we can do a single pass
        // over the CFG in topological order and assign indexes as we go.

        let mut assigned_indexes = SecondaryMap::<ir::Variable, HeapIdx>::new();

        let block_order = topological_order(ir.start_block, |id| ir.blocks[id].exit.successors());

        for &block_id in &block_order {
            let block = &ir.blocks[block_id];

            // The set of heap indexes that are used at the start of this block.
            //
            // All upvalue indexes are always added to this set unconditionally.
            let mut live_in_indexes = [0u8; 32];
            for i in 0..heap_vars.len() {
                live_in_indexes.set_bit(i, true);
            }

            let mut var_life_starts = HashMap::new();
            let mut var_life_ends = HashMap::new();
            for (var, range) in variable_liveness.live_for_block(block_id) {
                if let Some(start) = range.start {
                    assert!(var_life_starts.insert(start, var).is_none());
                } else {
                    live_in_indexes.set_bit(assigned_indexes[var] as usize, true);
                }

                if let Some(end) = range.end {
                    var_life_ends.entry(end).or_insert_with(Vec::new).push(var);
                }
            }

            let mut available_indexes = (0u8..=255u8)
                .rev()
                .flat_map(|index| {
                    if live_in_indexes.get_bit(index as usize) {
                        None
                    } else {
                        Some(index)
                    }
                })
                .collect::<Vec<_>>();

            for inst_index in 0..=block.instructions.len() {
                if let Some(&var_life_start) = var_life_starts.get(&inst_index) {
                    let idx = available_indexes.pop()?;
                    assert!(assigned_indexes.insert(var_life_start, idx).is_none());
                }

                for &var_life_end in var_life_ends.get(&inst_index).into_iter().flatten() {
                    available_indexes.push(assigned_indexes[var_life_end]);
                }
            }
        }

        if !assigned_indexes.is_empty() {
            let mut max_idx = None;
            for (var, heap_idx) in assigned_indexes.into_iter() {
                assert!(heap_indexes.insert(var, heap_idx).is_none());
                max_idx = max_idx.max(Some(heap_idx));
                assert!(heap_idx as usize >= heap_vars.len());
            }

            let owned_start = heap_vars.len();
            for idx in heap_vars.len() as HeapIdx..=max_idx.unwrap() {
                heap_vars.push(HeapVarDescriptor::Owned(idx - owned_start as HeapIdx));
            }
        }

        Some(Self {
            heap_var_descriptors: heap_vars,
            heap_indexes,
        })
    }
}
