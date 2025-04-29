use crate::{
    closure::HeapVarDescriptor,
    compiler::{graph::dfs::topological_order, ir},
    instructions::HeapIdx,
    util::typed_id_map::SecondaryMap,
};

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

        // Since there can only be a single `OpenVariable` instruction and at most one
        // `CloseVariable` instruction, and all other accesses must occur *strictly* between those,
        // we can allocate all of the owned heap indexes in a single pass by marking indexes used on
        // open and freeing them on close while iterating in topological order.
        let block_order = topological_order(ir.start_block, |id| ir.blocks[id].exit.successors());

        let mut next_new_index = heap_vars.len();
        let mut closed_indexes = Vec::new();

        for &block_id in &block_order {
            let block = &ir.blocks[block_id];
            for &inst_id in &block.instructions {
                match ir.instructions[inst_id] {
                    ir::Instruction::OpenVariable(var) => {
                        let index = if let Some(closed) = closed_indexes.pop() {
                            closed
                        } else {
                            let new = next_new_index.try_into().ok()?;
                            heap_vars.push(HeapVarDescriptor::Owned(new));
                            next_new_index += 1;
                            new
                        };
                        assert!(heap_indexes.insert(var, index).is_none());
                    }
                    ir::Instruction::CloseVariable(var) => {
                        let index = heap_indexes[var];
                        closed_indexes.push(index);
                    }
                    _ => {}
                }
            }
        }

        Some(Self {
            heap_var_descriptors: heap_vars,
            heap_indexes,
        })
    }
}
