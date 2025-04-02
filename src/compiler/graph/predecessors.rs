use std::ops;

use crate::{compiler::graph::Node, util::index_containers::IndexMap};

// Calculate the inverse of a directed graph.
#[derive(Debug)]
pub struct Predecessors<N>(IndexMap<Vec<N>>);

impl<N: Node> Predecessors<N> {
    /// For all the of the nodes in `nodes`, find their successors with `successors` and accumulate
    /// these in a mapping from successor to predecessor.
    ///
    /// Does not check for duplicate nodes in `nodes`. Predecessor list will have duplicates if
    /// `nodes` contains duplicates.
    pub fn compute<NI, SI>(nodes: NI, successors: impl Fn(N) -> SI) -> Self
    where
        NI: IntoIterator<Item = N>,
        SI: IntoIterator<Item = N>,
        N: Node,
    {
        let mut predecessors = IndexMap::new();

        for p in nodes {
            for s in successors(p) {
                let pred_list = predecessors
                    .get_mut_option(s.index())
                    .get_or_insert_with(Vec::new);
                pred_list.push(p);
            }
        }

        Self(predecessors)
    }

    /// Returns the accumulated predecessors of `node`.
    pub fn get(&self, node: N) -> &[N] {
        if let Some(successors) = self.0.get(node.index()) {
            successors.as_slice()
        } else {
            &[]
        }
    }
}

impl<N: Node> ops::Index<N> for Predecessors<N> {
    type Output = [N];

    fn index(&self, index: N) -> &Self::Output {
        self.get(index)
    }
}
