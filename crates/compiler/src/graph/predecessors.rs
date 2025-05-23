use fabricator_util::index_containers::IndexMap;

use crate::graph::Node;

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
                predecessors.get_or_insert_with(s.index(), Vec::new).push(p);
            }
        }

        Self(predecessors)
    }

    /// Returns the accumulated predecessors of `node`.
    pub fn get(&self, node: N) -> impl ExactSizeIterator<Item = N> + '_ {
        if let Some(preds) = self.0.get(node.index()) {
            preds.iter().copied()
        } else {
            [].iter().copied()
        }
    }
}
