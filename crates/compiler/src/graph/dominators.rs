use fabricator_util::index_containers::{IndexMap, IndexSet};

use crate::graph::{
    Node,
    dfs::{depth_first_search, dfs_post_order},
    predecessors::Predecessors,
};

/// Calculate dominators and dominance frontiers for every node in a directed graph.
#[derive(Debug)]
pub struct Dominators<N> {
    // We reference all nodes internally by depth-first post-order index, and we keep dictionaries
    // to go from post-order index <-> node.
    post_order: Vec<N>,
    post_order_indexes: IndexMap<usize>,

    dominators: Vec<usize>,
    dominance_ranges: Vec<(usize, usize)>,
    dominance_frontiers: Vec<IndexSet>,
    dominance_tree: IndexMap<IndexSet>,
}

impl<N: Node> Dominators<N> {
    /// Calculate the dominator tree and dominance frontiers for every reachable node a directed
    /// graph, starting with node `start`.
    ///
    /// The `successors` function should return all nodes which are reachable from the given node
    /// parameter in the directed graph.
    ///
    /// The `start` node is always considered to dominate every other node.
    pub fn compute<I>(start: N, successors: impl Fn(N) -> I) -> Self
    where
        I: IntoIterator<Item = N>,
    {
        let post_order = dfs_post_order(start, &successors);

        let mut post_order_indexes = IndexMap::new();
        for (i, n) in post_order.iter().enumerate() {
            post_order_indexes.insert(n.index(), i);
        }

        let predecessors = Predecessors::compute(0..post_order.len(), |ni| {
            successors(post_order[ni])
                .into_iter()
                .map(|n| post_order_indexes[n.index()])
        });

        // Dominance algorithm is sourced from:
        //
        // A Simple, Fast Dominance Algorithm, Cooper et al. (2006)
        // https://www.clear.rice.edu/comp512/Lectures/Papers/TR06-33870-Dom.pdf
        let dominators = {
            let intersect =
                |dominators: &IndexMap<usize>, mut finger1: usize, mut finger2: usize| -> usize {
                    while finger1 != finger2 {
                        while finger1 < finger2 {
                            finger1 = dominators[finger1];
                        }
                        while finger2 < finger1 {
                            finger2 = dominators[finger2];
                        }
                    }
                    finger1
                };

            let mut dominators = IndexMap::new();
            dominators.insert(
                post_order_indexes[start.index()],
                post_order_indexes[start.index()],
            );

            // The start node should be at the end of the post-order.
            assert!(post_order.last().copied() == Some(start));

            let mut changed = true;
            while changed {
                changed = false;

                // We skip the start node, which will always be the final post-order node.
                for node in post_order.iter().rev().copied().skip(1) {
                    let ni = post_order_indexes[node.index()];
                    let mut new_idom = None;
                    for p in predecessors.get(ni) {
                        if dominators.contains(p) {
                            new_idom = Some(match new_idom {
                                Some(idom) => intersect(&dominators, p, idom),
                                None => p,
                            });
                        }
                    }

                    // We should be iterating in an order where every node will have at least one
                    // predecessor with a computed dominator.
                    let new_idom = new_idom
                        .expect("all nodes should have a predecessor with a computed dominator");
                    if dominators.get(ni).copied() != Some(new_idom) {
                        dominators.insert(ni, new_idom);
                        changed = true;
                    }
                }
            }

            // Every dominator should be computed at this point
            (0..post_order.len())
                .map(|i| dominators[i])
                .collect::<Vec<_>>()
        };

        // Find the pre and post order numbers in a DFS of the dominator tree. These represent
        // ranges that can make "does A dominate B?" queries O(1).
        let dominance_ranges = {
            let dominance_children =
                Predecessors::compute(0..post_order.len(), |ni| [dominators[ni]]);

            let mut pre_order_i = 0;
            let mut pre_order_numbers = vec![0; post_order.len()];

            let mut post_order_i = 0;
            let mut post_order_numbers = vec![0; post_order.len()];

            depth_first_search(
                post_order.len() - 1,
                |i| {
                    pre_order_numbers[i] = pre_order_i;
                    pre_order_i += 1;

                    dominance_children.get(i)
                },
                |i| {
                    post_order_numbers[i] = post_order_i;
                    post_order_i += 1;
                },
            );

            (0..post_order.len())
                .map(|i| (pre_order_numbers[i], post_order_numbers[i]))
                .collect::<Vec<_>>()
        };

        // Dominance frontier algorithm is also sourced from Cooper et al. (2006).
        // https://www.clear.rice.edu/comp512/Lectures/Papers/TR06-33870-Dom.pdf
        let dominance_frontiers = {
            let mut dominance_frontiers = vec![IndexSet::new(); post_order.len()];

            for i in 0..post_order.len() {
                let preds = predecessors.get(i);
                if preds.len() >= 2 {
                    for p in preds {
                        let mut runner = p;
                        while runner != dominators[i] {
                            dominance_frontiers[runner].insert(i);
                            runner = dominators[runner];
                        }
                    }
                }
            }

            dominance_frontiers
        };

        let mut dominance_tree = IndexMap::new();
        for index in 0..post_order.len() {
            if let Some(&idom) = dominators.get(index) {
                if idom != index {
                    dominance_tree
                        .get_or_insert_with(idom, IndexSet::new)
                        .insert(index);
                }
            }
        }

        Dominators {
            post_order,
            post_order_indexes,
            dominators,
            dominance_ranges,
            dominance_frontiers,
            dominance_tree,
        }
    }

    /// Return true if the given node was reachable when `Dominators` was constructed.
    pub fn was_reached(&self, n: N) -> bool {
        self.post_order_indexes.contains(n.index())
    }

    /// Return all reachable nodes in topological dominance order which always starts with the
    /// provided `start` node.
    ///
    /// The specific order is unspecified, but every node in this list will come before any other
    /// nodes that it dominates.
    pub fn topological_order(&self) -> impl Iterator<Item = N> + '_ {
        self.post_order.iter().copied().rev()
    }

    /// Return the immediate dominator ("idom") of the given node.
    ///
    /// Returns `None` if the given node `n` was not reachable when `Dominators` was constructed and
    /// thus has no dominance information.
    pub fn idom(&self, n: N) -> Option<N> {
        Some(self.post_order[self.dominators[self.post_order_indexes.get(n.index()).copied()?]])
    }

    /// Queries whether node `a` dominates node `b`.
    ///
    /// Ranges are calculated during `Dominators` construction that make this query O(1).
    ///
    /// Returns `None` if the either of the given nodes `a` or `b` was not reachable when
    /// `Dominators` was constructed and thus no dominance can be determined.
    pub fn dominates(&self, a: N, b: N) -> Option<bool> {
        let (a_start, a_end) =
            self.dominance_ranges[self.post_order_indexes.get(a.index()).copied()?];
        let (b_start, b_end) =
            self.dominance_ranges[self.post_order_indexes.get(b.index()).copied()?];
        Some(a_start <= b_start && b_end <= a_end)
    }

    /// Return the (precalculated) dominance frontier of the given node.
    ///
    /// Returns `None` if the given node `n` was not reachable when `Dominators` was constructed and
    /// thus has no dominance information.
    pub fn dominance_frontier(&self, n: N) -> Option<impl Iterator<Item = N> + '_> {
        Some(
            self.dominance_frontiers[self.post_order_indexes.get(n.index()).copied()?]
                .iter()
                .map(move |n| self.post_order[n]),
        )
    }

    /// Return the (precalculated) immediate children of the given node in the dominator tree.
    ///
    /// Every returned node will have the parameter `n` as its immediate dominator.
    ///
    /// Returns `None` if the given node `n` was not reachable when `Dominators` was constructed and
    /// thus has no dominance information.
    pub fn dominance_children(&self, n: N) -> Option<impl Iterator<Item = N> + '_> {
        Some(
            self.dominance_tree
                .get(self.post_order_indexes.get(n.index()).copied()?)
                .into_iter()
                .flat_map(|is| is.iter())
                .map(move |index| self.post_order[index]),
        )
    }
}

pub struct DominatorTree<N> {
    children: IndexMap<Vec<N>>,
}

impl<N: Node> DominatorTree<N> {
    /// Return an iterator over every node that has the given `node` as its immediate dominator.
    pub fn children(&self, node: N) -> impl Iterator<Item = N> + '_ {
        if let Some(succs) = self.children.get(node.index()) {
            succs.iter().copied()
        } else {
            [].iter().copied()
        }
    }
}

#[cfg(test)]
mod tests {
    use fabricator_util::index_containers::IndexMap;

    use super::*;

    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    struct TestNode(usize, &'static str);

    #[derive(Default)]
    struct TestGraph {
        next_node: usize,
        edges: IndexMap<Vec<TestNode>>,
    }

    impl Node for TestNode {
        fn index(&self) -> usize {
            self.0
        }
    }

    impl TestGraph {
        pub fn create_node(&mut self, name: &'static str) -> TestNode {
            let i = self.next_node;
            self.next_node += 1;
            let n = TestNode(i, name);

            self.edges.insert(n.index(), Vec::new());
            n
        }

        pub fn add_edge(&mut self, from: TestNode, to: TestNode) {
            self.edges.get_mut(from.index()).unwrap().push(to);
        }

        fn edges_from(&self, node: TestNode) -> impl Iterator<Item = TestNode> + '_ {
            self.edges[node.index()].iter().copied()
        }
    }

    #[test]
    fn test_dominator_tree() {
        let mut graph = TestGraph::default();

        //    [A] # Start
        //     |
        //     |
        //     V
        //    [B]------+
        //     |       |
        //     |       |
        //     V       V
        //    [C]<----[D]
        //    / \      |
        //   /   \     |
        //  V     V    |
        // [E]   [F]<--+
        //  ^
        //  |
        //  |
        // [G] # Not reachable

        // Make the node indexes are of a different range from the post-order indexes
        graph.create_node("T");
        graph.create_node("T");

        let a = graph.create_node("A");
        let b = graph.create_node("B");
        let c = graph.create_node("C");
        let d = graph.create_node("D");
        let e = graph.create_node("E");
        let f = graph.create_node("F");

        // `G` should not be reachable and should have no dominance information
        let g = graph.create_node("G");

        graph.add_edge(a, b);
        graph.add_edge(b, c);
        graph.add_edge(b, d);
        graph.add_edge(c, e);
        graph.add_edge(c, f);
        graph.add_edge(d, c);
        graph.add_edge(d, f);

        graph.add_edge(g, e);

        let dominators = Dominators::compute(a, |n| graph.edges_from(n));

        assert_eq!(dominators.idom(a).unwrap(), a);
        assert_eq!(dominators.idom(b).unwrap(), a);
        assert_eq!(dominators.idom(c).unwrap(), b);
        assert_eq!(dominators.idom(d).unwrap(), b);
        assert_eq!(dominators.idom(e).unwrap(), c);
        assert_eq!(dominators.idom(f).unwrap(), b);
        assert!(dominators.idom(g).is_none());

        let dominating_pairs = [
            (a, a),
            (a, b),
            (a, c),
            (a, d),
            (a, e),
            (a, f),
            (b, b),
            (b, c),
            (b, d),
            (b, e),
            (b, f),
            (c, c),
            (c, e),
            (d, d),
            (e, e),
            (f, f),
        ];

        for na in [a, b, c, d, e, f] {
            for nb in [a, b, c, d, e, f] {
                assert!(
                    dominators.dominates(na, nb).unwrap() == dominating_pairs.contains(&(na, nb))
                );
            }

            assert!(dominators.dominates(na, g).is_none());
        }

        let dominance_frontiers = [
            (a, vec![]),
            (b, vec![]),
            (c, vec![f]),
            (d, vec![c, f]),
            (e, vec![]),
            (f, vec![]),
        ];

        for (n, domf) in dominance_frontiers {
            let observed_domf = dominators
                .dominance_frontier(n)
                .unwrap()
                .collect::<Vec<_>>();
            for f in &observed_domf {
                assert!(domf.contains(f));
            }
            for f in &domf {
                assert!(observed_domf.contains(f));
            }
        }

        assert!(dominators.dominance_frontier(g).is_none());

        let dominance_children = [
            (a, vec![b]),
            (b, vec![f, c, d]),
            (c, vec![e]),
            (d, vec![]),
            (e, vec![]),
            (f, vec![]),
        ];

        for (n, children) in dominance_children {
            let observed_children = dominators
                .dominance_children(n)
                .unwrap()
                .collect::<Vec<_>>();
            for f in &observed_children {
                assert!(children.contains(f));
            }
            for f in &children {
                assert!(observed_children.contains(f));
            }
        }
    }
}
