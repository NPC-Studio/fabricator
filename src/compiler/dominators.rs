use std::mem;

use bit_vec::BitVec;

pub trait Node: Copy + Eq {
    /// Must be a unique, low-valued array index, algorithms may use memory proportional to the
    /// maximum length returned here.
    fn index(&self) -> usize;
}

pub trait Graph {
    type Node: Node;

    type Edges<'a>: IntoIterator<Item = Self::Node> + 'a
    where
        Self: 'a;

    /// Return all edges from this node to others in the directed graph.
    fn edges<'a>(&'a self, node: Self::Node) -> Self::Edges<'a>;
}

#[derive(Debug)]
pub struct DominatorTree<N> {
    postorder: Vec<N>,
    dominators: Vec<usize>,
}

impl<N: Node> DominatorTree<N> {
    pub fn compute<G>(graph: &G, start: N) -> Self
    where
        G: Graph<Node = N>,
    {
        let mut postorder = Vec::new();

        let mut visited = IndexSet::new();
        let mut stack = Vec::new();
        stack.push(start);

        while let Some(node) = stack.last().copied() {
            visited.insert(node.index());

            let mut leaf = true;
            for en in graph.edges(node) {
                if !visited.contains(en.index()) {
                    leaf = false;
                    stack.push(en);
                }
            }

            if leaf {
                postorder.push(node);
                stack.pop();
            }
        }

        let mut postorder_indexes = IndexMap::new();
        for i in 0..postorder.len() {
            postorder_indexes.insert(postorder[i].index(), i);
        }
        let postorder_index = |n: N| postorder_indexes.get(n.index()).copied().unwrap();

        let mut predecessors = Vec::new();
        for _ in 0..postorder.len() {
            predecessors.push(IndexSet::new());
        }

        for &node in &postorder {
            for en in graph.edges(node) {
                predecessors
                    .get_mut(postorder_index(en))
                    .unwrap()
                    .insert(postorder_index(node));
            }
        }

        let mut dominators = vec![None; postorder.len()];
        dominators[postorder_index(start)] = Some(postorder_index(start));

        // Dominance algorithm is sourced from:
        //
        // A Simple, Fast Dominance Algorithm, Cooper et al.
        // https://www.clear.rice.edu/comp512/Lectures/apers/TR06-33870-Dom.pdf

        let intersect = |doms: &[Option<usize>], mut f1: usize, mut f2: usize| -> usize {
            while f1 != f2 {
                while f1 < f2 {
                    f1 = doms[f1].unwrap();
                }
                while f2 < f1 {
                    f2 = doms[f2].unwrap();
                }
            }
            f1
        };

        let mut changed = true;
        while changed {
            changed = false;

            for node in postorder.iter().rev().copied() {
                if node == start {
                    continue;
                }

                let ni = postorder_index(node);
                let mut new_idom = None;
                for p in predecessors.get(ni).unwrap().iter() {
                    if dominators[p].is_some() {
                        new_idom = Some(match new_idom {
                            Some(idom) => intersect(&dominators, p, idom),
                            None => p,
                        });
                    }
                }

                if dominators[ni] != new_idom {
                    dominators[ni] = new_idom;
                    changed = true;
                }
            }
        }

        DominatorTree {
            postorder,
            dominators: dominators.into_iter().map(|i| i.unwrap()).collect(),
        }
    }
}

#[derive(Debug)]
struct IndexMap<V> {
    vec: Vec<Option<V>>,
}

impl<V> Default for IndexMap<V> {
    fn default() -> Self {
        Self { vec: Vec::new() }
    }
}

impl<V> IndexMap<V> {
    fn new() -> Self {
        Self::default()
    }

    fn get(&self, i: usize) -> Option<&V> {
        self.vec.get(i)?.as_ref()
    }

    fn get_mut(&mut self, i: usize) -> Option<&mut V> {
        self.vec.get_mut(i)?.as_mut()
    }

    fn contains(&self, i: usize) -> bool {
        self.get(i).is_some()
    }

    fn insert(&mut self, i: usize, v: V) -> Option<V> {
        if i >= self.vec.len() {
            self.vec.resize_with(i.checked_add(1).unwrap(), || None);
        }
        mem::replace(&mut self.vec[i], Some(v))
    }
}

#[derive(Default)]
struct IndexSet {
    vec: BitVec,
}

impl IndexSet {
    fn new() -> Self {
        Self::default()
    }

    fn contains(&mut self, i: usize) -> bool {
        self.vec.get(i).unwrap_or(false)
    }

    fn insert(&mut self, i: usize) -> bool {
        self.set(i, true)
    }

    fn remove(&mut self, i: usize) -> bool {
        self.set(i, false)
    }

    fn iter(&self) -> impl Iterator<Item = usize> + '_ {
        self.vec
            .iter()
            .enumerate()
            .filter_map(|(i, b)| if b { Some(i) } else { None })
    }

    fn set(&mut self, i: usize, val: bool) -> bool {
        if i >= self.vec.len() {
            self.vec
                .grow(i.checked_add(1).unwrap() - self.vec.len(), false);
        }

        let old = self.vec[i];
        self.vec.set(i, val);
        old
    }
}

#[cfg(test)]
mod tests {
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

    impl Graph for TestGraph {
        type Node = TestNode;
        type Edges<'a> = Vec<TestNode>;

        fn edges(&self, node: Self::Node) -> Vec<TestNode> {
            self.edges.get(node.index()).unwrap().clone()
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
    }

    #[test]
    fn test_dominator_tree() {
        let mut graph = TestGraph::default();

        //      [A]
        //       |
        //       |
        //      [B]
        //      / \
        //     /   \
        //    [C]  [D]
        //    / \
        //   /   \
        //  [E]  [F]

        let a = graph.create_node("A");
        let b = graph.create_node("B");
        let c = graph.create_node("C");
        let d = graph.create_node("D");
        let e = graph.create_node("E");
        let f = graph.create_node("F");

        graph.add_edge(a, b);
        graph.add_edge(b, c);
        graph.add_edge(b, d);
        graph.add_edge(c, e);
        graph.add_edge(c, f);

        dbg!(DominatorTree::compute(&graph, a));
    }
}
