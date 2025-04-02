use crate::{compiler::graph::Node, util::index_containers::IndexSet};

/// Search a directed graph in a depth-first manner and call the `pre` and `post` callbacks for
/// each node.
///
/// If the graph is cyclic, a node will only be visisted a single time.
///
/// The `pre` callback is called in a pre-order, the `post` callback is called in a post-order.
pub fn depth_first_search<N, I>(
    start: N,
    successors: impl Fn(N) -> I,
    mut pre: impl FnMut(N),
    mut post: impl FnMut(N),
) where
    N: Node,
    I: IntoIterator<Item = N>,
{
    let mut stack = Vec::new();
    let mut visited = IndexSet::new();
    visited.insert(start.index());
    stack.push((start, successors(start).into_iter()));
    pre(start);

    while let Some((node, iter)) = stack.last_mut() {
        if let Some(next) = iter.next() {
            if visited.insert(next.index()) {
                pre(next);
                stack.push((next, successors(next).into_iter()))
            }
        } else {
            post(*node);
            stack.pop();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::util::index_containers::IndexMap;

    use super::*;

    #[test]
    fn test_dfs() {
        let mut successors = IndexMap::new();
        successors.insert(1, Vec::new());
        successors.insert(2, Vec::new());
        successors.insert(3, Vec::new());
        successors.insert(4, Vec::new());

        successors.get_mut(1).unwrap().push(2);
        successors.get_mut(1).unwrap().push(3);
        successors.get_mut(2).unwrap().push(4);
        successors.get_mut(3).unwrap().push(4);

        let mut pre = Vec::new();
        let mut post = Vec::new();

        depth_first_search(
            1,
            |i| successors[i].iter().copied(),
            |i| pre.push(i),
            |i| post.push(i),
        );

        assert!(pre == &[1, 2, 4, 3] || pre == &[1, 3, 4, 2]);
        assert!(post == &[4, 2, 3, 1] || post == &[4, 3, 2, 1]);
    }
}
