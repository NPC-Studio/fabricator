use fabricator_util::index_containers::IndexSet;

use crate::graph::Node;

/// Search a directed graph in a depth-first manner and call the `pre` and `post` callbacks for
/// each node.
///
/// The `pre` callback is called in "pre-order", as soon as a node is encountered, the callback is
/// called. It is should return return the successors for the provided node.
///
/// The `post` callback is called in "post-order", the callback is called after all children
/// from that node have been visited.
///
/// The order that nodes are visited in will be the same as the order returned by the `pre`
/// callback. If the graph contains cycles, a node will only be visisted a single time.
pub fn depth_first_search<N, I>(start: N, mut pre: impl FnMut(N) -> I, mut post: impl FnMut(N))
where
    N: Node,
    I: IntoIterator<Item = N>,
{
    depth_first_search_with(&mut (), start, move |_, n| pre(n), move |_, n| post(n));
}

/// Search a directed graph in a depth-first manner and call the `pre` and `post` callbacks for
/// each node.
///
/// This is a version of `depth_first_search` that accepts a mutable state parameter and passes it
/// to each callback.
pub fn depth_first_search_with<S, N, I>(
    state: &mut S,
    start: N,
    mut pre: impl FnMut(&mut S, N) -> I,
    mut post: impl FnMut(&mut S, N),
) where
    N: Node,
    I: IntoIterator<Item = N>,
{
    let mut stack = Vec::new();
    let mut visited = IndexSet::new();
    visited.insert(start.index());
    stack.push((start, pre(state, start).into_iter()));

    while let Some((node, iter)) = stack.last_mut() {
        if let Some(next) = iter.next() {
            if visited.insert(next.index()) {
                stack.push((next, pre(state, next).into_iter()))
            }
        } else {
            post(state, *node);
            stack.pop();
        }
    }
}

/// Accumulate nodes in DFS pre-order
pub fn dfs_pre_order<N, I>(start: N, successors: impl Fn(N) -> I) -> Vec<N>
where
    N: Node,
    I: IntoIterator<Item = N>,
{
    let mut pre_order = Vec::new();
    depth_first_search(
        start,
        |n| {
            pre_order.push(n);
            successors(n)
        },
        |_| {},
    );
    pre_order
}

/// Accumulate nodes in DFS post-order
pub fn dfs_post_order<N, I>(start: N, successors: impl Fn(N) -> I) -> Vec<N>
where
    N: Node,
    I: IntoIterator<Item = N>,
{
    let mut post_order = Vec::new();
    depth_first_search(start, successors, |n| {
        post_order.push(n);
    });
    post_order
}

/// The reverse of `dfs_post_order`, which is a topological sorting of the given graph.
pub fn topological_order<N, I>(start: N, successors: impl Fn(N) -> I) -> Vec<N>
where
    N: Node,
    I: IntoIterator<Item = N>,
{
    let mut v = dfs_post_order(start, successors);
    v.reverse();
    v
}

#[cfg(test)]
mod tests {
    use fabricator_util::index_containers::IndexMap;

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
            |i| {
                pre.push(i);
                successors[i].iter().copied()
            },
            |i| post.push(i),
        );

        assert!(pre == &[1, 2, 4, 3]);
        assert!(post == &[4, 2, 3, 1]);
    }
}
