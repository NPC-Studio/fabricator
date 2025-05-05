pub mod dfs;
pub mod dominators;
pub mod predecessors;

pub trait Node: Copy + Eq {
    /// Must be a unique, low-valued array index that identifies the node. Graph algorithms may use
    /// memory proportional to the maximum length returned here.
    fn index(&self) -> usize;
}

impl Node for usize {
    fn index(&self) -> usize {
        *self
    }
}
