#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum MagicMode {
    ReadOnly,
    ReadWrite,
}

pub trait MagicDict<S> {
    /// If the given identifier identifies magic value, shoudl return its `MagicMode`.
    ///
    /// All bare identifiers (identifiers that are not a field suffix of some table) that are
    /// valid magic values will be treated as magic values if there is no variable declaration that
    /// shadows them. They will never be interpreted as an implicit field on `self`.
    ///
    /// Reading and writing to magic values compiles as separate kinds of VM instructions. If the
    /// magic variable is `MagicMode::ReadOnly`, then assigning to such a variable is a compiler
    /// error.
    fn magic_mode(&self, ident: &S) -> Option<MagicMode>;

    /// Should return a unique index per magic value.
    fn magic_index(&self, ident: &S) -> Option<usize>;
}
