use fabricator_vm as vm;

pub trait StringInterner {
    type String: AsRef<str> + Clone;

    fn intern(&mut self, s: &str) -> Self::String;
}

pub struct VmInterner<'gc>(vm::Context<'gc>);

impl<'gc> VmInterner<'gc> {
    pub fn new(ctx: vm::Context<'gc>) -> Self {
        Self(ctx)
    }
}

impl<'gc> StringInterner for VmInterner<'gc> {
    type String = vm::String<'gc>;

    fn intern(&mut self, s: &str) -> vm::String<'gc> {
        self.0.intern(s)
    }
}
