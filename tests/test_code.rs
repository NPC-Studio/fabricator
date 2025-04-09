use fabricator::{
    closure::Closure,
    compiler::{
        analysis::ssa_conversion::convert_to_ssa, codegen, compiler, parser,
        string_interner::StringInterner,
    },
    thread::Thread,
    value::{String, Value},
};
use gc_arena::{arena, Gc, Mutation};

#[test]
fn test_ir_codegen() {
    const CODE: &str = r#"
        var sum = 0;
        for (var i = 1; i <= 100000; i += 1) {
            sum += i;
        }
        return sum;
    "#;

    arena::rootless_mutate(|mc| {
        struct Interner<'a, 'gc>(&'a Mutation<'gc>);

        impl<'a, 'gc> StringInterner for Interner<'a, 'gc> {
            type String = String<'gc>;

            fn intern(&mut self, s: &str) -> String<'gc> {
                String(Gc::new(self.0, s.to_string()))
            }
        }

        let parsed = parser::parse(CODE, Interner(mc)).unwrap();
        let mut compiled = compiler::compile(&parsed).unwrap();
        convert_to_ssa(&mut compiled);
        let prototype = Gc::new(mc, codegen::generate(compiled).unwrap());
        let closure = Closure::new(mc, prototype);

        let mut thread = Thread::default();
        assert_eq!(
            thread.exec(mc, closure).unwrap()[0],
            Value::Integer(5000050000)
        );
    });
}
