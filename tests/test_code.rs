use fabricator::{closure::Closure, compiler::compile, thread::Thread, value::Value};
use gc_arena::{arena, Gc};

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
        let prototype = compile(mc, CODE).unwrap();
        let closure = Closure::new(mc, Gc::new(mc, prototype));

        let mut thread = Thread::default();
        assert_eq!(
            thread.exec(mc, closure).unwrap()[0],
            Value::Integer(5000050000)
        );
    });
}
