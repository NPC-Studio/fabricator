use fabricator::{
    callback::Callback, closure::Closure, compiler::compile, object::Object, string::String,
    thread::Thread, value::Value,
};
use gc_arena::{arena, Gc};

#[test]
fn test_code_callback() {
    const CODE: &str = r#"
        var sum = 0;
        for (var i = 1; i <= 100000; i += 1) {
            sum += i;
        }
        assert(sum == 5000050000);
    "#;

    arena::rootless_mutate(|mc| {
        let this = Object::new(mc);
        let assert = Callback::from_fn(mc, |_, stack| {
            if !stack.get(0).to_bool() {
                Err("assert failed".into())
            } else {
                Ok(())
            }
        });
        this.set(mc, String::new(mc, "assert"), Value::Callback(assert));

        let prototype = compile(mc, CODE).unwrap();
        let closure = Closure::new(mc, Gc::new(mc, prototype), this);

        let mut thread = Thread::default();
        thread.exec(mc, closure).unwrap();
    });
}
