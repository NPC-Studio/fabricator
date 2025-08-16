globalvar foo;

var this = {
    foo: 1,
};

with this {
    foo = 2;
}

assert(this.foo == 1);
assert(foo == 2);

function test(foo) {
    assert(foo);
    foo = 3;
}

test(true);

assert(foo == 2);

return true;
