function Test1() constructor {
    self.foo = 1;
}

var t1 = new Test1();
assert(t1.foo == 1);

function Test2() constructor {
    static foo = 1;
    bar = 2;
}

var t2 = new Test2();
assert(t2.foo == 1);
assert(t2.bar == 2);
 
function Test3(b) constructor {
    static foo = 1;
    bar = b;
}

var t3 = new Test3(3);
assert(t3.foo == 1);
assert(t3.bar == 3);

function Test4(b, c): Test3(b) constructor {
    static baz = 3;
    qux = c;
}

var t4 = new Test4(2, 4);
assert(t4.foo == 1);
assert(t4.bar == 2);
assert(t4.baz == 3);
assert(t4.qux == 4);

return true;
