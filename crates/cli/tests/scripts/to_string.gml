assert(string("foo") == "foo");

var obj1 = {
    toString: function() {
        return "foo"
    },
};

assert(string(obj1) == "foo");

var obj2 = {
    a: obj1,
};

assert(string(obj2) == "{ a: \"foo\" }")

obj2.b = obj1;
string(obj2);

obj2.c = obj2;
string(obj2);

var obj3 = {
    i: 4,
};

var obj4 = {
    a: obj3,
    b: obj3,
};

var obj5 = json_parse(json_stringify(obj4));
assert(obj5.a.i == 4 && obj5.b.i == 4);

try {
    obj5.c = obj5;
    json_stringify(obj5);
    assert(false);
} catch(e) {}

return true;
