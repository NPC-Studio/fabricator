function test_to_string() {
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
        a: { b: 1 },
    };

    assert(string(obj3) == "{ a: { b: 1 } }");

    var arr1 = [1, 2, 3];
    assert(string(arr1) == "[1, 2, 3]");
    var arr2 = [1, [2, 3, 4], 5];
    assert(string(arr2) == "[1, [2, 3, 4], 5]");

    arr2[0] = arr1;
    arr2[1][0] = arr1;
    string(arr2);

    arr2[1][2] = arr2;
    string(arr2);
}
test_to_string();

function test_to_from_json() {
    var obj1 = {
        i: 4,
    };

    var obj2 = {
        a: obj1,
        b: obj1,
    };

    var obj3 = json_parse(json_stringify(obj2));
    assert(obj3.a.i == 4 && obj3.b.i == 4);

    // Trying to convert a recursive object to JSON should error.
    try {
        obj3.c = obj3;
        json_stringify(obj3);
        assert(false);
    } catch(e) {}
}
test_to_from_json();

function test_pretty_print_doesnt_lock() {
    var obj1 = {
        a: 1,
    }
    var obj2 = {
        toString: method(obj1, function() {
            self.a = 2;
            return "foo"
        }),
    };
    obj1.b = obj2;
    string(obj1);
}
test_pretty_print_doesnt_lock();

return true;
