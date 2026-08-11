var this_1 = {};
var this_2 = {};

with this_1 {
    with this_2 {
        assert(self == this_2);
        assert(other == this_1);
    }
}

with this_1 {
    // We purposefully do not match GMS2 in any of these cases. On method calls, GMS2 always
    // modifies the value of `self` without affecting the value of `other`. The entire design of
    // `other` is strange, but the thing that makes the most sense to me is "the previous value of
    // `self`", and that is the case for `with` blocks but not for function calls which change the
    // value of `self` in GMS2.

    var obj1 = {
        a_method: method(undefined, function(self_obj, other_obj) {
            assert(self == self_obj);
            assert(other == other_obj);
        }),
    };
    obj1.a_method(obj1, this_1);

    var b_method = method(obj1, function(self_obj, other_obj) {
        assert(self == self_obj);
        assert(other == other_obj);
    });
    b_method(obj1, this_1);

    // Ensure that callling a bound function as a field only pushes a new `self` value *once*.
    var obj2 = {
        c_method: function(self_obj, other_obj) {
            assert(self == self_obj);
            assert(other == other_obj);
        },
    };
    obj2.c_method(obj2, this_1);
    obj1.c_method = obj2.c_method;
    obj1.c_method(obj2, this_1);
}

return true;
