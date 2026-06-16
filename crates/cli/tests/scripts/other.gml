var this_1 = {};
var this_2 = {};

with this_1 {
    with this_2 {
        assert(self == this_2);
        assert(other == this_1);
    }
}

with this_1 {
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

    // var obj2 = {
    //     c_method: function(self_obj, other_obj) {
    //         assert(self == self_obj);
    //         assert(other == other_obj);
    //     },
    // };

    // obj2.c_method(obj2, this_1);
}

return true;
