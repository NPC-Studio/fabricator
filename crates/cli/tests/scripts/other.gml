var this_1 = {};
var this_2 = {};

with this_1 {
    with this_2 {
        assert(self == this_2);
        assert(other == this_1);
    }
}

with this_1 {
    var obj = {
        a_method: method(undefined, function(self_obj, other_obj) {
            assert(self == self_obj);
            assert(other == other_obj);
        }),
    };

    var b_method = method(obj, function(self_obj, other_obj) {
        assert(self == self_obj);
        assert(other == other_obj);
    });

    b_method(obj, this_1);
}

return true;
