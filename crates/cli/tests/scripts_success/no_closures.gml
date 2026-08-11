j = 0;

var create = function() {
    var j = 0;

    return function() {
        j += 1;
        return j;
    };
};

var a = create();
var b = create();
var c = create();

assert(a() == 1);
assert(b() == 2);
assert(c() == 3);

return true;
