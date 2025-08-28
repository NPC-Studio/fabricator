var err;
try {
    throw "error";
} catch(e) {
    err = e;
}
assert(e == "error");

var i = 0;
for (; i < 10; ++i) {
    if i == 5 {
        try {
            if black_box(false) {
                throw "not thrown";
            }

            break;
        } catch(e) {
            assert(false);
        }
    }
}
assert(i == 5);

var sum = 0;
for (var i = 0; i < 10; ++i) {
    if i >= 5 {
        try {
            if black_box(false) {
                throw "not thrown";
            }

            continue;
        } catch(e) {
            assert(false);
        }
    }

    sum += 1;
}
assert(sum == 5);

var val;
try {
    val = 4;
} catch(_) {}
assert(val == 4);

var Test = function() constructor {
    static FOO = 2;
    static BAR = 3;

    // Try-catch blocks are desugared as closures. Since constructors are disallowed in FML, this is
    // the only way to test the interaction of constructor statics and closures!
    try {
        self.result_add = FOO + BAR;
        try {
            self.result_mult = FOO * BAR;
        } catch(e) {}
    } catch(e) {}
};

var t = new Test();
assert(t.result_add == 5);
assert(t.result_mult == 6);

return true;