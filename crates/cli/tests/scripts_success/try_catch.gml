var err;
try {
    throw "error";
} catch(e) {
    err = e;
}
assert(err == "error");

function throw_err2() {
    throw "error 2";
}

var err2;
try {
    throw_err2();
} catch(e) {
    err2 = e;
}
assert(err2 == "error 2");

function test_err3() {
    var throw_err3 = function() {
        throw "error 3";
    }

    try {
        throw_err3();
    } catch(e) {
        return e;
    }
}
assert(test_err3(1, 2, 3) == "error 3");

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

function TestErr4() constructor {
    function execute() {
        var throw_err = function() {
            throw "error 4";
        };

        var err4;
        try {
            throw_err();
        } catch(e) {
            err4 = e;
        }

        assert(err4 == "error 4");
        return "execute";
    }
}

var t4 = new TestErr4();
assert(t4.execute(4, 5, 6) == "execute");

return true;