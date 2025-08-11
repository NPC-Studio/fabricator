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

return true;