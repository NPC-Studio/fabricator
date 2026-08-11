function check_args() {
    for (var i = 0; i < argument_count; ++i) {
        assert(argument[i] == i);
    }
    return argument_count;
}

assert(check_args() == 0);
assert(check_args(0) == 1);
assert(check_args(0, 1) == 2);
assert(check_args(0, 1, 2) == 3);
assert(check_args(0, 1, 2, 3) == 4);

return true;
