var z = 0;
assert(is_nan(z / z));
assert(is_nan(0 * infinity));
assert(is_nan(infinity - infinity));
assert(is_nan(infinity / infinity));
assert(is_nan(power(-2, 0.5)));
assert(!is_nan(infinity));
assert(!is_nan(-infinity));
assert(!is_nan(0));
assert(!is_nan(-0));
assert(!is_nan(1.5));
assert(!is_nan("5"));
assert(!is_nan("5.5"));
assert(is_nan("NaN")); // note: GM actually disagrees with this, which is presumably a bug
assert(is_nan("hello"));
assert(is_nan(undefined));
assert(is_nan([]));
assert(is_nan({}));
assert(is_nan(function() {}));

return true;
