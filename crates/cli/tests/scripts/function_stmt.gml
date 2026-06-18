// Assert that functions may be used before their top-level definition (exported as magics).

assert(my_fun_1() == 2);

function my_fun_1() {
	return my_fun_2();
}

function my_fun_2() {
	return 2;
}

// Assert that inner functions both create a named variable AND set a field in `self`.

function test_fn() {
    return "global";
}

function test() {
    function test_fn() {
        return "local";
    }

	try {
	    assert(test_fn() == "local");
	} catch(e) {
		assert(false);
	}

    assert(test_fn() == "local");
    assert(self.test_fn() == "local");
}

test();

assert(test_fn() == "global");
assert(self.test_fn() == "local");

return true;
