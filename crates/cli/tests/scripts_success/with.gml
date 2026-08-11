self.foo = 1;

var obj = {};

with obj {
    self.foo = 2;
}

assert(self.foo == 1);
assert(obj.foo == 2);

with obj {
    if black_box(true) {
        break;
    }
    self.foo = 3;
}

assert(obj.foo == 2);

return true;
