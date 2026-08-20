var obj1 = {
    a: 1,
};

var obj2 = {
    a: 2,
    black_box,
};

with obj1 {
    assert(self.a == 1);
    obj2.black_box();
    assert(self.a == 1);
}

return true;
