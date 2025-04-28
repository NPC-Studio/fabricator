var i = 0;

if true {
    i = 1;
}

assert(i == 1);

if false {
    i = 1;
} else {
    i = 2;
}

assert(i == 2);

