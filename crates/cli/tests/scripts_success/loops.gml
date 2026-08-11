{
    for (var i = 0; i < 10; i += 1) {
        if i == 5 {
            break;
        }
    }

    assert(i == 5);
}

{
    for (var i = 0; i < 10; i += 1) {
        if i == 5 {
            continue;
        }

        assert(i != 5);
    }
}

{
    var i = 0;
    while i < 10 {
        i += 1;
    }

    assert(i == 10);
}

{
    var i = 0;
    repeat 7 {
        i += 1;
    }
    assert(i == 7);

    var i = 0;
    repeat 9 {
        i += 1;
        if i == 5 {
            break;
        }
    }
    assert(i == 5);
}

// Ensure that breaks and continues in loops handle variable scoping correctly.
{
    while true {
        var i = 1;
        if black_box(true) {
            break;
        }
        black_box(i);
    }

    for (var go = true; go;) {
        var i = 1;

        go = false;
        if black_box(true) {
            continue;
        }
        go = true;

        black_box(i);
    }
}

{
    var j = 0;
    for (var i = 0; i < 10; ++i) {
        j += 1;
        i += 1;
    }
    assert(j == 5);
}

return true;
