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
    repeat {
        i += 1;
        if i == 7 {
            break;
        }
    }

    assert(i == 7);
}

return true;
