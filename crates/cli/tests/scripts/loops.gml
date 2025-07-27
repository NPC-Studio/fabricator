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

return true;
