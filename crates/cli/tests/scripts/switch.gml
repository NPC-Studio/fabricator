{
    var j = 2;

    var c = 0;
    switch j {
        case 1:
            c = 1;
            break;
        case 2:
            c = 2;
            break;
        default:
            c = 0;
            break;
    }

    assert(c == 2);
}

{
    var j = 1;

    var c = 0;
    switch j {
        case 1:
            c = 1;
            break;
        case 2:
            c = 2;
            break;
        default:
            c = 0;
            break;
    }

    assert(c == 1);
}

{
    var j = 3;

    var c = 0;
    switch j {
        case 1:
            c = 1;
            break;
        case 2:
            c = 2;
            break;
        default:
            c = -1;
            break;
    }

    assert(c == -1);
}

{
    var f = function(v) {
        switch v {
            case 1:
            case 2:
            case 3:
            case 4:
                return 7;
            default:
                return 13;
        }
    }

    assert(f(0) == 13);
    assert(f(1) == 7);
    assert(f(2) == 7);
    assert(f(3) == 7);
    assert(f(4) == 7);
    assert(f(5) == 13);
}

return true;
