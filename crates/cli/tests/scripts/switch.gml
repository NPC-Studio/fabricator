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
            // break;
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

return true;
