var i = 1;
{
    var i = 2;
}

assert(i == 2);

for (var j = 0; j < 1; j += 1) {
    var i = 3;
}

assert(i == 3);

return true;
