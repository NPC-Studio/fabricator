var i = 1;
{
    var i = 2;
}

assert(i == 2);

for (var j = 0; j < 1; j += 1) {
    var i = 3;
}

assert(i == 3);

var j = 4;

if false {
    var j = 5;
}

assert(j == 4);

return true;
