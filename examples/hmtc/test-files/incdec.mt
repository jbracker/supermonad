// Basic tests for pre and post increment and decrement operators.

let
    var x : Integer := 7;
    var y : Integer := x++;
    const z : Integer = --y
in begin
    putint(x);
    putint(y);
    putint(z);
    while (x-- > 0) do
        putint(x);
    while (++y < 10) do
        putint(y)
end
