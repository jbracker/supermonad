let
    var x : Boolean := false;
    proc p(x : Integer, out n : Integer) begin
        if x < 1000 then
            begin
                putint(x);
                p(f(x,c), n)
            end
        else
           n := x
    end;
    fun f(x : Integer, y : Integer) : Integer = x * x + y * y;
    const c : Integer = 2;
    var r : Integer
in begin
    p(1,r);
    putint(r)
end
