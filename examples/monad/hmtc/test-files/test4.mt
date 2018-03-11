let
    var x : Boolean := false;
    const z : Integer = 1;
    proc p(in x : Integer, out n : Integer) begin
        let
            fun readT() : Boolean = t;
            var t : Boolean := true;
            fun foo(t : Integer) : Integer = t + (-d);
            const d : Integer = 6;
            const e : Integer = 7
        in
            if x < 1000 && readT() then
                let
                    proc putint2(in x : Integer) begin
                        let
                            const y : Integer = x;
                            const z : Integer = 3;
                            fun fie(x : Integer) : Integer = x * e * z;
                            proc q(x : Integer) putint(foo(x))
                        in begin
                            putint(x);
                            putint(y);
                            putint(fie(2));
                            let const z : Integer = 105 in
                                q(z)
                        end
                    end
                in 
                    begin
                        putint2(x);
                        let
                            var t : Integer := f(x,foo(c))
                        in
                            p(t, n)
                    end
            else
               n := x
    end;
    fun f(x : Integer, y : Integer) : Integer = x * x + y * y;
    const c : Integer = 4;
    var r : Integer
in begin
    p(z,r);
    putint(r)
end

