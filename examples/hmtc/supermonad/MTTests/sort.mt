// Reads 10 numbers, sorts them, then prints them.
let
    proc sort(var a : Integer[10])
        let
            proc swap(var x : Integer, var y : Integer)
                let
                    var t : Integer
                in begin
                    t := x;
                    x := y;
                    y := t
                end;
            var i : Integer;
            var j : Integer
        in begin
            i := 0;
            while i < 9 do begin
                j := i + 1;
                while j < 10 do begin
                    if a[i] > a[j] then swap(a[i], a[j]) else skip();
                    j := j + 1
                end;
                i := i + 1
            end            
        end;
    var x : Integer[10];
    var i : Integer
in begin
    i := 0;
    while i < 10 do begin
        getint(x[i]);
        i := i + 1
    end;
    sort(x);
    i := 0;
    while i < 10 do begin
        putint(x[i]);
        i := i + 1
    end
end
