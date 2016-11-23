let
    // Matrix multiplication by constant matrix y
    proc matmult(x : Integer[2][4], out z : Integer[3][4])
        let
            var i : Integer;
            var j : Integer
        in begin
            i := 0;
            while i < 4 do begin
                j := 0;
                while j < 3 do begin
                    let
                        var s : Integer := 0;
                        var k : Integer := 0
                    in begin
                        while k < 2 do begin
                            s := s + x[i][k] * y[k][j];
                            k := k + 1
                        end;
                        z[i][j] := s
                    end;
                    j := j + 1
                end;
                i := i + 1
            end
        end;
    const y : Integer[3][2] = [[1,2,3],[4,5,6]]
in let
    var i : Integer;
    var j : Integer;
    var r : Integer[3][4];
    var s : Integer[3][4];
    var t : Integer[3][4]
in begin
    matmult([[1,2],[3,4],[5,6],[7,8]], r);
    // Some gratuitous copying to make sure block operations work
    i := 0;
    while i < 4 do begin
        s[i] := r[i];
        i := i + 1
    end;
    t := s;
    i := 0;
    // The right result is:
    // [[9, 12, 15], [19, 26, 33], [29, 40, 51], [39, 54, 69]]
    while i < 4 do begin
        j := 0;
        while j < 3 do begin
            putint(t[i][j]);
            j := j + 1
        end;
        i := i + 1
    end
end
