// This is a comment.  It continues to the end of the line.
let
    const d1: Integer = 2;
    var d2: Integer;
    var n: Integer
in
    begin
        d2 := 1;
        n := 1;
        while n < (25 + 1) do
            begin
	        putint(n);
	        d2 := d2 + d1;
                n := n + d2
            end
    end
