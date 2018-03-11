let
   var a : Boolean := true;
   const b : Boolean = false;
   var c : Integer := 3;
   overloaded proc put(n1 : Integer, n2 : Integer, n3 : Integer)
       let
           overloaded proc put(n1 : Integer, n2 : Integer)
               begin
                   put(n1 * n2)
               end
       in
           begin
               put(n1, n2);
               put(n3)
           end;
   overloaded proc put(n1 : Integer, n2 : Integer)
       begin
           put(n1);
           put(n2)
       end;
   overloaded proc put(n : Integer)
       putint(n)
in begin
    if b < a && c >= 2 then
        putint(1)
    else
        putint(2);
    put(10);
    put(11,12);
    put(13,14,15)
end
